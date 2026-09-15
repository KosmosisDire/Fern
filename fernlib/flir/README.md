# FLIR

FLIR (Fern Low-level Intermediate Representation) is the IR produced by lowering FHIR. The goal of the FLIR is to be the single source of truth for codegen, including a future bytecode, WASM, and LLVM.

## Lowering

The lowering pass walks FHIR top-down and emits FLIR. Notable rewrites:

- `FhirConstructionExpr` lowers three ways. An intrinsic constructor is the allocation itself, so it becomes a value producing `FlirIntrinsic` stored into a temp. A non intrinsic ref constructor becomes a `FlirAlloc` plus a constructor `FlirCall` through the handle. A value constructor runs in place through the temp's address.
- `FhirCallExpr` becomes a `FlirCall`, and instance calls gain a `thisArg`.
- Instance methods and constructors get a synthesized `this` parameter at the front of their parameter list, and a `this` expression loads it.
- Index access `a[i]` becomes a getter call. When the getter returns `ref`, the call is the element's address and `a[i] = v` writes through it, otherwise `a[i] = v` becomes a setter call. There is no index node in FLIR.
- Assignment becomes a sequence that stores the value into a temp, writes the temp to the target, then yields the temp, so an assignment is an expression with a value.
- Compound assignment `x += y` takes the target's address once into a `Ptr<T>` temp, then reads, applies the operator, and stores back through it. A value indexer target instead stages its object and index and runs the getter and setter on the temps.
- An object builder `Foo { a = 1, b.c = 2 }` becomes a construct into a temp followed by a field store per entry, walking the path, yielding the temp.
- An array literal `[a, b, c]` becomes an intrinsic constructor sized to the element count stored into a temp, then one element store per element through the array's `ref` indexer, yielding the temp.
- A cast becomes a `FlirCall` when it runs a user defined cast method, or a primitive `FlirCast` when it is intrinsic.
- An operator becomes a `FlirIntrinsic` when intrinsic, or a `FlirCall` to the operator method otherwise.
- `while` becomes a `FlirLoop` containing an `if (!cond) break` prelude. There is no dedicated `while` node.
- `else if` chains nest as `FlirIf` inside the parent's `elseBlock`.

## Address Model

Locals are addressable memory. A scalar or handle typed expression evaluates to a value, a value type expression evaluates to an address. `FlirLocalAddr`, `FlirFieldAddr`, and `FlirElemAddr` produce addresses, `FlirLoad` and `FlirStore` move a scalar or handle at an address, and `FlirCopy` is the only node that moves bytes, a count of elements of its type with overlap allowed, where a value assignment is count one. Element indexes are `isize` and copy counts are `usize`. A `usize` index is cast to `isize` at the element address. Reading a variable loads for scalars and handles but stays an address for value types. Assignment stores for scalars and handles and copies for value types. A value type call result is written into a temp through the call's `resultDest`, so the call still yields an address. A call to a `ref` returning method is an address too, the place the callee returned. `Ptr<T>` indexing is the one intrinsic indexer and expands to `FlirElemAddr`, `p + n` expands to the same node as a value, and `p.CopyTo(dest, count)` expands to `FlirCopy`, while every other indexer, Array and String included, stays a call. A helper `is_memory_value` in `symbol.hpp` reads the flag the layout pass sets, deciding value type versus scalar or handle, and `FlirVerifier` checks these rules after lowering.

A frame pass (`frame.cpp`) then walks each method's parameters and locals, assigns a byte offset with C alignment, and sets `frameSize`, so `FlirLocalAddr` becomes the frame base plus a constant offset. The verifier rejects any slot left without an offset.

Static fields live in one region shared by every type, laid out by the layout pass into `StaticLayout` on the semantic context. `FlirStaticAddr` names the field and each backend turns it into its own storage, the region base plus the field offset on the VM. It never has a base expression, and the verifier rejects a `FlirFieldAddr` on a static field.

At the call boundary, value type `this` and value type parameters are by address (an incoming pointer, no frame slot), value type arguments are caller copied, and a value type return is copied into the method's hidden `sretParam`.

Intrinsic methods lower to `FlirIntrinsic`, which shares `FlirCall`'s shape (`method`, `thisArg`, `args`) but is a distinct type. `build_call` is the one choke point that routes intrinsics there, and the verifier rejects a `FlirCall` to an intrinsic method.

The bigger thing is just there are many less nodes, and this difference will grow as more features like async, for loops, and iterables are added, but FLIR should stay small implementing the minimal set of normalized constructs that are easy to codegen from.