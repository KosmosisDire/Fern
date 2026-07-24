# FLIR

FLIR (Fern Low-level Intermediate Representation) is the IR produced by lowering FHIR. The goal of the FLIR is to be the single source of truth for codegen, including a future bytecode, WASM, and LLVM.

## Lowering

The lowering pass walks FHIR top-down and emits FLIR. Notable rewrites:

- `FhirConstructionExpr` lowers three ways. An intrinsic constructor is the allocation itself, so it becomes a value producing `FlirIntrinsic` stored into a temp. A non intrinsic ref constructor becomes a `FlirAlloc` plus a constructor `FlirCall` through the handle. A value constructor runs in place through the temp's address.
- `FhirCallExpr` becomes a `FlirCall`, and instance calls gain a `thisArg`.
- Instance methods and constructors get a synthesized `this` parameter at the front of their parameter list, and a `this` expression loads it.
- Index access `a[i]` becomes a getter call and an index store `a[i] = v` becomes a setter call. There is no index node in FLIR.
- Assignment becomes a sequence that stores the value into a temp, writes the temp to the target, then yields the temp, so an assignment is an expression with a value.
- Compound assignment `x += y` becomes read target, apply the operator, store back, sequenced through temps. The object and index of an indexed or field target are evaluated once into temps so they do not run twice.
- An object builder `Foo { a = 1, b.c = 2 }` becomes a construct into a temp followed by a field store per entry, walking the path, yielding the temp.
- An array literal `[a, b, c]` becomes an intrinsic constructor sized to the element count stored into a temp, then one indexed setter call per element, yielding the temp.
- A cast becomes a `FlirCall` when it runs a user defined cast method, or a primitive `FlirCast` when it is intrinsic.
- An operator becomes a `FlirIntrinsic` when intrinsic, or a `FlirCall` to the operator method otherwise.
- `while` becomes a `FlirLoop` containing an `if (!cond) break` prelude. There is no dedicated `while` node.
- `else if` chains nest as `FlirIf` inside the parent's `elseBlock`.

## Address Model

Locals are addressable memory. A scalar or handle typed expression evaluates to a value, a value type expression evaluates to an address. `FlirLocalAddr`, `FlirFieldAddr`, and `FlirElemAddr` produce addresses, `FlirLoad` and `FlirStore` move a scalar or handle at an address, and `FlirCopy` is the only node that moves aggregate bytes. Reading a variable loads for scalars and handles but stays an address for value types. Assignment stores for scalars and handles and copies for value types. An aggregate call result is written into a temp through the call's `resultDest`, so the call still yields an address. Array and string indexing through the intrinsic accessors expands to `FlirElemAddr` plus a load or store, unchecked, while a user defined indexer stays a call. A helper `is_memory_class` in `builder.hpp` decides value type versus scalar or handle from the type tags, and `FlirVerifier` checks these rules after lowering.

A frame pass (`frame.cpp`) then walks each method's parameters and locals, assigns a byte offset with C alignment, and sets `frameSize`, so `FlirLocalAddr` becomes the frame base plus a constant offset. The verifier rejects any slot left without an offset.

At the call boundary, value type `this` and aggregate value parameters are by-address (an incoming pointer, no frame slot), aggregate arguments are caller copied, and an aggregate return is copied into the method's hidden `sretParam`.

Intrinsic methods lower to `FlirIntrinsic`, which shares `FlirCall`'s shape (`method`, `thisArg`, `args`) but is a distinct type. `build_call` is the one choke point that routes intrinsics there, and the verifier rejects a `FlirCall` to an intrinsic method.

The bigger thing is just there are many less nodes, and this difference will grow as more features like async, for loops, and iterables are added, but FLIR should stay small implementing the minimal set of normalized constructs that are easy to codegen from.