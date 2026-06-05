# FLIR

FLIR (Fern Low-level Intermediate Representation) is the IR produced by lowering FHIR. The goal of the FLIR is to be the single source of truth for codegen, including a future bytecode, WASM, and LLVM.

## Lowering

The lowering pass walks FHIR top-down and emits FLIR. Notable rewrites:

- `FhirConstructionExpr` becomes a `FlirAlloc` plus a constructor `FlirCall`, with the alloc wired in as the call's `thisArg`.
- `FhirCallExpr` becomes a `FlirCall`, and instance calls gain a `thisArg`.
- Instance methods and constructors get a synthesized `this` parameter at the front of their parameter list, and a `this` expression loads it.
- Index access `a[i]` becomes a getter call and an index store `a[i] = v` becomes a setter call. There is no index node in FLIR.
- Assignment becomes a sequence that stores the value into a temp, writes the temp to the target, then yields the temp, so an assignment is an expression with a value.
- Compound assignment `x += y` becomes read target, apply the operator, store back, sequenced through temps. The object and index of an indexed or field target are evaluated once into temps so they do not run twice.
- An initializer `Foo { a: 1, b.c: 2 }` becomes a construct into a temp followed by a field store per entry, walking the path, yielding the temp.
- An array literal `[a, b, c]` becomes an alloc plus a constructor call sized to the element count, then one indexed setter call per element, yielding the temp.
- A cast becomes a `FlirCall` when it runs a user defined cast method, or a primitive `FlirCast` when it is intrinsic.
- An operator becomes a `FlirIntrinsic` when intrinsic, or a `FlirCall` to the operator method otherwise.
- `while` becomes a `FlirLoop` containing an `if (!cond) break` prelude. There is no dedicated `while` node.
- `else if` chains nest as `FlirIf` inside the parent's `elseBlock`.

The bigger thing is just there are many less nodes, and this difference will grow as more features like async, for loops, and iterables are added, but FLIR should stay small implementing the minimal set of normalized constructs that are easy to codegen from.