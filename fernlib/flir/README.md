# FLIR

FLIR (Fern Low-level Intermediate Representation) is the IR produced by lowering FHIR. The goal of the FLIR is to be the single source of truth for codegen, including a future bytecode, WASM, and LLVM.

## Lowering

The lowering pass walks FHIR top-down and emits FLIR. Notable rewrites:

- `FhirConstructionExpr` becomes `FlirAlloc` plus a constructor `FlirCall`. The alloc is wired as the call's `thisArg`
- `FhirCallExpr` becomes `FlirCall` and a parameter 'thisArg' is added if the call is an instance method call.
- `while` becomes `FlirLoop` containing an `if (!cond) break` prelude. There is no dedicated `while` node
- `else if` chains nest as `FlirIf` inside the parent's `elseBlock`

The bigger thing is just there are many less nodes, and this difference will grow as more features like async, for loops, and iterables are added, but FLIR should stay small implementing the minimal set of normalized constructs that are easy to codegen from.