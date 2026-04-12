# FHIR

FHIR (Fern High-level Intermediate Representation) is the fully-typed semantic IR produced by the binder. Every node carries a resolved type and a backpointer to its original syntax for source locations. Unlike the AST, FHIR normalizes constructs that have multiple syntactic forms into a single representation. Most desugaring (initializer lists, array literals, compound assignments) happens during binding rather than as separate tree-rewriting passes.

## Node Hierarchy

`FhirNode` is the base. It splits into `FhirExpr` (has a `TypeSymbol* type`) and `FhirStmt`. `FhirBlock` holds a list of statements. `FhirMethod` is the top-level unit: a `MethodSymbol*` plus an optional `FhirBlock* body`.

The tree uses a visitor pattern. `FhirVisitor` is pure virtual, `DefaultFhirVisitor` walks children by default, and the `FHIR_NODE` macro generates the `accept`, `is`, and `as` boilerplate.

## Call Representations

There are three distinct call nodes, because codegen needs to handle them differently:

- **FhirCallExpr**: static call to a method. Used for static functions and user-defined operators.
- **FhirMethodCallExpr**: instance call with a receiver expression. The receiver is passed implicitly.
- **FhirObjectCreateExpr**: constructor call that creates a new instance. Carries the constructor method and arguments.

## Intrinsics

`FhirIntrinsicExpr` represents builtin operations (arithmetic, comparison, logical) on primitive types. These won't go through method dispatch since codegen can emit them directly as machine instructions. User-defined operators on non-builtin types produce `FhirCallExpr` instead.

## Error Recovery

`FhirErrorExpr` is a poison node produced when binding fails. It optionally wraps an `inner` expression (the partial result before the error). Code that encounters an error expression propagates it without reporting further errors, preventing cascades.

## Constant Evaluation

`FhirExpr::get_constant()` lazily computes and caches a `ConstantValue` (int, float, bool, or string). Literals, intrinsic operations, and casts can produce constants. The binder uses this for integer range checking (does this value fit in `u8`?) and the flow analyzer uses it for constant condition detection.

## Flow Analysis

`FlowAnalyzer` is a post-binding pass that checks whether all code paths return a value and warns about unreachable code. It understands constant conditions (`if true { return }` counts as a definite return, `if false { ... }` is skipped).
