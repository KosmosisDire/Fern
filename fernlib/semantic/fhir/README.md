# FHIR

FHIR (Fern High-level Intermediate Representation) is the fully-typed semantic IR produced by the binder. Every node carries a resolved type and a backpointer to its original syntax for source locations. Unlike the AST, FHIR normalizes constructs that have multiple syntactic forms into a single representation. Most desugaring (initializer lists, array literals, compound assignments) happens during binding rather than as separate tree-rewriting passes.

## Node Hierarchy

`FhirNode` is the base. It splits into `FhirExpr` (has a `TypeSymbol* type`) and `FhirStmt`. `FhirBlock` holds a list of statements. `FhirMethod` is the top-level unit: a `MethodSymbol*` plus an optional `FhirBlock* body`.

The tree uses a visitor pattern. `FhirVisitor` is pure virtual, `DefaultFhirVisitor` walks children by default, and the `FHIR_NODE` macro generates the `accept`, `is`, and `as` boilerplate.

## Call Representations

Two call shapes:

- **FhirCallExpr**: a method call. The `callee` is a `FhirMethodRefExpr` that bundles the resolved method with the optional `thisRef` (null for static, free, or operator calls). Static and instance calls share this single node.
- **FhirConstructionExpr**: constructor call that creates a new instance. Holds a `FhirTypeRef` for the user written type plus a nested `FhirCallExpr` carrying the resolved constructor and arguments. The inner call's `callee.thisRef` is null because the new instance is synthesized by codegen when emitting the construction. Constructors only appear inside this nested call.

## Symbol Reference Family

Names in source that bind to non value symbols like types, namespaces, methods become typed `*RefExpr` nodes in FHIR:

- `FhirTypeRef`. A type. Holds the `TypeSymbol*` and recursive type arg refs. Used in type slots like var, param, cast types, and as construction "callees".
- `FhirNamespaceRefExpr`. A namespace. Only valid as the left side of a member access while resolving qualified names.
- `FhirMethodGroupRefExpr`. A method name, before overload resolution. Carries the owning scope plus the name. `bind_call` resolves it against arg types and produces a `FhirMethodRefExpr` for the call. In any non call slot it is an error.
- `FhirMethodRefExpr`. A specific resolved overload. Lives as the callee of `FhirCallExpr`. Rarely escapes elsewhere.

The `*RefExpr` always wraps the user written reference even in error cases. `FhirErrorExpr` carries the ref as its `inner` so IDE features like hover and goto def keep working on broken code.

All four `*RefExpr` nodes inherit `FhirExpr`. Their `type` field is null because they are not values at runtime. The convention is that `bind_value_expr` rejects them when they appear in value slots. `FhirTypeRef` and `FhirMethodRefExpr` may eventually carry real value types if I add that later.

## Intrinsics

`FhirIntrinsicExpr` represents builtin operations (arithmetic, comparison, logical) on primitive types. These won't go through method dispatch since codegen can emit them directly as machine instructions. User-defined operators on non-builtin types produce `FhirCallExpr` instead.

## Error Recovery

`FhirErrorExpr` is a poison node produced when binding fails. It optionally wraps an `inner` expression (the partial result before the error). Code that encounters an error expression propagates it without reporting further errors, preventing cascades.

## Constant Evaluation

`FhirExpr::get_constant()` lazily computes and caches a `ConstantValue` (int, float, bool, or string). Literals, intrinsic operations, and casts can produce constants. The binder uses this for integer range checking (does this value fit in `u8`?) and the flow analyzer uses it for constant condition detection.

## Flow Analysis

`FlowAnalyzer` is a post-binding pass that checks whether all code paths return a value and warns about unreachable code. It understands constant conditions (`if true { return }` counts as a definite return, `if false { ... }` is skipped).
