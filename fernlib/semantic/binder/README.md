# Binder

Produces FHIR from the AST. Hierarchy of binders, one per scope. `lookup(name)` walks the chain from innermost out.

The binders:

- **Binder**: abstract base. Chain, accessors, binding logic.
- **RootBinder**: chain terminator. Resolves primitive aliases and global types.
- **NamespaceBinder**: namespace members.
- **TypeBinder**: type members, type params, generic substitutions.
- **MethodBinder**: method parameters, tempCounter.
- **BlockBinder**: locals, pendingStmts.

Subclasses are pure scope contributors. They override `lookup_in_single_binder` and optional virtual accessors. Binding logic lives on `Binder` and reaches scope state through those accessors.

## Pipeline

`BinderPipeline` drives the phases. `Compilation::compile()` constructs one and calls:

1. `declare_symbols` walks the AST and produces symbols.
2. `resolve_signatures` resolves field, parameter, return types.
3. `resolve_attributes` binds attribute argument expressions.
4. `bind_methods` produces FHIR for every method body.
5. `validate_signatures` runs duplicate and operator checks.

IDE callers use `SemanticContext::bind_single_method(method)` instead.

## Chain construction

Binder chains live on `SemanticContext`, built lazily.

```cpp
Binder& mBinder = semanticContext.method_binder(method);
mBinder.bind_block(callable->body);
```

`namespace_binder`, `type_binder`, `method_binder` each build their parent and cache. `BlockBinder` is not cached because it owns per call state.

## Accessors and lookup

Each binder overrides what it contributes. Defaults walk the chain.

- `containing_method`, `containing_type`, `containing_namespace`, `type_param_substitutions` expose scope.
- `pending_statements`, `temp_counter`, `current_block_scope` expose body state.
- `lookup_in_single_binder` contributes names.

`lookup(name)` walks `next` and returns the first match.

## Diagnostics

`Binder` forwards `error`, `warn`, `info`, `report` up the chain. `RootBinder` routes to `SemanticContext::diagnostics`, which points at `Compilation`.

## Type resolution

`resolve_type_expr` and `resolve_generic_type` live on `Binder`. They read `containing_type`, `containing_namespace`, `type_param_substitutions` via virtual accessors. Works for declaration resolution and body resolution.

## Pending statements

Initializer lists and array literals inject setup statements before their containing expression. They call `pending_statements()` which walks to the innermost `BlockBinder`. For example `var x = Foo { x = 1 }` becomes:

```
var __init_0 = Foo();
__init_0.x = 1;
var x = __init_0;
```

## Bidirectional type flow

`bind_expr` takes an optional `expected` type that flows downward. Matches pass through. Mismatches try an implicit cast. Integer constants get range checked. Drives suffixed literal disambiguation and array literal element inference.

## Implicit casting

`try_implicit_cast` checks for `Convertibility::Implicit` and wraps in a `FhirCastExpr`. Integer constants narrow silently if the value fits. `coerce_to_param` applies this at call sites.

## Compound comparisons

`>=`, `<=`, `!=` are synthesized. `>` plus `==` gives `>=`. `<` plus `==` gives `<=`. `==` gives `!=`.

## File naming

- `binder.hpp/cpp` is the `Binder` base class.
- `*_binder.hpp/cpp` is a concrete scope binder. One file per class.
- `binder_*.cpp` is a slice of `Binder` method definitions grouped by topic (exprs, calls, literals, stmts, attributes, initializers). Declared in `binder.hpp`.
- `binder_pipeline.hpp/cpp` is the phase driver `BinderPipeline`, not part of the chain.
