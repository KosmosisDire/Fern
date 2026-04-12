# Binder

The binder walks the AST and produces FHIR (Fern High-level Intermediate Representation). It creates symbols for declarations, resolves names, checks types, and emits fully-typed FHIR nodes. This is conceptually similar to the Roslyn binder. The single `Binder` class is split across multiple files because the logic is deeply interdependent and splitting into separate classes would just mean passing the same state around.

## Phases

The binder runs in five phases, called in order. Each phase depends on the previous one being complete.

1. **bind_ast** creates type, method, field, and parameter symbols from the AST. If a type has no explicit constructor, a synthetic one is generated with a parameter per field.
2. **resolve_all_types** resolves type annotations on fields, parameters, and return types to their `TypeSymbol*`. For generic templates, it sets up `TypeParamSymbol` substitutions so references like `T` resolve correctly.
3. **resolve_all_attributes** resolves `@Attribute` annotations to their types and constructors.
4. **bind_all_methods** produces FHIR for every method body. Non-generic and non-builtin methods are bound first, then each concrete generic instantiation is bound separately with monomorphized type substitutions.
5. **validate_all_types** runs cross-method checks: duplicate signatures, operator parameter rules, literal parameter rules.

## Files

Usually it is silly to include a list of files in a README, but since the binder is split across many files without clear class boundaries, this serves as a rough map of where to find different pieces.

| File | Responsibility |
|------|---------------|
| `binder.cpp` | Constructor and `bind_ast` entry point |
| `binder_define.cpp` | Phase 1: symbol definition for types, methods, fields |
| `binder_types.cpp` | Phase 2: type expression resolution, generic instantiation |
| `binder_attributes.cpp` | Phase 3: attribute resolution |
| `binder_methods.cpp` | Phase 4 entry: `bind_all_methods`, `bind_method`, synthetic constructors, field defaults |
| `binder_stmts.cpp` | Statements: blocks, var decls, if, while, return |
| `binder_exprs.cpp` | Expressions: identifiers, member access, binary/unary ops, assignment, indexing |
| `binder_calls.cpp` | Call binding: method calls, constructor calls, receiver resolution |
| `binder_literals.cpp` | Literals: string escapes, suffixed literals, array literals |
| `binder_initializers.cpp` | Initializer list binding and lowering |
| `binder_scope.cpp` | Scope stack and name resolution |
| `binder_validate.cpp` | Phase 5: duplicate method detection, operator/literal validation |

## Bidirectional Type Flow

`bind_expr` takes an optional `expected` type that flows downward. When present, the result is checked against it: matching types pass through, mismatches attempt an implicit cast, and integer constants are range-checked to see if they fit the target type. This downward flow also drives suffixed literal disambiguation (picking which type's literal method to call) and array literal element type inference.

## Implicit Casting and Coercion

`try_implicit_cast` checks if a `Convertibility::Implicit` relationship exists between two types and wraps the expression in a `FhirCastExpr`. Integer constants with known values get special treatment: they can silently narrow to smaller types if the value fits, with a clear error if it doesn't. `coerce_to_param` applies this at call sites to match arguments to parameter types.

## Pending Statements

Initializer lists and array literals both need to emit setup statements before the expression they appear in. They use `pendingStmts` (a pointer to the current block's statement list) to inject a temporary local and assignment statements ahead of the expression, then the expression result is just a reference to that temp. For example `var x = Foo { x = 1 }` becomes roughly
```
var __init_0 = Foo();
__init_0.x = 1;
var x = __init_0;
```

## Compound Comparison Synthesis

The binder synthesizes `>=`, `<=`, and `!=` from simpler operators rather than requiring types to define them all. If a type defines `>` and `==`, then `>=` works automatically. Same for `<` + `==` giving `<=`, and `==` giving `!=`.
