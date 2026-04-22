# Semantic Analysis

Everything after parsing, before code generation.

## Context

`SemanticContext` holds:

- The symbol table.
- The list of bound FHIR methods.
- The primitive type alias map (`i32` to `Core.I32` etc) via `resolve_type_name`.
- Cached binder chains and the `boundMethods` cache. Built lazily. Lifetime matches the context.
- The `RootBinder` chain terminator.
- A reference to the outer `Diagnostics` so chain diagnostics land on `Compilation`.