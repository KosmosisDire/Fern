# Semantic Analysis

Everything related to semantic analysis. Basically everything that comes after parsing / AST and before low level code generation (FLIR).

## Context

`SemanticContext` holds the symbol table and the list of bound FHIR methods. It also owns the primitive type alias map (`i32` -> `Core.I32`, etc.) via `resolve_type_name`. I'm still not sure the context is really warranted as a separate type. Right now it's just holding the symbol table and FHIR output together, so I may reorganize this later, but for now it is fine.