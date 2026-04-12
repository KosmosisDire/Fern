# Symbols

Symbols represent named entities in the program: namespaces, types, fields, methods, parameters, and locals. They also serve as the type system for FHIR: all types including primitives like `f32` are a `NamedTypeSymbol` (aliased from `Core.F32`). This is the same way Roslyn does it.

## Symbol Hierarchy

`Symbol` is the base. It carries a name, kind, parent pointer, and optional syntax backpointer. The parent chain gives you qualified names (`Core.Array`) and enclosing namespace lookup.

- **NamespaceSymbol**: contains child namespaces and types.
- **NamedTypeSymbol**: a declared type with fields, methods, nested types, generic parameters, and resolved attributes.
- **TypeParamSymbol**: a generic type parameter with an index and owning type.
- **FieldSymbol**: a typed field on a type, with an index for layout ordering.
- **MethodSymbol**: covers functions, constructors, operators, literals, and casts (distinguished by `CallableKind`). Holds parameters and a protected return type.
- **ParameterSymbol** / **LocalSymbol**: typed value symbols scoped to a method.

## Generic Instantiation

Generic instantiations need their own copies of fields, methods, and parameters with substituted types. If `Box<T>` has a field of type `T`, then `Box<i32>` needs that field to have type `i32`. But these copies share the same syntax, name, and structure as the template's members. `SubstitutedFieldSymbol`, `SubstitutedMethodSymbol`, and `SubstitutedParameterSymbol` handle this: they extend the base symbol types with an `original*` pointer back to the template member, and carry the concrete substituted types. `SubstitutedMethodSymbol` also resolves its return type lazily since it may require recursive substitution.

## Overload Resolution

All overloaded lookups (`find_method`, `find_constructor`, `find_binary_operator`, etc) live on `NamedTypeSymbol` and follow the same pattern: filter candidates by kind and arity, score each one with `Overload::grade`, then pick a winner with `Overload::resolve`.

Grading scores each parameter as exact, implicit, or fail using `get_convertibility`. Resolution picks the candidate with the fewest failures, breaking ties by most exact matches. If two candidates tie, the result is marked ambiguous to be handled by the binder.

## Convertibility

`NamedTypeSymbol::get_convertibility` determines how one type converts to another. It checks both the source and target types for user-defined casts (`CallableKind::Cast` with `Modifier::Implicit` or `Modifier::Explicit`). The four levels are Exact (same type), Implicit (automatic), Explicit (requires cast), and None. 

## Attribute-Driven Type Properties

Rather than hardcoding which types are numeric or builtin, `NamedTypeSymbol` checks its resolved attributes: `is_builtin()` looks for `Core.BuiltinType`, `is_integer()` for `Core.NumericInt`, `is_float()` for `Core.NumericFloat`, `allows_custom_literals()` for `Core.AllowCustomLiterals`. Generic instantiations look their attributes up on the template, so `Array<i32>` is builtin because `Array<T>` is.

---

# Symbol Table

The `SymbolTable` owns all symbol objects and provides lookup and generic instantiation. It creates a `<global>` root namespace on construction.

## Ownership

The table holds a `vector<unique_ptr<Symbol>>`. The `own()` method takes a `unique_ptr`, stores it, and returns the raw pointer. Everything else in the compiler holds raw `Symbol*` pointers so there is only one owner.

## Generic Instantiation

`get_or_create_instantiation` creates or returns a cached instantiation of a generic template with concrete type arguments. The instantiation starts empty, then members are populated lazily by `populate_instantiation_members` when first accessed.

`substitute_type` recursively replaces `TypeParamSymbol` references with their concrete type arguments, and handles nested instantiations (e.g. if `Box<T>` has a field of type `Array<T>`, instantiating `Box<i32>` substitutes the field to have type of `Array<i32>`). This is what produces the substituted symbols described above.
