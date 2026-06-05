# Symbols

Symbols are the named entities in a program: namespaces, types, fields, methods, parameters, and locals. They also double as the type system. Every type, including primitives like `f32`, is just a `NamedTypeSymbol` (`f32` is an alias for `Core.F32`). Each symbol knows its name, kind, parent, and the syntax it came from, and the parent chain is what makes qualified names like `Core.Array`.

## Type properties come from attributes

Whether a type is numeric, an integer, a float, or builtin is not hardcoded in the compiler. Instead I used attributes, like `Core.BuiltinType` or `Core.NumericInt` in the fern source itself. Generic instantiations read their attributes off the template, so `Array<i32>` is builtin because `Array<T>` is.

## Generics

A generic instantiation like `Box<i32>` needs its own fields, methods, and parameters with the type parameter replaced by the concrete type. These are substituted symbols: they keep a pointer back to the template member they came from and carry the substituted types. They are created and filled in lazily, only when the instantiation is first used, and substitution recurses through nested generics so a `Box<T>` field of type `Array<T>` becomes `Array<i32>` in `Box<i32>`.

## Overload resolution

Looking up a method, constructor, or operator filters candidates by kind and arity, grades each by how well its parameters convert, then picks the best. Fewest failed conversions wins, ties break toward the most exact matches, and a genuine tie is reported as ambiguous for the binder to handle. Conversions come from the type itself, which knows its user defined casts and whether another type converts to it exactly, implicitly, explicitly, or not at all.

## Ownership

The symbol table owns every symbol and hands raw pointers to the rest of the compiler, so there is a single owner. It also caches generic instantiations and drives the generic substitution.
