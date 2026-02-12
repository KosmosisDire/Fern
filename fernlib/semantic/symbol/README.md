# Symbols

Symbols represend named entities in the program. They also act as the high level type system for fern. All types are declared as a named type, and thus have a symbol. This includes primitives like f32 which is just an alias for Core.F32. This is basically the same way C#'s Roslyn compiler does it. The symbol table owns all symbol objects and provides methods to lookup symbols.