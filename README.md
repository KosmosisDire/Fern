# Fern

Fern is a programming language which aims to be delightful to use. It is statically typed, with syntax inspired by Rust and C# and semantics inspired by C#. It aims to run anywhere from embedded Lua-like scripting to native cross-platform code. The compiler is written in c++ and compiles to an interpretable IR which runs in a VM. It can also be compiled to WASM, or optionally to native code via LLVM.

For now this is the GOAL of the language, but is not yet finished.


## Pipeline

The way I hope for the compiler to work is:

Source -> Lexer -> Parser -> AST -> Binder -> FHIR (Fern High-level IR) -> FLIR (Fern Low-level IR) > Backends (VM, WASM, LLVM)