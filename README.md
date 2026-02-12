# Fern

Fern is a programming language which aims to be delightful to use. It is statically typed, with syntax inspired by Rust and C# and semantics inspired by C#. It aims to run anywhere from embedded Lua-like scripting to native cross-platform code. The compiler is written in c++ and compiles to an interpretable IR which runs in a VM. It can also be compiled to WASM, or optionally to native code via LLVM.

For now this is the GOAL of the language, but is not yet finished.


## Pipeline

The way I hope for the compiler to work is:

Source -> Lexer -> Parser -> AST -> Binder -> AST Side tables -> FHIR (Fern High-level IR) -> FLIR (Fern Low-level IR) > Backends (VM, WASM, LLVM)

## AST

The AST is pretty standard, however we have side tables which bind each expression in the AST to a type and a symbol if one exists. This information is then used to create the FHIR.

## FHIR

The high level IR is similar to the bound tree in the roslyn compiler. It is a tree which is meant to ONLY represent semantics and not syntax. So any construct that has multiple syntax representations will be normalized to a single semantic represenation here.


## FLIR

The Fern Low-level IR is the second IR which is much more similar to like an LLVM IR. This IR is generated from the transformed FHIR. The main thing this IR does is to expose the actual memory model underlying the semantics and make it explicit. For example, reference types are converted to pointers. Store and load instructions are explicit.