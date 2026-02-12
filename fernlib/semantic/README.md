# Semantic Analysis

This folder holds all the code related to semantic analysis. Basically everything that comes after parsing / AST and before code generation (FLIR).
This includes:
1. Symbol building
2. Binding
3. Type inference
4. Type checking (not implemented)
5. FHIR generation (not implemented)
6. FHIR transformations (not implemented)

There is one main top level file for this folder:

## Context

The context file is meant to store the big picture context of semantic analysis.
Right now it just holds the symbol table and the bindings for the AST. I might end up moving this stuff to the compilation instead, not sure yet