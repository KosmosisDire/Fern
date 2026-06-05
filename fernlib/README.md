# fernlib

The Fern compiler library. This is where all the actual compiler work happens.

The pipeline goes:

```
source -> lexer -> parser -> AST -> binder -> FHIR -> FLIR
```