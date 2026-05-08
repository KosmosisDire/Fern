# fernfuzz

libFuzzer harness for the Fern compiler frontend. Coverage guided
fuzzing with AddressSanitizer.

## Build

Linux only, requires Clang 19+. Use the `linux-fuzz` CMake preset:

```
cmake --preset linux-fuzz
cmake --build --preset linux-fuzz-build
```

## Run

Continuous fuzzing:

```
./build/fuzz/bin/fernfuzz -dict=build/fuzz/bin/fern.dict corpus/
```

Replay a crashing input:

```
./build/fuzz/bin/fernfuzz path/to/crash-input
```

## Coverage

The harness splits each input on the byte sequence `0xFF 0x00 0xFF 0x00`
so a single fuzz buffer can encode multiple source files in one
compilation. Each chunk becomes a synthetic `fuzz_N.fn` source.

Per iteration the harness runs `Compilation::compile()` (lexer, parser,
binder, flow analysis) followed by every formatter on the resulting
state (AST, semantic context, FHIR, diagnostics). Codegen is not covered 
by the default preset since LLVM and libffi are disabled.
