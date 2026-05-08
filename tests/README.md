# Tests

Atomic tests organized by purpose and feature.

## Structure

- `working/` - Programs that should compile and produce a value.
- `errors/` - Programs that should produce a compile error or warning.
- `bugs/` - Tests that track known unfixed behavior. Expected to fail until the underlying bug is resolved.

Tests are grouped by feature in subfolders. A test may exercise more than one
feature, in which case it lives under the folder for its primary subject. The test runner automatically discovers all files under tests/.

Every test file has a header comment in this format:

```
---
Description: a very short description of the test
Expected: <expected return value> OR <comma separated list of diagnostic codes>
---
```

`Expected` is one of:

- A return value, like `42` or `true`. The test passes only if the program compiles with zero diagnostics.
- A comma separated list of diagnostic codes, like `FN0012` or `FN0012, FN0034`. Every code starts with `FN`. The test passes only if the actual emitted diagnostics match the listed codes exactly (same multiset, order does not matter).

Every program has an entry point at `Program.Main` that returns a value
that can be checked against the expected return value.
