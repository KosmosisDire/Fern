# Tests

Atomic tests organized by purpose and feature.

## Structure

- `working/` - Programs that should compile and produce a value.
- `errors/` - Programs that should produce a compile error or warning.
- `bugs/` - Tests that track known unfixed behavior. Expected to fail until the underlying bug is resolved.

Tests are grouped by feature in subfolders. A test may exercise more than one
feature, in which case it lives under the folder for its primary subject. The test runner wutomatically discovers all files under tests/.

Every test file has a header comment in this format:

```
---
Description: a very short description of the test
Expected: a primitive value or "compile error" / "compile warning"
---
```

Every program has an entry point at `Program.Main` that returns a value
that can be checked against `Expected`.
