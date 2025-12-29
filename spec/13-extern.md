# Extern

Call native C/C++ functions from Fern using the `extern` keyword.

## Syntax

```fern
extern fn FunctionName(ParamType param) -> ReturnType
```

The function body is not defined - it's implemented in native code.

## Basic Example

```fern
-- Declare C functions
extern fn sqrt(f64 x) -> f64
extern fn printf(string format, ...) -> i32

-- Use them
var result = sqrt(16.0)  -- 4.0
printf("Result: %f\n", result)
```

## Type Mapping

Fern types map to C types:

| Fern Type | C Type |
|-----------|--------|
| `i32` | `int32_t` |
| `i64` | `int64_t` |
| `u32` | `uint32_t` |
| `u64` | `uint64_t` |
| `f32` | `float` |
| `f64` | `double` |
| `bool` | `bool` |
| `string` | `const char*` |
| `ptr` | `void*` |

## C Variadic Functions

Use `...` for C-style varargs:

```fern
extern fn printf(string format, ...) -> i32

printf("Int: %d, Float: %f, String: %s\n", 42, 3.14, "hello")
```

**Note:** This is unsafe C-style varargs, not Fern's type-safe [`params`](/06-functions).

## Custom Libraries

Link your own C/C++ code:

```fern
extern fn InitializeEngine() -> bool
extern fn ProcessAudio(ptr buffer, i32 samples) -> i32
extern fn Shutdown()

if InitializeEngine()
{
    var buffer = malloc(1024)
    ProcessAudio(buffer, 256)
    Shutdown()
}
```

## Safety

External functions bypass Fern's type safety. Incorrect types or bad pointers can cause crashes. Wrap extern calls in safe Fern functions when possible.
