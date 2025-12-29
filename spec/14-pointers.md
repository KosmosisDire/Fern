# Pointers

Pointers store memory addresses, enabling direct memory manipulation and interop with native code.

## Pointer Types

Append `*` to any type to create a pointer type:

```fern
i32* intPtr           -- pointer to i32
f32* floatPtr         -- pointer to f32
MyType* typePtr       -- pointer to custom type
i32** ptrToPtr        -- pointer to pointer
```

## Address-Of Operator

Use `&` to get the address of a variable:

```fern
var x = 42
var ptr = &x          -- ptr is i32*, points to x
```

## Dereference Operator

Use `*` to access the value at a pointer's address:

```fern
var x = 42
var ptr = &x
var value = *ptr      -- value is 42

*ptr = 100            -- x is now 100
```

## Null Pointers

Pointers can be null (point to nothing):

```fern
i32* ptr = null

if ptr != null
{
    var value = *ptr
}
```

## Pointer Arithmetic

Pointers support arithmetic operations, automatically scaled by element size:

```fern
var arr = [10, 20, 30, 40, 50]
var ptr = &arr[0]

var first = *ptr          -- 10
var second = *(ptr + 1)   -- 20
var third = *(ptr + 2)    -- 30
```

## Use with Extern Functions

Pointers are essential for FFI with native code:

```fern
extern fn malloc(i64 size) -> ptr
extern fn free(ptr memory)
extern fn memcpy(ptr dest, ptr src, i64 size) -> ptr

var buffer = malloc(1024)
-- use buffer...
free(buffer)
```

## Void Pointers

Use `ptr` for untyped (void) pointers:

```fern
ptr rawPtr              -- equivalent to void* in C
```

Cast to typed pointers when needed:

```fern
var raw = malloc(4)
var intPtr = (i32*)raw
*intPtr = 42
```

## Future Pointer Syntax

Eventually, pointers will be a generic type and can be used like so:

```fern
int myInt = 0
ptr<i32> myIntPointer = ptr.Addr(myInt)
```

All these functions are simply showing that the shift from hardcoded syntax to pointers being integrated with the type / object model. The symbol-based syntax will simply be shorthand for this.