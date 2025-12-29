# Functions

## Defining Functions

Functions use PascalCase naming:

```fern
fn Greet
{
    Print("Hello, World!")
}

Greet()  -- Call the function
```

## Arrow Function
For short functions, use `=>` to define the body:

```fern
fn Add(i32 a, i32 b) => a + b

var result = Add(5, 3)  -- result = 8
```

## Parameters

```fern
fn Greet(string name)
{
    Print("Hello, {name}!")
}

Greet("Alice")  -- "Hello, Alice!"

-- Multiple parameters
fn Add(i32 a, i32 b)
{
    Print("Sum: {a + b}")
}

Add(5, 3)  -- "Sum: 8"
```

### Variable Arguments

Use `params` before an array parameter to accept any number of arguments. Must be the last parameter:

```fern
fn PrintAll(params string[] messages)
{
    for var msg in messages
    {
        Print(msg)
    }
}

PrintAll("Hello")                    -- One argument
PrintAll("Hello", "World")           -- Two arguments
PrintAll("A", "B", "C", "D")         -- Four arguments

-- Mix regular parameters with params
fn FormatLog(string level, params string[] messages)
{
    Print("[{level}]")
    for var msg in messages
    {
        Print("  {msg}")
    }
}

FormatLog("INFO", "Server started", "Port: 8080")
FormatLog("ERROR", "Connection failed", "Retrying...", "Timeout")
```

The `params` array receives all remaining arguments:

```fern
fn Sum(params i32[] numbers) -> i32
{
    var total = 0
    for var num in numbers
    {
        total += num
    }
    return total
}

var result1 = Sum(1, 2, 3)           -- 6
var result2 = Sum(10, 20, 30, 40)    -- 100
```

## Return Types

```fern
fn Add(i32 a, i32 b) -> i32
{
    return a + b
}

var result = Add(10, 20)  -- result = 30

-- Return multiple values
fn GetCoordinates -> (f32, f32)
{
    return (10.5, 20.3)
}

var (x, y) = GetCoordinates()
```

## Function Overloading

Same function name, different parameter types:

```fern
fn Process(i32 value)
{
    Print("Processing integer: {value}")
}

fn Process(string value)
{
    Print("Processing string: {value}")
}

fn Process(f32 value)
{
    Print("Processing float: {value}")
}

Process(42)       -- "Processing integer: 42"
Process("hello")  -- "Processing string: hello"
Process(3.14)     -- "Processing float: 3.14"
```

## Generic Functions

Write functions that work with multiple types:

```fern
fn Swap<T>(T a, T b) -> (T, T)
{
    return (b, a)
}

var (x, y) = Swap(1, 2)           -- Works with i32
var (a, b) = Swap("hi", "bye")    -- Works with string
```

See [Generic Programming](/10-generic-programming) for detailed coverage of generics.
