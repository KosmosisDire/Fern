# Control Flow

## If Statements

```fern
var score = 85

if score >= 90
{
    Print("Grade: A")
}
else if score >= 80
{
    Print("Grade: B")
}
else if score >= 70
{
    Print("Grade: C")
}
else
{
    Print("Grade: F")
}
```

## While Loops

```fern
var count = 0
while count < 5
{
    Print(count)
    count += 1
}
-- Prints: 0, 1, 2, 3, 4
```

## For Loops - Traditional

C-style for loops:

```fern
for (var i = 0; i < 10; i += 1)
{
    Print(i)
}
```

## For Loops - Range-based

### Repeat N times

```fern
for 5
{
    Print("Hello!")
}
-- Prints "Hello!" 5 times
```

### Ranges

```fern
-- Exclusive end (0 to 9)
for var i in 0...10
{
    Print(i)
}

-- Inclusive end (0 to 10)
for var i in 0..10
{
    Print(i)
}

-- With step
for var i in 0...20 by 2
{
    Print(i)  -- 0, 2, 4, 6, 8, 10, 12, 14, 16, 18
}
```

## Collection Loops

Iterate over arrays and other collections:

```fern
var fruits = ["apple", "banana", "cherry"]

-- Simple iteration
for var fruit in fruits
{
    Print(fruit)
}

-- With index
for var fruit in fruits at var i
{
    Print("{i}: {fruit}")
}
-- 0: apple
-- 1: banana
-- 2: cherry
```

See [Arrays](/07-arrays) for detailed iteration patterns.

## Match Expressions

Match expressions provide powerful pattern matching for control flow. They're similar to switch statements but more expressive and type-safe.

### Basic Match

```fern
fn GetGrade(i32 score): string
{
    return match (score)
    {
        100 => "Perfect!",
        90..99 => "A",
        80..89 => "B",
        70..79 => "C",
        60..69 => "D",
        _ => "F",
    };
}
```

### Match with Enums

Match is especially powerful with enums (see [Enums](/09-enums)):

```fern
fn ProcessDirection(Direction dir): void
{
    match (dir)
    {
        Direction.North => Console.Log("Going north"),
        Direction.East =>
        {
            Console.Log("Going east");
            UpdatePosition(1, 0);
            return;
        },
        Direction.South => Console.Log("Going south"),
        Direction.West => Console.Log("Going west"),
    };
}
```


### Match Range

Use range patterns to match ranges of values (see [Operators](/04-operators-and-expressions#range-operators) for range syntax):

```fern
fn CheckHealth(i32 health): void
{
    match (health)
    {
        in ..0 => Console.Log("Enemy is dead"),
        in 1..10 => Console.Log("Enemy is severely injured"),
        in 11..50 => Console.Log("Enemy is injured"),
        _ => Console.Log("Enemy is healthy"),
    };
}
```

### Wildcard Pattern

The `_` pattern matches anything and is typically used as the default case:

```fern
match (value)
{
    1 => Console.Log("One"),
    2 => Console.Log("Two"),
    _ => Console.Log("Other"),
};
```
