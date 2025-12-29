# Enums

Enums in Fern are inspired by Rust and provide a way to define types that can be one of several variants. Unlike traditional enums in other languages, Fern enums can hold data and include methods.

## Basic Enums

Simple enums define a set of named variants:

```fern
public enum Direction
{
    North,
    East,
    South,
    West
}
```

Usage:

```fern
Direction dir = Direction.North
```

## Enums with Data

Enum variants can hold associated data, similar to Rust's enum variants:

```fern
public enum Shape
{
    None,
    Square(i32 x, i32 y, i32 width, i32 height),
    Circle(i32 x, i32 y, i32 radius)
}
```

Creating enum instances with data:

```fern
Shape square = Shape.Square(0, 0, 10, 10)
Shape circle = Shape.Circle(5, 5, 15)
Shape none = Shape.None
```

## Enum Methods

Enums can have methods that operate on their variants:

```fern
public enum Direction
{
    North,
    East,
    South,
    West,

    public fn Combine(Direction other) -> string
    {
        return this.ToString() + other.ToString()
    }
}
```

Using enum methods:

```fern
Direction dir = Direction.North
Print(dir.Combine(Direction.East))  -- "NorthEast"
```

## Pattern Matching with Enums

Use `match` expressions to handle different enum variants (see [Control Flow](/05-control-flow) for more details on match):

```fern
public enum Direction
{
    North,
    East,
    South,
    West,

    public fn Opposite() -> Direction
    {
        return match (this)
        {
            Direction.North => Direction.South,
            Direction.East => Direction.West,
            Direction.South => Direction.North,
            Direction.West => Direction.East,
        }
    }
}
```
