# Custom Types

Create your own data types with fields, methods, and custom behavior.

## Defining Types

```fern
type Player
{
    string name
    i32 health
    i32 score
}

var player = new Player()
player.name = "Alice"
player.health = 100
player.score = 0
```

## Constructors

```fern
type Player
{
    string name
    i32 health

    new(string playerName)
    {
        name = playerName
        health = 100
    }
}

var player = new Player("Bob")
Print(player.name)    -- "Bob"
Print(player.health)  -- 100
```

## Methods

```fern
type Player
{
    string name
    i32 health

    fn TakeDamage(i32 amount)
    {
        health -= amount
        Print("{name} took {amount} damage. Health: {health}")
    }

    fn Heal(i32 amount)
    {
        health += amount
    }
}

var player = new Player()
player.name = "Alice"
player.health = 100
player.TakeDamage(25)  -- "Alice took 25 damage. Health: 75"
player.Heal(10)
```

## Properties

Types can have properties with computed values or custom get/set behavior:

```fern
type Circle
{
    f32 radius

    -- Computed property (read-only)
    var Area => 3.14159 * radius * radius
}

var circle = new Circle()
circle.radius = 5.0
Print(circle.Area)  -- 78.53975
```

See the [Properties](/09-properties) page for more details on getters, setters, and advanced property patterns.

## Value Types vs Reference Types

### Value Types (Default)

Copied when assigned:

```fern
type Point
{
    f32 x, y
}

var p1 = new Point()
p1.x = 10

var p2 = p1        -- p2 is a copy
p2.x = 20

Print(p1.x)        -- 10 (unchanged)
Print(p2.x)        -- 20
```

### Reference Types

Share the same instance:

```fern
ref type Enemy
{
    i32 health
}

var e1 = new Enemy()
e1.health = 100

var e2 = e1        -- e2 references same object
e2.health = 50

Print(e1.health)   -- 50 (changed!)
Print(e2.health)   -- 50
```

Use `ref type` when you want shared state or large objects that would be expensive to copy.

## Inheritance

```fern
type Vehicle
{
    string brand
    i32 speed

    fn Start
    {
        Print("{brand} starting...")
    }
}

type Car : Vehicle
{
    i32 doors

    fn Honk
    {
        Print("Beep beep!")
    }
}

var myCar = new Car()
myCar.brand = "Toyota"
myCar.doors = 4
myCar.Start()  -- "Toyota starting..." (inherited)
myCar.Honk()   -- "Beep beep!" (Car-specific)
```

## Operator Overloading

Define how operators work with your types:

```fern
type Vector2
{
    f32 x, y

    new(f32 xVal, f32 yVal)
    {
        x = xVal
        y = yVal
    }

    -- Addition operator
    op +(Vector2 other) -> Vector2
    {
        return new Vector2(x + other.x, y + other.y)
    }

    -- Scalar multiplication
    op *(f32 scalar) -> Vector2
    {
        return new Vector2(x * scalar, y * scalar)
    }

    -- Equality
    op ==(Vector2 other) -> bool
    {
        return x == other.x && y == other.y
    }
}

var v1 = new Vector2(3.0, 4.0)
var v2 = new Vector2(1.0, 2.0)
var v3 = v1 + v2              -- Vector2(4.0, 6.0)
var v4 = v1 * 2.0             -- Vector2(6.0, 8.0)
var same = v1 == v2           -- false
```

## Type Conversions

Define how types convert to other types:

```fern
type Vector2
{
    f32 x, y
}

type Transform
{
    Vector2 position
    f32 rotation

    new(Vector2 pos, f32 rot)
    {
        position = pos
        rotation = rot
    }
}

type Vector2
{
    f32 x, y

    -- Implicit conversion to Transform
    implicit op cast -> Transform
    {
        return new Transform(this, 0.0)
    }
}

var vec = new Vector2()
vec.x = 10.0
vec.y = 20.0

Transform trans = vec  -- Automatic conversion
```

## Best Practices

**Use value types for small, immutable data**
```fern
type Point { f32 x, y }
type Color { u8 r, g, b, a }
```

**Use reference types for large or mutable state**
```fern
ref type GameState { /* lots of data */ }
ref type Player { /* mutable state */ }
```

**Keep constructors simple:**
```fern
new(string name, i32 startingHealth)
{
    this.name = name
    health = startingHealth
}
```
