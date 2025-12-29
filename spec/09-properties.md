# Properties

Properties provide computed values or custom get/set behavior for types. They can only exist on custom types.

## Read-Only Properties

Use `=>` for simple computed properties:

```fern
type Temperature
{
    f32 celsius

    -- Computed property for Fahrenheit (read only)
    var Fahrenheit => (celsius * 9.0/5.0) + 32.0
}

var temp = new Temperature()
temp.celsius = 25.0
Print(temp.Fahrenheit)  -- 77.0
```

```fern
type Rectangle
{
    f32 width
    f32 height

    var Area => width * height
    var Perimeter => 2.0 * (width + height)
}

var rect = new Rectangle()
rect.width = 10.0
rect.height = 5.0
Print(rect.Area)       -- 50.0
Print(rect.Perimeter)  -- 30.0
```

## Custom Getters and Setters

Define custom behavior for reading and writing:

```fern
type Square
{
    f32 sideLength

    -- Property with custom getter and setter
    var Area
    {
        get => sideLength * sideLength
        set
        {
            sideLength = sqrt(value) -- value is the value being set
        }
    }
}

var sq = new Square()
sq.sideLength = 4.0
Print(sq.Area)  -- 16.0

sq.Area = 25.0
Print(sq.sideLength)  -- 5.0
```

## The `field` Keyword

Access the backing field in getter/setter blocks:

```fern
type Counter
{
    var Count = 0
    {
        get
        {
            field += 1 -- field is the actual stored value
            return field
        }
    }
}

var counter = new Counter()
Print(counter.Count)  -- 1
Print(counter.Count)  -- 2
Print(counter.Count)  -- 3
```

```fern
type ValidatedAge
{
    var Age = 0
    {
        set
        {
            if value < 0
                field = 0
            else if value > 150
                field = 150
            else
                field = value
        }
    }
}

var person = new ValidatedAge()
person.Age = 200  -- Clamped to 150
Print(person.Age)  -- 150
```

## Lazy Initialization

Compute values only when first accessed:

```fern
type ExpensiveCalculation
{
    var Result = 0.0
    {
        get
        {
            if field == 0.0
            {
                field = ComputeExpensiveValue()
            }
            return field
        }
    }

    fn ComputeExpensiveValue() -> f32
    {
        -- Simulate expensive work
        var sum = 0.0
        for i = 0 to 1000000
            sum += i
        return sum
    }
}
```

## Validation and Side Effects

Properties can validate input or trigger actions:

```fern
type Player
{
    i32 health = 100
    i32 maxHealth = 100

    var Health = 100
    {
        get => field
        set
        {
            field = value
            if field > maxHealth
                field = maxHealth
            else if field <= 0
            {
                field = 0
                OnDeath()
            }
        }
    }

    fn OnDeath
    {
        Print("Player died!")
    }
}
```

## Property Dependencies

Properties can depend on other properties:

```fern
type Inventory
{
    i32 gold = 0
    i32 silver = 0
    i32 copper = 0

    -- Total wealth in copper pieces (100 copper = 1 silver, 100 silver = 1 gold)
    var TotalCopper => (gold * 10000) + (silver * 100) + copper

    -- Formatted display
    var Display => "{gold}g {silver}s {copper}c"
}

var inv = new Inventory()
inv.gold = 5
inv.silver = 23
inv.copper = 47
Print(inv.TotalCopper)  -- 52347
Print(inv.Display)      -- "5g 23s 47c"
```

## Best Practices

**Use read-only properties for derived values:**
```fern
var Area => width * height        -- Good
var FullName => firstName + " " + lastName
```

**Use getters/setters for validation:**
```fern
var Speed = 0.0
{
    set
    {
        if value < 0.0
            field = 0.0
        else if value > maxSpeed
            field = maxSpeed
        else
            field = value
    }
}
```

**Avoid expensive computations in getters:**
```fern
-- Bad: computed every time
var Average => CalculateAverage()

-- Good: cache the result
var Average
{
    get
    {
        if needsRecalculation
        {
            field = CalculateAverage()
            needsRecalculation = false
        }
        return field
    }
}
```

## Special Keywords

- **`value`** - In a setter, refers to the value being assigned
- **`field`** - Refers to the backing storage for the property
- **`this`** - Refers to the current instance (useful when accessing other members)
