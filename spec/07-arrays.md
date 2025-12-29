# Collections: Arrays

Arrays are typed collections that hold elements of a single type.

## Creating Arrays

```fern
-- Array literals (type inferred)
var numbers = [1, 2, 3, 4, 5]           -- i32[]
var names = ["Alice", "Bob", "Charlie"] -- string[]

-- Empty arrays
var empty = []                          -- needs type hint
i32[] scores = []                       -- explicit type

-- Multi-dimensional arrays
var grid = [[1, 2, 3], [4, 5, 6]]      -- i32[][]
```

## Accessing Elements

```fern
var fruits = ["apple", "banana", "cherry"]

var first = fruits[0]   -- "apple"
var second = fruits[1]  -- "banana"

-- Assignment
fruits[0] = "apricot"
Print(fruits[0])        -- "apricot"
```

## Array Properties

```fern
var items = [10, 20, 30, 40]

var length = items.Count   -- 4
```

## Array Operations

### Adding Elements

```fern
var numbers = [1, 2, 3]
numbers.Add(4)
Print(numbers)  -- [1, 2, 3, 4]
```

### Slicing

```fern
var items = [10, 20, 30, 40, 50]

var slice1 = items[1...3]    -- [20, 30] (exclusive end)
var slice2 = items[1..3]   -- [20, 30, 40] (inclusive end)
var slice3 = items[...3]     -- [10, 20, 30] (from start)
var slice4 = items[2..]     -- [30, 40, 50] (to end)
```

## Iterating Over Arrays

### Simple Iteration

```fern
var scores = [95, 87, 92, 88, 91]

for var score in scores
{
    Print(score)
}
-- Prints each score
```

### With Index

```fern
var players = ["Alice", "Bob", "Charlie"]

for var player in players at var i
{
    Print("Player {i + 1}: {player}")
}
-- Player 1: Alice
-- Player 2: Bob
-- Player 3: Charlie
```

### Practical Patterns

```fern
-- Find maximum
var numbers = [23, 45, 12, 67, 34]
var max = numbers[0]
for var num in numbers
{
    if num > max
    {
        max = num
    }
}
Print("Max: {max}")  -- "Max: 67"

-- Sum elements
var values = [10, 20, 30, 40, 50]
var sum = 0
for var val in values
{
    sum += val
}
Print("Sum: {sum}")  -- "Sum: 150"
```

## Multi-dimensional Arrays

```fern
var matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
]

var element = matrix[1][2]  -- 6

-- Iterate over 2D array
for var row in matrix
{
    for var cell in row
    {
        Print(cell)
    }
}
```


## Future: Generic Arrays

Eventually arrays will be reimplemented as a generic type. So they will be typed like:

```fern
Array<i32> arr = [1, 2, 3, 4, 5]
Array<Array<i32>> nested = [[1, 2, 3], [42, 69], [67, 420]]
arr.Count -- = 5
nested.Count -- = 3
nested[2].Count -- = 2
```
