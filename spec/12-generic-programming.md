# Generic Programming

Write code that works with multiple types using type parameters.

## What are Generics?

Instead of writing similar code for each type:

```fern
type IntBox { i32 value }
type StringBox { string value }
type FloatBox { f32 value }
```

Write one generic version:

```fern
type Box<T>
{
    T value
}

var intBox = new Box<i32>()
var stringBox = new Box<string>()
var floatBox = new Box<f32>()
```

## Generic Types

```fern
type Box<T>
{
    T item

    new(T newItem)
    {
        item = newItem
    }

    fn GetItem -> T
    {
        return item
    }

    fn SetItem(T newItem)
    {
        item = newItem
    }
}

-- Explicit type
var box1 = new Box<i32>(42)
Print(box1.GetItem())  -- 42

-- Type inference
var box2 = new Box("hello")  -- Box<string>
Print(box2.GetItem())        -- "hello"
```

## Generic Functions

```fern
fn Swap<T>(T a, T b) -> (T, T)
{
    return (b, a)
}

var (x, y) = Swap(10, 20)
Print("{x}, {y}")  -- "20, 10"

var (first, second) = Swap("hello", "world")
Print("{first}, {second}")  -- "world, hello"
```

### Multiple Type Parameters

```fern
fn Pair<T, U>(T first, U second) -> (T, U)
{
    return (first, second)
}

var result = Pair(42, "answer")
-- result is (i32, string)
```

## Type Constraints

Constraints are inferred from how you use the type:

```fern
fn Add<T>(T a, T b) -> T
{
    return a + b  -- T must support + operator
}

var sum1 = Add(5, 10)        -- Works with i32
var sum2 = Add(3.14, 2.86)   -- Works with f32
-- var sum3 = Add("a", "b")  -- Error if string doesn't support +
```

```fern
fn Max<T>(T a, T b) -> T
{
    if a > b     -- T must support > operator
        return a
    else
        return b
}

var biggest = Max(10, 20)  -- 20
```

## Generic Collections

```fern
type List<T>
{
    T[] items = []

    fn Add(T item)
    {
        items.Add(item)
    }

    fn Get(i32 index) -> T
    {
        return items[index]
    }

    fn Count -> i32
    {
        return items.Count
    }
}

var numbers = new List<i32>()
numbers.Add(1)
numbers.Add(2)
numbers.Add(3)
Print(numbers.Get(1))  -- 2

var names = new List<string>()
names.Add("Alice")
names.Add("Bob")
```

### Dictionary Example

```fern
type Dictionary<K, V>
{
    K[] keys = []
    V[] values = []

    fn Set(K key, V value)
    {
        -- Find or add key
        for var k in keys at var i
        {
            if k == key
            {
                values[i] = value
                return
            }
        }
        keys.Add(key)
        values.Add(value)
    }

    fn Get(K key) -> V
    {
        for var k in keys at var i
        {
            if k == key
            {
                return values[i]
            }
        }
        -- Handle missing key
    }
}

var scores = new Dictionary<string, i32>()
scores.Set("Alice", 95)
scores.Set("Bob", 87)
Print(scores.Get("Alice"))  -- 95
```

## Advanced Generic Patterns

### Nested Generics

```fern
var listOfLists = new List<List<i32>>()
var subList = new List<i32>()
subList.Add(1)
subList.Add(2)
listOfLists.Add(subList)

var boxOfBoxes = new Box<Box<string>>(new Box("nested!"))
```

### Self-Referencing Generics

```fern
type Node<T>
{
    T value
    Node<T> next = null

    new(T val)
    {
        value = val
    }
}

var head = new Node(1)
head.next = new Node(2)
head.next.next = new Node(3)
```

### Curiously Recurring Template Pattern (CRTP)

A type references itself as a generic parameter:

```fern
type Box<T>
{
    T item
}

-- ResolvedBox inherits from Box, passing itself as T
type ResolvedBox : Box<this>
{
    fn Describe -> string
    {
        return "This box contains: {item.ToString()}"
    }
}
```

## Best Practices

**Use descriptive type parameter names for clarity:**
```fern
type Dictionary<TKey, TValue>  -- Clear purpose
type Result<TSuccess, TError>  -- Clear meaning
```

**Keep generics simple:**
```fern
-- Good: Single responsibility
type Container<T> { T item }

-- Avoid: Too many type parameters
type ComplexThing<T, U, V, W, X>  -- Probably too complex
```

**Use generics for reusable collections:**
```fern
type Stack<T>
type Queue<T>
type Tree<T>
```
