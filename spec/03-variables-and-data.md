# Variables & Data

## Declaring Variables

Use `var` for type inference or specify the type explicitly:

```fern
var score = 100         -- inferred as i32
var health = 75.5       -- inferred as f32

i32 level = 5           -- explicit type
string playerName       -- can declare without initializing
```

## Number Types

### Integers

```fern
var x = 42              -- i32 (default)
var y = 42u             -- u32 (unsigned)
var big = 1000000000l   -- i64 (long)
var bigU = 1000000000ul -- u64 (unsigned long)
```

### Floating Point

```fern
var pi = 3.14           -- f32 (default)
var precise = 3.14159l  -- f64 (double precision)
```

## Booleans

```fern
var isReady = true
var isGameOver = false
```

## Strings

```fern
var name = "Alice"
var empty = ""
```

### Multi-line Strings

Use triple quotes for multi-line strings:

```fern
var poem = """
Roses are red,
Violets are blue,
Fern is great,
And so are you!
"""
```

### String Interpolation

Use `{}` to embed expressions:

```fern
var x = 10
var y = 20
var sum = "The sum of {x} and {y} is {x + y}"

-- "The sum of 10 and 20 is 30"

var name = "Bob"
var age = 25
var message =
"""
Hello, {name}! 
You are {age} years old
"""

---
Hello, Bob!
You are 25 years old
---
```

### Raw Strings

Use backticks (above the tab key on most keyboards) for raw strings (no interpolation or escapes):

````fern
-- Raw strings keep special chars intact - no escape sequences needed
var path = `C:\Users\Developer\Projects\fern-lang`
var regex = `\d{3}-\d{4}`
var multiline = `This is a raw string.
It can span multiple lines without needing triple quotes.`
````

To use a backtick inside a string you must use normal double-quoted strings. If you need it with other special characters, use string escapes or concatenation.

### String Escapes

Escape special characters with a backslash:

```fern
var escaped = "He said, \"Hello, World!\""
-- He said, "Hello, World!"

var newLine = "First Line\nSecond Line"
---
First Line
Second Line
---
```

**Escape characters:**

| Character | Description               |
|-----------|---------------------------|
| `\n`      | New line                  |
| `\t`      | Tab                       |
| `\\`      | Backslash                 |
| `\"`      | Double quote              |
| `\'`      | Single quote              |

## Type Inference

Let Fern figure out types from context:

```fern
var items = [1, 2, 3]           -- i32[]
var coords = [10.5, 20.3]       -- f32[]
var dict = new Dictionary()     -- type based on constructor
```
