# Operators & Expressions

## Arithmetic Operators

```fern
var a = 10 + 5      -- 15 (addition)
var b = 10 - 5      -- 5 (subtraction)
var c = 10 * 5      -- 50 (multiplication)
var d = 10 / 5      -- 2 (division)
var e = 10 % 3      -- 1 (modulus/remainder)
var f = 2 ** 8      -- 256 (exponentiation)
var g = -a          -- -15 (negation)
```

## Comparison Operators

```fern
var x = 10
var y = 20

x == y    -- false (equal to)
x != y    -- true (not equal to)
x > y     -- false (greater than)
x < y     -- true (less than)
x >= 10   -- true (greater than or equal)
y <= 20   -- true (less than or equal)
```

## Logical Operators

```fern
var isReady = true
var hasPermission = false

isReady && hasPermission    -- false (AND)
isReady || hasPermission    -- true (OR)
!isReady                    -- false (NOT)

-- Combine them
var canProceed = isReady && (hasPermission || isAdmin)
```

## Bitwise Operators

```fern
var flags = 0b1010

flags & 0b1100      -- 0b1000 (AND)
flags | 0b0101      -- 0b1111 (OR)
flags ^ 0b1111      -- 0b0101 (XOR)
~flags              -- 0b0101 (NOT)
flags << 2          -- 0b101000 (left shift)
flags >> 1          -- 0b0101 (right shift)
```

## Operator Precedence

From highest to lowest:

1. Unary: `-`, `!`, `~`
2. Exponentiation: `**`
3. Multiplicative: `*`, `/`, `%`
4. Additive: `+`, `-`
5. Shift: `<<`, `>>`
6. Bitwise AND: `&`
7. Bitwise XOR: `^`
8. Bitwise OR: `|`
9. Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
10. Logical AND: `&&`
11. Logical OR: `||`

Use parentheses to be explicit:

```fern
var result = (a + b) * c
```

## Range Operators

Fern provides two range operators for creating ranges of values.

### Inclusive Range (`..`)

Creates a range that includes the end value:

```fern
0..9     -- Range from 0 to 9
```

### Exclusive Range (`...`)

Creates a range that excludes the end value:

```fern
0...10    -- Range from 0 to 9
```

### Open-Ended Ranges

Ranges can be open-ended:

```fern
..9      -- up to 9 (inclusive)
...10    -- From start to 9 ( exclusive)
9..      -- From 10 to end
```

Ranges are commonly used in:
- **For loops** - See [Control Flow](/05-control-flow)
- **Match expressions** - See [Control Flow](/05-control-flow)
- **Array slicing** - See [Arrays](/07-arrays)
