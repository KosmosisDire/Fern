# Syntax Basics

## Statements and Newlines

Newlines end statements. Semicolons are optional:

```fern
var x = 10
x += 1

-- Semicolons let you put multiple statements on one line
var y = 5; y *= 2
```

## Comments

```fern
-- Single line comment

---
Multi-line comments use
three or more hyphens
---

--- Region Name ---
-- Creates a collapsible region in your editor
```

Note: There are no `++` or `--` operators.

## Identifiers

Valid names contain letters, digits, and underscores. Cannot start with a digit:

```fern
var myVar = 1      -- valid
var my_var_2 = 2   -- valid
var 2fast = 3      -- invalid
```

## Naming Conventions

| Type     | Convention | Example                    |
|----------|------------|----------------------------|
| Variable | camelCase  | `var userName = "Alice"`   |
| Function | PascalCase | `fn ProcessData() {}`      |
| Property | PascalCase | `var Name { get; set; }`   |
| Type     | PascalCase | `type Player {}`           |
