# Namespaces

Organize code into logical hierarchies to avoid naming conflicts and improve project structure.

## Why Namespaces?

Without namespaces, all names share one global space:

```fern
type Renderer { }  -- Graphics renderer
type Renderer { }  -- Error: name conflict with audio renderer!
```

With namespaces:

```fern
namespace Graphics
{
    type Renderer { }
}

namespace Audio
{
    type Renderer { }
}
-- No conflict!
```

## Declaring Namespaces

```fern
namespace Graphics
{
    type Renderer
    {
        fn Draw
        {
            Print("Drawing...")
        }
    }

    fn Initialize
    {
        Print("Graphics initialized")
    }
}

var renderer = new Graphics.Renderer()
Graphics.Initialize()
```

## Nested Namespaces

Use dot notation for clean hierarchy:

```fern
namespace Game.Entities
{
    type Player { }
    type Enemy { }
}

namespace Game.Systems
{
    type Physics { }
    type AI { }
}

var player = new Game.Entities.Player()
var physics = new Game.Systems.Physics()
```

Nested namespaces work but create deep indentation:

```fern
namespace Game
{
    namespace Entities
    {
        type Enemy { }
    }
}
```

## Using Namespaces

Access with fully qualified names:

```fern
var renderer = new Graphics.Renderer()
renderer.Draw()
```

Or import with `using`:

```fern
using Graphics

var renderer = new Renderer()  -- No prefix needed
renderer.Draw()
```

Import multiple namespaces:

```fern
using Graphics
using Game.Entities
using Game.Systems

var renderer = new Renderer()
var player = new Player()
var physics = new Physics()
```

## File-scoped Namespaces

Apply a namespace to the entire file without braces:

```fern
namespace Game.Systems

type Physics
{
    fn Update { }
}

type Collision
{
    fn Check { }
}

fn InitializeSystems
{
    Print("Systems initialized")
}
```

All declarations belong to `Game.Systems`. No indentation needed!

## Global Namespace

Code without a namespace declaration lives in the global namespace:

```fern
-- File: main.fn
type Program
{
    fn Main
    {
        Print("Hello!")
    }
}
```

Access from anywhere without qualification.

## Best Practices

**Match project structure:**
```txt
Graphics/
    Renderer.fn     -> namespace Graphics
    Shader.fn       -> namespace Graphics
Game/
    Entities/
        Player.fn       -> namespace Game.Entities
        Enemy.fn        -> namespace Game.Entities
```

**Use file-scoped namespaces for cleaner code:**
```fern
namespace MyGame.Core

-- No indentation needed
type Game { }
type Player { }
```
