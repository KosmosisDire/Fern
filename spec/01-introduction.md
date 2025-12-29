# Introduction

Fern is a general-purpose, statically typed, embeddable language inspired by C#, Rust, Swift, and Python. It aims to be intuitive, performant, safe, and powerful.

## Quick Example

```fern
type Person
{
    string name
    i32 age
    var Greeting => "Hello, my name is {name} and I am {age}"

    fn HaveBirthday
    {
        age += 1
        Print("Happy birthday {name}, you're now {age}!")
    }
}

var person = new Person("Alice", 30)
Print(person.Greeting) -- "Hello, my name is Alice and I am 30"
person.HaveBirthday() -- "Happy birthday Alice! You're now 31!"
```
