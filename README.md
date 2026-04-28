# Fern

Fern is a programming language which aims to be delightful to use. It is statically typed, with syntax inspired by Rust and C# and semantics inspired by C#. It aims to run anywhere from embedded Lua-like scripting to native cross-platform code. The compiler is written in c++ and compiles to an interpretable IR which runs in a VM. It can also be compiled to WASM, or optionally to native code via LLVM.

For now this is the GOAL of the language, but is not yet finished.


## Pipeline

The way I hope for the compiler to work is:

Source -> Lexer -> Parser -> AST -> Binder -> FHIR (Fern High-level IR) -> FLIR (Fern Low-level IR) > Backends (VM, WASM, LLVM)


## Building

Fern requires:

* CMake 3.18 or newer
* Ninja
* Clang 19 or newer

### Windows

Install the toolchain:

```
winget install Kitware.CMake Ninja-build.Ninja LLVM.LLVM
```

Then add LLVM to `PATH` if it isn't already, using powershell:

```powershell
[Environment]::SetEnvironmentVariable(
    "Path",
    [Environment]::GetEnvironmentVariable("Path", "User") + ";C:\Program Files\LLVM\bin",
    "User"
)
```

> **Note:** if you ran this without admin, LLVM installs but does not add itself to `PATH`. Either reinstall elevated, or add `C:\Program Files\LLVM\bin` to `PATH` manually.

Open a fresh shell, then configure and build:

```
cmake --preset windows-debug
cmake --build --preset windows-debug-build
```

Output binaries are at:

* `build/debug/bin/ferncli.exe`
* `build/debug/bin/fernlsp.exe`

### Debian/Ubuntu

Install Clang 19, build tools, and required libraries:

```
sudo apt install clang-19 cmake ninja-build libzstd-dev libstdc++-13-dev
```

> **Note (Ubuntu 22.04):** the default repos do not ship `clang-19` or `libstdc++-13-dev`. Add the LLVM apt repo and the toolchain PPA first:
>
> ```
> wget https://apt.llvm.org/llvm.sh
> chmod +x llvm.sh
> sudo ./llvm.sh 19
> sudo add-apt-repository ppa:ubuntu-toolchain-r/test
> sudo apt update
> ```
>
> Then run the `apt install` line above.

Configure and build:

```
cmake --preset linux-debug
cmake --build --preset linux-debug-build
```

Output binaries are at:

* `build/debug/bin/ferncli`
* `build/debug/bin/fernlsp`