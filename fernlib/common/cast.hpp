#pragma once

namespace Fern
{

// Null-safe kind-based downcast. Returns nullptr for a null pointer or
// kind mismatch. Works on any class with a `template<typename T> T* as()`.
template<typename T, typename P>
T* as(P* ptr)
{
    return ptr ? ptr->template as<T>() : nullptr;
}

}
