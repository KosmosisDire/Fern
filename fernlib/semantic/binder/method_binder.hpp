#pragma once

#include "binder.hpp"

namespace Fern
{

// Binder for code written within a method body. Exposes the method with
// containing_method() and contributes the method's parameters to lookup.
class MethodBinder : public Binder
{
public:
    MethodBinder(Binder& parent, MethodSymbol* m)
        : Binder(parent), method(m)
    {
    }

protected:
    MethodSymbol* containing_method() override { return method; }
    LookupResult lookup_in_single_binder(std::string_view name) override;

private:
    MethodSymbol* method;
};

}
