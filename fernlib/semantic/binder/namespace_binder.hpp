#pragma once

#include "binder.hpp"

namespace Fern
{

// Binder for code written within a namespace. Contributes the namespace's members
// to lookup and exposes it via containing_namespace().
class NamespaceBinder : public Binder
{
public:
    NamespaceBinder(Binder& parent, NamespaceSymbol* ns)
        : Binder(parent), namespaceSymbol(ns)
    {
    }

protected:
    NamespaceSymbol* containing_namespace() override { return namespaceSymbol; }
    LookupResult lookup_in_single_binder(std::string_view name) override;

private:
    NamespaceSymbol* namespaceSymbol;
};

}
