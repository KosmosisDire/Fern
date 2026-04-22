#pragma once

#include <string>
#include <unordered_map>

#include "binder.hpp"

namespace Fern
{

// Binder for code within a type. Contributes members + type params, plus a
// substitution map for generic instantiations.
class TypeBinder : public Binder
{
public:
    TypeBinder(Binder& parent, NamedTypeSymbol* type)
        : Binder(parent), typeSymbol(type)
    {
    }

    std::unordered_map<std::string, TypeSymbol*>& substitutions() { return typeParamSubs; }

protected:
    NamedTypeSymbol* containing_type() override { return typeSymbol; }
    const std::unordered_map<std::string, TypeSymbol*>* type_param_substitutions() override { return &typeParamSubs; }
    Symbol* lookup_in_single_binder(std::string_view name) override;

private:
    NamedTypeSymbol* typeSymbol;
    std::unordered_map<std::string, TypeSymbol*> typeParamSubs;
};

}
