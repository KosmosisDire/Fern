#pragma once

#include <vector>
#include <string_view>
#include <symbol/table.hpp>
#include <token/token.hpp>

namespace Fern
{

struct FhirMethod;

struct SemanticContext
{
    SymbolTable symbols;
    std::vector<FhirMethod*> methods;

    TypeSymbol* resolve_type_name(Token name);
    TypeSymbol* resolve_type_name(std::string_view alias);

    std::string format() const;
};

}
