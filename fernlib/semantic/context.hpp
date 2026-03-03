#pragma once

#include <vector>
#include <symbol/table.hpp>
#include <semantic/binder/binding.hpp>
#include <token/token.hpp>

namespace Fern
{

struct FhirMethod;

struct SemanticContext
{
    SymbolTable symbols;
    AstBinding bindings;
    std::vector<FhirMethod*> methods;

    TypeSymbol* resolve_type_name(Token name);
    TypeSymbol* resolve_type_name(TokenKind kind);

    std::string format() const;
};

}
