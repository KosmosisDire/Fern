#pragma once

#include <symbol/table.hpp>
#include <semantic/binder/binding.hpp>
#include <token/token.hpp>

namespace Fern
{

struct SemanticContext
{
    SymbolTable symbols;
    AstBinding bindings;

    TypeSymbol* resolve_type_name(Token name);
    TypeSymbol* resolve_type_name(TokenKind kind);

    std::string format() const;
};

}
