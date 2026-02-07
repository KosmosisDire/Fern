#pragma once

#include "kind.hpp"
#include "source/span.hpp"
#include <string_view>

namespace Fern
{

struct Token
{
    TokenKind kind = TokenKind::Invalid;
    Span span;
    std::string_view lexeme;

    static Token Invalid()
    {
        static Token instance{TokenKind::Invalid, Span(), ""};
        return instance;
    }
};

} 
