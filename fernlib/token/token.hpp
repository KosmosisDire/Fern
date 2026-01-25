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

    constexpr bool is_literal() const { return Fern::is_literal(kind); }
    constexpr bool is_keyword() const { return Fern::is_keyword(kind); }
    constexpr bool is_operator() const { return Fern::is_operator(kind); }
    constexpr bool is_punctuation() const { return Fern::is_punctuation(kind); }
    constexpr bool is_type_keyword() const { return Fern::is_type_keyword(kind); }
};

} 
