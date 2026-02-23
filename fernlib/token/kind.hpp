#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace Fern
{

#pragma region TokenKind

enum class TokenKind
{
    Invalid,
    EndOfFile,
    Newline,
    Identifier,

    // Literals
    LiteralF32,
    LiteralI32,
    LiteralBool,

    // Type keywords
    F32Keyword,
    I32Keyword,
    BoolKeyword,

    // Keywords
    Pub,
    Static,
    Ref,
    Attr,
    Fn,
    Init,
    Op,
    Var,
    Type,
    Namespace,
    Return,
    This,
    If,
    Else,
    While,

    // Assignment operators
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,

    // Arithmetic operators
    Plus,
    Minus,
    Star,
    Slash,

    // Comparison operators
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,

    // Logical operators
    And,
    Or,
    Not,

    // Other symbols
    At,
    Colon,
    ThinArrow,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
    Dot,
};


#pragma region Semantic Enums

enum class BinaryOp
{
    Add,
    Sub,
    Mul,
    Div,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
    And,
    Or,
};

enum class Precedence
{
    None = 0,
    Or,
    And,
    Comparison,
    Addition,
    Multiplication
};

enum class UnaryOp
{
    Negative,
    Positive,
    Not,
};

enum class AssignOp
{
    Simple,
    Add,
    Sub,
    Mul,
    Div,
};

enum class Modifier : uint16_t
{
    None   = 0,
    Public = 1 << 0,
    Static = 1 << 1,
    Ref    = 1 << 2,
    Attr   = 1 << 3,
};

constexpr Modifier operator|(Modifier a, Modifier b)
{
    return static_cast<Modifier>(static_cast<uint16_t>(a) | static_cast<uint16_t>(b));
}

constexpr Modifier operator&(Modifier a, Modifier b)
{
    return static_cast<Modifier>(static_cast<uint16_t>(a) & static_cast<uint16_t>(b));
}

constexpr bool has_modifier(Modifier set, Modifier flag)
{
    return (set & flag) == flag;
}

#pragma region Category Helpers

constexpr std::optional<Modifier> to_modifier(TokenKind k)
{
    switch (k)
    {
        case TokenKind::Pub:    return Modifier::Public;
        case TokenKind::Static: return Modifier::Static;
        case TokenKind::Ref:    return Modifier::Ref;
        case TokenKind::Attr:   return Modifier::Attr;
        default:                return std::nullopt;
    }
}

constexpr bool is_modifier(TokenKind k)
{
    return k == TokenKind::Pub ||
           k == TokenKind::Static ||
           k == TokenKind::Ref ||
           k == TokenKind::Attr;
}

constexpr bool is_literal(TokenKind k)
{
    return k == TokenKind::LiteralF32 ||
           k == TokenKind::LiteralI32 ||
           k == TokenKind::LiteralBool;
}

constexpr bool is_type_keyword(TokenKind k)
{
    return k == TokenKind::F32Keyword ||
           k == TokenKind::I32Keyword ||
           k == TokenKind::BoolKeyword;
}

constexpr bool is_operator_token(TokenKind k)
{
    return k == TokenKind::Plus ||
           k == TokenKind::Minus ||
           k == TokenKind::Star ||
           k == TokenKind::Slash ||
           k == TokenKind::Greater ||
           k == TokenKind::Less ||
           k == TokenKind::GreaterEqual ||
           k == TokenKind::LessEqual ||
           k == TokenKind::Equal ||
           k == TokenKind::NotEqual ||
           k == TokenKind::And ||
           k == TokenKind::Or ||
           k == TokenKind::Not;
}

constexpr TokenKind binary_op_to_token(BinaryOp op)
{
    switch (op)
    {
        case BinaryOp::Add:          return TokenKind::Plus;
        case BinaryOp::Sub:          return TokenKind::Minus;
        case BinaryOp::Mul:          return TokenKind::Star;
        case BinaryOp::Div:          return TokenKind::Slash;
        case BinaryOp::Greater:      return TokenKind::Greater;
        case BinaryOp::Less:         return TokenKind::Less;
        case BinaryOp::GreaterEqual: return TokenKind::GreaterEqual;
        case BinaryOp::LessEqual:    return TokenKind::LessEqual;
        case BinaryOp::Equal:        return TokenKind::Equal;
        case BinaryOp::NotEqual:     return TokenKind::NotEqual;
        case BinaryOp::And:          return TokenKind::And;
        case BinaryOp::Or:           return TokenKind::Or;
    }
}

constexpr TokenKind unary_op_to_token(UnaryOp op)
{
    switch (op)
    {
        case UnaryOp::Negative: return TokenKind::Minus;
        case UnaryOp::Positive: return TokenKind::Plus;
        case UnaryOp::Not:      return TokenKind::Not;
    }
}

constexpr std::optional<TokenKind> literal_to_type_keyword(TokenKind k)
{
    switch (k)
    {
        case TokenKind::LiteralF32:  return TokenKind::F32Keyword;
        case TokenKind::LiteralI32:  return TokenKind::I32Keyword;
        case TokenKind::LiteralBool: return TokenKind::BoolKeyword;
        default:                     return std::nullopt;
    }
}

#pragma region Conversions

constexpr std::optional<BinaryOp> to_binary_op(TokenKind k)
{
    switch (k)
    {
        case TokenKind::Plus:
            return BinaryOp::Add;
        case TokenKind::Minus:
            return BinaryOp::Sub;
        case TokenKind::Star:
            return BinaryOp::Mul;
        case TokenKind::Slash:
            return BinaryOp::Div;
        case TokenKind::Greater:
            return BinaryOp::Greater;
        case TokenKind::Less:
            return BinaryOp::Less;
        case TokenKind::GreaterEqual:
            return BinaryOp::GreaterEqual;
        case TokenKind::LessEqual:
            return BinaryOp::LessEqual;
        case TokenKind::Equal:
            return BinaryOp::Equal;
        case TokenKind::NotEqual:
            return BinaryOp::NotEqual;
        case TokenKind::And:
            return BinaryOp::And;
        case TokenKind::Or:
            return BinaryOp::Or;
        default:
            return std::nullopt;
    }
}

constexpr Precedence precedence_of(BinaryOp op)
{
    switch (op)
    {
        case BinaryOp::Or:
            return Precedence::Or;
        case BinaryOp::And:
            return Precedence::And;
        case BinaryOp::Greater:
        case BinaryOp::Less:
        case BinaryOp::GreaterEqual:
        case BinaryOp::LessEqual:
        case BinaryOp::Equal:
        case BinaryOp::NotEqual:
            return Precedence::Comparison;
        case BinaryOp::Add:
        case BinaryOp::Sub:
            return Precedence::Addition;
        case BinaryOp::Mul:
        case BinaryOp::Div:
            return Precedence::Multiplication;
    }
}

constexpr std::optional<UnaryOp> to_unary_op(TokenKind k)
{
    switch (k)
    {
        case TokenKind::Minus: return UnaryOp::Negative;
        case TokenKind::Plus:  return UnaryOp::Positive;
        case TokenKind::Not:   return UnaryOp::Not;
        default:               return std::nullopt;
    }
}

constexpr std::optional<AssignOp> to_assign_op(TokenKind k)
{
    switch (k)
    {
        case TokenKind::Assign:
            return AssignOp::Simple;
        case TokenKind::AssignAdd:
            return AssignOp::Add;
        case TokenKind::AssignSub:
            return AssignOp::Sub;
        case TokenKind::AssignMul:
            return AssignOp::Mul;
        case TokenKind::AssignDiv:
            return AssignOp::Div;
        default:
            return std::nullopt;
    }
}

#pragma region Format

constexpr std::string_view format(TokenKind k)
{
    switch (k)
    {
        case TokenKind::Invalid:      return "Invalid";
        case TokenKind::EndOfFile:    return "EndOfFile";
        case TokenKind::Newline:      return "Newline";
        case TokenKind::Identifier:   return "Identifier";

        case TokenKind::LiteralF32:   return "LiteralF32";
        case TokenKind::LiteralI32:   return "LiteralI32";
        case TokenKind::LiteralBool:  return "LiteralBool";
        case TokenKind::F32Keyword:   return "F32Keyword";
        case TokenKind::I32Keyword:   return "I32Keyword";
        case TokenKind::BoolKeyword:  return "BoolKeyword";

        case TokenKind::Pub:          return "Pub";
        case TokenKind::Static:       return "Static";
        case TokenKind::Ref:          return "Ref";
        case TokenKind::Attr:         return "Attr";
        case TokenKind::Fn:           return "Fn";
        case TokenKind::Init:         return "Init";
        case TokenKind::Op:           return "Op";
        case TokenKind::Var:          return "Var";
        case TokenKind::Type:         return "Type";
        case TokenKind::Namespace:    return "Namespace";
        case TokenKind::Return:       return "Return";
        case TokenKind::This:         return "This";
        case TokenKind::If:           return "If";
        case TokenKind::Else:         return "Else";
        case TokenKind::While:        return "While";

        case TokenKind::Assign:       return "=";
        case TokenKind::AssignAdd:    return "+=";
        case TokenKind::AssignSub:    return "-=";
        case TokenKind::AssignMul:    return "*=";
        case TokenKind::AssignDiv:    return "/=";
        case TokenKind::Plus:         return "+";
        case TokenKind::Minus:        return "-";
        case TokenKind::Star:         return "*";
        case TokenKind::Slash:        return "/";
        case TokenKind::Greater:      return ">";
        case TokenKind::Less:         return "<";
        case TokenKind::GreaterEqual: return ">=";
        case TokenKind::LessEqual:    return "<=";
        case TokenKind::Equal:        return "==";
        case TokenKind::NotEqual:     return "!=";
        case TokenKind::And:          return "&&";
        case TokenKind::Or:           return "||";
        case TokenKind::Not:          return "!";

        case TokenKind::At:           return "@";
        case TokenKind::Colon:        return ":";
        case TokenKind::ThinArrow:    return "->";
        case TokenKind::LeftParen:    return "(";
        case TokenKind::RightParen:   return ")";
        case TokenKind::LeftBrace:    return "{";
        case TokenKind::RightBrace:   return "}";
        case TokenKind::Semicolon:    return ";";
        case TokenKind::Comma:        return ",";
        case TokenKind::Dot:          return ".";
    }
}

constexpr std::string_view format(BinaryOp op)
{
    switch (op)
    {
        case BinaryOp::Add:          return "+";
        case BinaryOp::Sub:          return "-";
        case BinaryOp::Mul:          return "*";
        case BinaryOp::Div:          return "/";
        case BinaryOp::Greater:      return ">";
        case BinaryOp::Less:         return "<";
        case BinaryOp::GreaterEqual: return ">=";
        case BinaryOp::LessEqual:    return "<=";
        case BinaryOp::Equal:        return "==";
        case BinaryOp::NotEqual:     return "!=";
        case BinaryOp::And:          return "&&";
        case BinaryOp::Or:           return "||";
    }
}

constexpr std::string_view format(UnaryOp op)
{
    switch (op)
    {
        case UnaryOp::Negative:     return "-";
        case UnaryOp::Positive:     return "+";
        case UnaryOp::Not:          return "!";
    }
}

constexpr std::string_view format(AssignOp op)
{
    switch (op)
    {
        case AssignOp::Simple: return "=";
        case AssignOp::Add:    return "+=";
        case AssignOp::Sub:    return "-=";
        case AssignOp::Mul:    return "*=";
        case AssignOp::Div:    return "/=";
    }
}

inline std::string format(Modifier mods)
{
    std::string result;
    if (has_modifier(mods, Modifier::Public))
    {
        result += "pub ";
    }
    if (has_modifier(mods, Modifier::Static))
    {
        result += "static ";
    }
    if (has_modifier(mods, Modifier::Ref))
    {
        result += "ref ";
    }
    if (has_modifier(mods, Modifier::Attr))
    {
        result += "attr ";
    }
    if (!result.empty())
    {
        result.pop_back();
    }
    return result;
}

}
