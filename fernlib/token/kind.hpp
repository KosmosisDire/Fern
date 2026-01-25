#pragma once

#include <optional>
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

    LiteralF32,

    Fn,
    Var,
    Return,
    F32Keyword,

    Assign,
    Plus,

    Colon,
    ThinArrow,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
};



#pragma region to_string

constexpr std::string_view to_string(TokenKind k)
{
    switch (k)
    {
        case TokenKind::Invalid:     return "Invalid";
        case TokenKind::EndOfFile:   return "EndOfFile";
        case TokenKind::Newline:     return "Newline";
        case TokenKind::Identifier:  return "Identifier";
        case TokenKind::LiteralF32:  return "LiteralF32";
        case TokenKind::Fn:          return "Fn";
        case TokenKind::Var:         return "Var";
        case TokenKind::Return:      return "Return";
        case TokenKind::F32Keyword:  return "F32Keyword";
        case TokenKind::Assign:      return "Assign";
        case TokenKind::Plus:        return "Plus";
        case TokenKind::Colon:       return "Colon";
        case TokenKind::ThinArrow:   return "ThinArrow";
        case TokenKind::LeftParen:   return "LeftParen";
        case TokenKind::RightParen:  return "RightParen";
        case TokenKind::LeftBrace:   return "LeftBrace";
        case TokenKind::RightBrace:  return "RightBrace";
        case TokenKind::Semicolon:   return "Semicolon";
        case TokenKind::Comma:       return "Comma";
    }
}



#pragma region Category Checks

constexpr bool is_literal(TokenKind k)
{
    switch (k)
    {
        case TokenKind::LiteralF32:
            return true;
        default:
            return false;
    }
}

constexpr bool is_keyword(TokenKind k)
{
    switch (k)
    {
        case TokenKind::Fn:
        case TokenKind::Var:
        case TokenKind::Return:
        case TokenKind::F32Keyword:
            return true;
        default:
            return false;
    }
}

constexpr bool is_operator(TokenKind k)
{
    switch (k)
    {
        case TokenKind::Assign:
        case TokenKind::Plus:
            return true;
        default:
            return false;
    }
}

constexpr bool is_punctuation(TokenKind k)
{
    switch (k)
    {
        case TokenKind::Colon:
        case TokenKind::ThinArrow:
        case TokenKind::LeftParen:
        case TokenKind::RightParen:
        case TokenKind::LeftBrace:
        case TokenKind::RightBrace:
        case TokenKind::Semicolon:
        case TokenKind::Comma:
            return true;
        default:
            return false;
    }
}

constexpr bool is_type_keyword(TokenKind k)
{
    switch (k)
    {
        case TokenKind::F32Keyword:
            return true;
        default:
            return false;
    }
}



#pragma region Semantic Enums

enum class BinaryOp
{
    Add,
};

enum class AssignOp
{
    Simple,
};



#pragma region Operator to_string

constexpr std::string_view to_string(BinaryOp op)
{
    switch (op)
    {
        case BinaryOp::Add: return "Add";
    }
}

constexpr std::string_view to_string(AssignOp op)
{
    switch (op)
    {
        case AssignOp::Simple: return "Simple";
    }
}



#pragma region Conversions

constexpr std::optional<BinaryOp> to_binary_op(TokenKind k)
{
    switch (k)
    {
        case TokenKind::Plus:
            return BinaryOp::Add;
        default:
            return std::nullopt;
    }
}

constexpr std::optional<AssignOp> to_assign_op(TokenKind k)
{
    switch (k)
    {
        case TokenKind::Assign:
            return AssignOp::Simple;
        default:
            return std::nullopt;
    }
}



} 
