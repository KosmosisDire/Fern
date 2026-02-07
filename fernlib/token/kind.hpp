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
    Type,
    Namespace,
    Return,
    F32Keyword,

    Assign,
    AssignAdd,
    Plus,

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
};

enum class AssignOp
{
    Simple,
    Add,
};

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
        case TokenKind::AssignAdd:
            return AssignOp::Add;
        default:
            return std::nullopt;
    }
}


#pragma region Format

constexpr std::string_view format(TokenKind k)
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
        case TokenKind::Type:        return "Type";
        case TokenKind::Namespace:   return "Namespace";
        case TokenKind::Return:      return "Return";
        case TokenKind::F32Keyword:  return "F32Keyword";
        case TokenKind::Assign:      return "Assign";
        case TokenKind::AssignAdd:   return "AssignAdd";
        case TokenKind::Plus:        return "Plus";
        case TokenKind::Colon:       return "Colon";
        case TokenKind::ThinArrow:   return "ThinArrow";
        case TokenKind::LeftParen:   return "LeftParen";
        case TokenKind::RightParen:  return "RightParen";
        case TokenKind::LeftBrace:   return "LeftBrace";
        case TokenKind::RightBrace:  return "RightBrace";
        case TokenKind::Semicolon:   return "Semicolon";
        case TokenKind::Comma:       return "Comma";
        case TokenKind::Dot:         return "Dot";
    }
}

constexpr std::string_view format(BinaryOp op)
{
    switch (op)
    {
        case BinaryOp::Add: return "Add";
    }
}

constexpr std::string_view format(AssignOp op)
{
    switch (op)
    {
        case AssignOp::Simple: return "Simple";
        case AssignOp::Add: return "Add";
    }
}




} 
