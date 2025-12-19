#pragma once

#include <cstdint>
#include <string_view>
#include <string>
#include <magic_enum.hpp>

namespace Fern
{
    // Token kinds - all possible tokens in Fern language
    enum class TokenKind
    {
        // Special tokens
        None = 0,
        EndOfFile,
        Invalid,

        // Literals
        LiteralI32,
        LiteralF32,
        LiteralString,
        LiteralChar,
        LiteralBool,
        Null,
        Void,

        // Identifiers and keywords
        Identifier,

        // Declaration keywords
        Type,
        Enum,
        Var,
        Fn,

        // Function keywords
        New,

        // Control flow keywords
        If,
        Else,
        While,
        For,
        Match,
        Break,
        Continue,
        Return,
        In,
        At,

        // Property keywords
        Get,
        Set,

        // Modifier keywords
        Public,
        Private,
        Protected,
        Static,
        Virtual,
        Override,
        Abstract,
        Extern,
        Ref,

        // Other keywords
        This,
        Using,
        Namespace,
        Where,
        By, // (for 0..10 by 2) syntax

        // Operators - Arithmetic
        Plus,     // +
        Minus,    // -
        Asterisk, // *
        Slash,    // /
        Percent,  // %

        // Operators - Assignment
        Assign,             // =
        PlusAssign,         // +=
        MinusAssign,        // -=
        StarAssign,         // *=
        SlashAssign,        // /=
        PercentAssign,      // %=
        AndAssign,          // &=
        OrAssign,           // |=
        XorAssign,          // ^=
        LeftShiftAssign,    // <<=
        RightShiftAssign,   // >>=
        NullCoalesceAssign, // ??=

        // Operators - Comparison
        Equal,        // ==
        NotEqual,     // !=
        Less,         // <
        LessEqual,    // <=
        Greater,      // >
        GreaterEqual, // >=

        // Operators - Logical
        And, // &&
        Or,  // ||
        Not, // !

        // Operators - Bitwise
        BitwiseAnd, // &
        BitwiseOr,  // |
        BitwiseXor, // ^
        BitwiseNot, // ~
        LeftShift,  // <<
        RightShift, // >>

        // Operators - Other
        Question,     // ?
        Colon,        // :
        FatArrow,     // =>
        ThinArrow,    // ->
        Dot,          // .
        DotDot,       // ..
        DotDotDot,    // ...
        NullCoalesce, // ??

        // Punctuation
        LeftParen,    // (
        RightParen,   // )
        LeftBrace,    // {
        RightBrace,   // }
        LeftBracket,  // [
        RightBracket, // ]
        Semicolon,    // ;
        Comma,        // ,
    };

    enum class KeywordKind
    {
        Invalid = (int)TokenKind::Invalid,
        Type = (int)TokenKind::Type,
        Ref = (int)TokenKind::Ref,
        Enum = (int)TokenKind::Enum,
        Var = (int)TokenKind::Var,
        Fn = (int)TokenKind::Fn,
        New = (int)TokenKind::New,
        Return = (int)TokenKind::Return,
        If = (int)TokenKind::If,
        Else = (int)TokenKind::Else,
        While = (int)TokenKind::While,
        For = (int)TokenKind::For,
        Match = (int)TokenKind::Match,
        Break = (int)TokenKind::Break,
        Continue = (int)TokenKind::Continue,
        In = (int)TokenKind::In,
        At = (int)TokenKind::At,
        Get = (int)TokenKind::Get,
        Set = (int)TokenKind::Set,
        Public = (int)TokenKind::Public,
        Private = (int)TokenKind::Private,
        Protected = (int)TokenKind::Protected,
        Static = (int)TokenKind::Static,
        Virtual = (int)TokenKind::Virtual,
        Override = (int)TokenKind::Override,
        Abstract = (int)TokenKind::Abstract,
        Extern = (int)TokenKind::Extern,
        This = (int)TokenKind::This,
        Where = (int)TokenKind::Where,
        Using = (int)TokenKind::Using,
        Namespace = (int)TokenKind::Namespace,
        By = (int)TokenKind::By
    };

    enum class UnaryOperatorKind
    {
        Invalid = (int)TokenKind::Invalid,
        Plus = (int)TokenKind::Plus,
        Minus = (int)TokenKind::Minus,
        Not = (int)TokenKind::Not,
        BitwiseNot = (int)TokenKind::BitwiseNot,
        AddressOf = (int)TokenKind::BitwiseAnd,
        Dereference = (int)TokenKind::Asterisk,
    };

    enum class BinaryOperatorKind
    {
        Invalid = (int)TokenKind::Invalid,
        Add = (int)TokenKind::Plus,
        Subtract = (int)TokenKind::Minus,
        Multiply = (int)TokenKind::Asterisk,
        Divide = (int)TokenKind::Slash,
        Modulo = (int)TokenKind::Percent,
        Equals = (int)TokenKind::Equal,
        NotEquals = (int)TokenKind::NotEqual,
        LessThan = (int)TokenKind::Less,
        GreaterThan = (int)TokenKind::Greater,
        LessThanOrEqual = (int)TokenKind::LessEqual,
        GreaterThanOrEqual = (int)TokenKind::GreaterEqual,
        LogicalAnd = (int)TokenKind::And,
        LogicalOr = (int)TokenKind::Or,
        BitwiseAnd = (int)TokenKind::BitwiseAnd,
        BitwiseOr = (int)TokenKind::BitwiseOr,
        BitwiseXor = (int)TokenKind::BitwiseXor,
        LeftShift = (int)TokenKind::LeftShift,
        RightShift = (int)TokenKind::RightShift,
        Coalesce = (int)TokenKind::NullCoalesce,
    };

    enum class AssignmentOperatorKind
    {
        Invalid = (int)TokenKind::Invalid,
        Assign = (int)TokenKind::Assign,
        Add = (int)TokenKind::PlusAssign,
        Subtract = (int)TokenKind::MinusAssign,
        Multiply = (int)TokenKind::StarAssign,
        Divide = (int)TokenKind::SlashAssign,
        Modulo = (int)TokenKind::PercentAssign,
        And = (int)TokenKind::AndAssign,
        Or = (int)TokenKind::OrAssign,
        Xor = (int)TokenKind::XorAssign,
        LeftShift = (int)TokenKind::LeftShiftAssign,
        RightShift = (int)TokenKind::RightShiftAssign,
        Coalesce = (int)TokenKind::NullCoalesceAssign,
    };

    enum class PropertyKind
    {
        Invalid = (int)TokenKind::Invalid,
        Get = (int)TokenKind::Get,
        Set = (int)TokenKind::Set,
    };

    enum class ModifierKindFlags : uint32_t
    {
        None = 0,
        Public = 1 << 0,
        Private = 1 << 1,
        Protected = 1 << 2,
        Static = 1 << 3,
        Ref = 1 << 4,
        Virtual = 1 << 5,
        Override = 1 << 6,
        Abstract = 1 << 7,
        Extern = 1 << 8,
        Enum = 1 << 9,
        Invalid = 1 << 11
    };

    enum class AccessModifierKind
    {
        None = (int)ModifierKindFlags::None,
        Public = (int)ModifierKindFlags::Public,
        Private = (int)ModifierKindFlags::Private,
        Protected = (int)ModifierKindFlags::Protected,
    };

    enum class SymbolKind {
        Namespace,
        Type,
        Function,
        Variable,
        Property,
        EnumCase,
        Block,
    };

    enum class LiteralKind
    {
        Invalid = (int)TokenKind::Invalid,
        I32 = (int)TokenKind::LiteralI32,
        F32 = (int)TokenKind::LiteralF32,
        String = (int)TokenKind::LiteralString,
        Char = (int)TokenKind::LiteralChar,
        Bool = (int)TokenKind::LiteralBool,
        Null = (int)TokenKind::Null,
        Void = (int)TokenKind::Void,
    };

    // Trivia kinds - whitespace and comments
    enum class TriviaKind : uint8_t
    {
        Whitespace,   // Spaces, tabs
        Newline,      // \n, \r\n, \r
        LineComment,  // // comment
        BlockComment, // /* comment */
    };

    // Bitwise operators for ModifierKindFlags
    inline ModifierKindFlags operator|(ModifierKindFlags lhs, ModifierKindFlags rhs)
    {
        return static_cast<ModifierKindFlags>(
            static_cast<uint32_t>(lhs) | static_cast<uint32_t>(rhs));
    }

    inline ModifierKindFlags operator&(ModifierKindFlags lhs, ModifierKindFlags rhs)
    {
        return static_cast<ModifierKindFlags>(
            static_cast<uint32_t>(lhs) & static_cast<uint32_t>(rhs));
    }

    inline ModifierKindFlags operator^(ModifierKindFlags lhs, ModifierKindFlags rhs)
    {
        return static_cast<ModifierKindFlags>(
            static_cast<uint32_t>(lhs) ^ static_cast<uint32_t>(rhs));
    }

    inline ModifierKindFlags operator~(ModifierKindFlags flags)
    {
        return static_cast<ModifierKindFlags>(~static_cast<uint32_t>(flags));
    }

    inline ModifierKindFlags &operator|=(ModifierKindFlags &lhs, ModifierKindFlags rhs)
    {
        lhs = lhs | rhs;
        return lhs;
    }

    inline ModifierKindFlags &operator&=(ModifierKindFlags &lhs, ModifierKindFlags rhs)
    {
        lhs = lhs & rhs;
        return lhs;
    }

    inline ModifierKindFlags &operator^=(ModifierKindFlags &lhs, ModifierKindFlags rhs)
    {
        lhs = lhs ^ rhs;
        return lhs;
    }

    // Helper function to check if a specific flag is set
    inline bool has_flag(ModifierKindFlags flags, ModifierKindFlags flag)
    {
        return (flags & flag) == flag;
    }

    constexpr inline AccessModifierKind get_access_modifier(ModifierKindFlags flags)
    {
        if (has_flag(flags, ModifierKindFlags::Public))
            return AccessModifierKind::Public;
        else if (has_flag(flags, ModifierKindFlags::Private))
            return AccessModifierKind::Private;
        else if (has_flag(flags, ModifierKindFlags::Protected))
            return AccessModifierKind::Protected;
        else
            return AccessModifierKind::None;
    }

    constexpr inline std::string_view to_string(TokenKind kind)
    {
        switch (kind)
        {
        // Special tokens
        case TokenKind::None:
            return "none";
        case TokenKind::EndOfFile:
            return "end of file";
        case TokenKind::Invalid:
            return "invalid";

        // Literals
        case TokenKind::LiteralI32:
            return "i32";
        case TokenKind::LiteralF32:
            return "f32";
        case TokenKind::LiteralString:
            return "string";
        case TokenKind::LiteralChar:
            return "char";
        case TokenKind::LiteralBool:
            return "bool";
        case TokenKind::Null:
            return "null";
        case TokenKind::Void:
            return "void";

        // Identifiers and keywords
        case TokenKind::Identifier:
            return "identifier";

        // Declaration keywords
        case TokenKind::Type:
            return "type";
        case TokenKind::Enum:
            return "enum";
        case TokenKind::Var:
            return "var";
        case TokenKind::Fn:
            return "fn";

        // Function keywords
        case TokenKind::New:
            return "new";

        // Control flow keywords
        case TokenKind::If:
            return "if";
        case TokenKind::Else:
            return "else";
        case TokenKind::While:
            return "while";
        case TokenKind::For:
            return "for";
        case TokenKind::Match:
            return "match";
        case TokenKind::Break:
            return "break";
        case TokenKind::Continue:
            return "continue";
        case TokenKind::Return:
            return "return";
        case TokenKind::In:
            return "in";
        case TokenKind::At:
            return "at";

        // Property keywords
        case TokenKind::Get:
            return "get";
        case TokenKind::Set:
            return "set";

        // Modifier keywords
        case TokenKind::Public:
            return "public";
        case TokenKind::Private:
            return "private";
        case TokenKind::Protected:
            return "protected";
        case TokenKind::Static:
            return "static";
        case TokenKind::Virtual:
            return "virtual";
        case TokenKind::Override:
            return "override";
        case TokenKind::Abstract:
            return "abstract";
        case TokenKind::Extern:
            return "extern";
        case TokenKind::Ref:
            return "ref";

        // Other keywords
        case TokenKind::This:
            return "this";
        case TokenKind::Using:
            return "using";
        case TokenKind::Namespace:
            return "namespace";
        case TokenKind::Where:
            return "where";
        case TokenKind::By:
            return "by";

        // Operators - Arithmetic
        case TokenKind::Plus:
            return "+";
        case TokenKind::Minus:
            return "-";
        case TokenKind::Asterisk:
            return "*";
        case TokenKind::Slash:
            return "/";
        case TokenKind::Percent:
            return "%";

        // Operators - Assignment
        case TokenKind::Assign:
            return "=";
        case TokenKind::PlusAssign:
            return "+=";
        case TokenKind::MinusAssign:
            return "-=";
        case TokenKind::StarAssign:
            return "*=";
        case TokenKind::SlashAssign:
            return "/=";
        case TokenKind::PercentAssign:
            return "%=";
        case TokenKind::AndAssign:
            return "&=";
        case TokenKind::OrAssign:
            return "|=";
        case TokenKind::XorAssign:
            return "^=";
        case TokenKind::LeftShiftAssign:
            return "<<=";
        case TokenKind::RightShiftAssign:
            return ">>=";
        case TokenKind::NullCoalesceAssign:
            return "?\?=";

        // Operators - Comparison
        case TokenKind::Equal:
            return "==";
        case TokenKind::NotEqual:
            return "!=";
        case TokenKind::Less:
            return "<";
        case TokenKind::LessEqual:
            return "<=";
        case TokenKind::Greater:
            return ">";
        case TokenKind::GreaterEqual:
            return ">=";

        // Operators - Logical
        case TokenKind::And:
            return "&&";
        case TokenKind::Or:
            return "||";
        case TokenKind::Not:
            return "!";

        // Operators - Bitwise
        case TokenKind::BitwiseAnd:
            return "&";
        case TokenKind::BitwiseOr:
            return "|";
        case TokenKind::BitwiseXor:
            return "^";
        case TokenKind::BitwiseNot:
            return "~";
        case TokenKind::LeftShift:
            return "<<";
        case TokenKind::RightShift:
            return ">>";

        // Operators - Other
        case TokenKind::Question:
            return "?";
        case TokenKind::Colon:
            return ":";
        case TokenKind::FatArrow:
            return "=>";
        case TokenKind::ThinArrow:
            return "->";
        case TokenKind::Dot:
            return ".";
        case TokenKind::DotDot:
            return "..";
        case TokenKind::DotDotDot:
            return "...";
        case TokenKind::NullCoalesce:
            return "??";

        // Punctuation
        case TokenKind::LeftParen:
            return "(";
        case TokenKind::RightParen:
            return ")";
        case TokenKind::LeftBrace:
            return "{";
        case TokenKind::RightBrace:
            return "}";
        case TokenKind::LeftBracket:
            return "[";
        case TokenKind::RightBracket:
            return "]";
        case TokenKind::Semicolon:
            return ";";
        case TokenKind::Comma:
            return ",";
        }

        // This should never be reached if all cases are handled
        return "unknown token";
    }

    constexpr inline std::string_view to_string(TriviaKind kind)
    {
        auto name = magic_enum::enum_name(kind);
        return name.empty() ? "unknown trivia" : name;
    }

    constexpr inline std::string_view to_string(UnaryOperatorKind kind)
    {
        // just convert to TokenKind and reuse its to_string
        return to_string(static_cast<TokenKind>(kind));
    }

    constexpr inline std::string_view to_string(BinaryOperatorKind kind)
    {
        // Just convert to TokenKind and reuse its to_string
        return to_string(static_cast<TokenKind>(kind));
    }

    constexpr inline std::string_view to_string(AssignmentOperatorKind kind)
    {
        // Just convert to TokenKind and reuse its to_string
        return to_string(static_cast<TokenKind>(kind));
    }

    constexpr inline std::string_view to_string(LiteralKind kind)
    {
        // Just convert to TokenKind and reuse its to_string
        return to_string(static_cast<TokenKind>(kind));
    }

    constexpr inline std::string_view to_string(AccessModifierKind kind)
    {
        switch (kind)
        {
        case AccessModifierKind::Public:
            return "public";
        case AccessModifierKind::Private:
            return "private";
        case AccessModifierKind::Protected:
            return "protected";
        default:
            return "invalid";
        }
    }

    constexpr inline std::string_view to_string(SymbolKind kind)
    {
        switch (kind)
        {
        case SymbolKind::Namespace:
            return "namespace";
        case SymbolKind::Type:
            return "type";
        case SymbolKind::Function:
            return "fn";
        case SymbolKind::Variable:
            return "var";
        case SymbolKind::Property:
            return "prop";
        case SymbolKind::EnumCase:
            return "case";
        case SymbolKind::Block:
            return "block";
        default:
            return "unknown";
        }
    }

    inline std::string to_string(ModifierKindFlags flags)
    {
        if (flags == ModifierKindFlags::None)
            return "";
        if (flags == ModifierKindFlags::Invalid)
            return "invalid";

        std::string result;
        bool first = true;

        // Helper lambda to check and append flag
        auto check_and_append = [&](ModifierKindFlags flag, const char *name)
        {
            if (has_flag(flags, flag))
            {
                if (!first)
                    result += " ";
                result += name;
                first = false;
            }
        };

        // Check each flag
        check_and_append(ModifierKindFlags::Public, "public");
        check_and_append(ModifierKindFlags::Private, "private");
        check_and_append(ModifierKindFlags::Protected, "protected");
        check_and_append(ModifierKindFlags::Static, "static");
        check_and_append(ModifierKindFlags::Ref, "ref");
        check_and_append(ModifierKindFlags::Virtual, "virtual");
        check_and_append(ModifierKindFlags::Override, "override");
        check_and_append(ModifierKindFlags::Abstract, "abstract");
        check_and_append(ModifierKindFlags::Extern, "extern");

        return result.empty() ? "unknown modifier" : result;
    }


} // namespace Fern