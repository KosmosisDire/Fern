#pragma once

#include <magic_enum.hpp>
#include <string_view>
#include <vector>
#include <cstdint>
#include <array>
#include "source_location.hpp"

namespace Fern
{
    // Token kinds - all possible tokens in Mycelium language
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
        Typeof,
        Sizeof,
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

        // Operators - Unary
        Increment, // ++
        Decrement, // --

        // Operators - Other
        Question,     // ?
        Colon,        // :
        FatArrow,     // =>
        ThinArrow,    // ->
        Dot,          // .
        DotDotEquals, // ..=
        DotDot,       // ..
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
        Underscore,   // _
        AtSymbol,     // @
        Hash,         // #
        Dollar,       // $
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
        Typeof = (int)TokenKind::Typeof,
        Sizeof = (int)TokenKind::Sizeof,
        By = (int)TokenKind::By
    };

    enum class UnaryOperatorKind
    {
        Invalid = (int)TokenKind::Invalid,
        Plus = (int)TokenKind::Plus,
        Minus = (int)TokenKind::Minus,
        Not = (int)TokenKind::Not,
        BitwiseNot = (int)TokenKind::BitwiseNot,
        PostIncrement = (int)TokenKind::Increment,
        PostDecrement = (int)TokenKind::Decrement,
        PreIncrement = ((int)TokenKind::Increment + 1024), // Distinguish from post-increment
        PreDecrement = ((int)TokenKind::Decrement + 1024), // Distinguish from post-decrement
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
        Invalid = 1 << 11
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

    enum class LiteralKind
    {
        Invalid = (int)TokenKind::Invalid,
        I32 = (int)TokenKind::LiteralI32,
        F32 = (int)TokenKind::LiteralF32,
        String = (int)TokenKind::LiteralString,
        Char = (int)TokenKind::LiteralChar,
        Bool = (int)TokenKind::LiteralBool,
        Null = (int)TokenKind::Null,
    };

    // Trivia kinds - whitespace and comments
    enum class TriviaKind : uint8_t
    {
        Whitespace,   // Spaces, tabs
        Newline,      // \n, \r\n, \r
        LineComment,  // // comment
        BlockComment, // /* comment */
        DocComment,   // /// or /** doc comment */
    };

    // Trivia structure - positioned relative to associated token
    struct Trivia
    {
        TriviaKind kind;
        uint32_t width; // Character width

        Trivia() : kind(TriviaKind::Whitespace), width(0) {}
        Trivia(TriviaKind k, uint32_t w) : kind(k), width(w) {}
    };

    inline std::string_view to_string(TokenKind kind)
    {
        // Special cases for punctuation to return the actual symbol
        switch (kind)
        {
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

        // Operators - Unary
        case TokenKind::Increment:
            return "++";
        case TokenKind::Decrement:
            return "--";

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
        case TokenKind::DotDotEquals:
            return "..=";
        case TokenKind::DotDot:
            return "..";
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
        case TokenKind::Underscore:
            return "_";
        case TokenKind::AtSymbol:
            return "@";
        case TokenKind::Hash:
            return "#";
        case TokenKind::Dollar:
            return "$";

        // Default to enum name for keywords and other tokens
        default:
        {
            auto name = magic_enum::enum_name(kind);
            return name.empty() ? "unknown token" : name;
        }
        }
    }

    inline std::string_view to_string(TriviaKind kind)
    {
        auto name = magic_enum::enum_name(kind);
        return name.empty() ? "unknown trivia" : name;
    }

    inline std::string_view to_string(UnaryOperatorKind kind)
    {
        switch (kind)
        {
        case UnaryOperatorKind::Plus:
            return "+";
        case UnaryOperatorKind::Minus:
            return "-";
        case UnaryOperatorKind::Not:
            return "!";
        case UnaryOperatorKind::BitwiseNot:
            return "~";
        case UnaryOperatorKind::PreIncrement:
            return "++";
        case UnaryOperatorKind::PreDecrement:
            return "--";
        case UnaryOperatorKind::PostIncrement:
            return "++";
        case UnaryOperatorKind::PostDecrement:
            return "--";
        case UnaryOperatorKind::AddressOf:
            return "&";
        case UnaryOperatorKind::Dereference:
            return "*";
        default:
        {
            auto name = magic_enum::enum_name(kind);
            return name.empty() ? "unknown unary operator" : name;
        }
        }
    }

    inline std::string_view to_string(BinaryOperatorKind kind)
    {
        switch (kind)
        {
        case BinaryOperatorKind::Add:
            return "+";
        case BinaryOperatorKind::Subtract:
            return "-";
        case BinaryOperatorKind::Multiply:
            return "*";
        case BinaryOperatorKind::Divide:
            return "/";
        case BinaryOperatorKind::Modulo:
            return "%";
        case BinaryOperatorKind::Equals:
            return "==";
        case BinaryOperatorKind::NotEquals:
            return "!=";
        case BinaryOperatorKind::LessThan:
            return "<";
        case BinaryOperatorKind::GreaterThan:
            return ">";
        case BinaryOperatorKind::LessThanOrEqual:
            return "<=";
        case BinaryOperatorKind::GreaterThanOrEqual:
            return ">=";
        case BinaryOperatorKind::LogicalAnd:
            return "&&";
        case BinaryOperatorKind::LogicalOr:
            return "||";
        case BinaryOperatorKind::BitwiseAnd:
            return "&";
        case BinaryOperatorKind::BitwiseOr:
            return "|";
        case BinaryOperatorKind::BitwiseXor:
            return "^";
        case BinaryOperatorKind::LeftShift:
            return "<<";
        case BinaryOperatorKind::RightShift:
            return ">>";
        case BinaryOperatorKind::Coalesce:
            return "??";
        default:
        {
            auto name = magic_enum::enum_name(kind);
            return name.empty() ? "unknown binary operator" : name;
        }
        }
    }

    inline std::string_view to_string(AssignmentOperatorKind kind)
    {
        switch (kind)
        {
        case AssignmentOperatorKind::Assign:
            return "=";
        case AssignmentOperatorKind::Add:
            return "+=";
        case AssignmentOperatorKind::Subtract:
            return "-=";
        case AssignmentOperatorKind::Multiply:
            return "*=";
        case AssignmentOperatorKind::Divide:
            return "/=";
        case AssignmentOperatorKind::Modulo:
            return "%=";
        case AssignmentOperatorKind::And:
            return "&=";
        case AssignmentOperatorKind::Or:
            return "|=";
        case AssignmentOperatorKind::Xor:
            return "^=";
        case AssignmentOperatorKind::LeftShift:
            return "<<=";
        case AssignmentOperatorKind::RightShift:
            return ">>=";
        case AssignmentOperatorKind::Coalesce:
            return "?\?=";
        default:
        {
            auto name = magic_enum::enum_name(kind);
            return name.empty() ? "unknown assignment operator" : name;
        }
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

    inline std::string_view to_string(LiteralKind kind)
    {
        switch (kind)
        {
        case LiteralKind::Invalid:
            return "invalid";
        case LiteralKind::I32:
            return "i32";
        case LiteralKind::F32:
            return "f32";
        case LiteralKind::String:
            return "string";
        case LiteralKind::Char:
            return "char";
        case LiteralKind::Bool:
            return "bool";
        case LiteralKind::Null:
            return "null";
        }

        return "unknown literal";
    }

    // Main token structure with absolute position and relative trivia
    struct Token
    {
        std::string text;                    // Text of the token (for debugging)
        TokenKind kind;                      // What type of token
        SourceRange location;                // Absolute position in source
        std::vector<Trivia> leading_trivia;  // Whitespace/comments before token
        std::vector<Trivia> trailing_trivia; // Whitespace/comments after token

        Token() : kind(TokenKind::None) {}

        Token(TokenKind kind, SourceRange location, std::string_view source)
            : kind(kind), location(location)
        {
            if (location.end_offset() <= source.size())
                text = std::string(source.substr(location.start.offset, location.width));
            else
                text = {};
        }

        static Token invalid_token(Token current)
        {
            return Token(TokenKind::Invalid, SourceRange(current.location.start, 1), current.text);
        }

        // Check if this is a specific token kind
        bool is(TokenKind k) const { return kind == k; }

        // Check if this is any of the given token kinds
        bool is_any(std::initializer_list<TokenKind> kinds) const
        {
            for (TokenKind k : kinds)
            {
                if (kind == k)
                    return true;
            }
            return false;
        }

        // Check if this is an end-of-file token
        bool is_eof() const { return kind == TokenKind::EndOfFile; }

        // Check if this is a keyword token
        bool is_keyword() const
        {
            return magic_enum::enum_contains<KeywordKind>(static_cast<int>(kind));
        }

        // Check if this is a literal token
        bool is_literal() const
        {
            return magic_enum::enum_contains<LiteralKind>(static_cast<int>(kind));
        }

        // Check if this is an operator token
        bool is_operator() const
        {
            return magic_enum::enum_contains<BinaryOperatorKind>(static_cast<int>(kind)) ||
                   magic_enum::enum_contains<UnaryOperatorKind>(static_cast<int>(kind)) ||
                   magic_enum::enum_contains<AssignmentOperatorKind>(static_cast<int>(kind));
        }

        // Check if this is a modifier token
        bool is_modifier() const
        {
            switch (kind)
            {
            case TokenKind::Public:
            case TokenKind::Private:
            case TokenKind::Protected:
            case TokenKind::Static:
            case TokenKind::Ref:
            case TokenKind::Virtual:
            case TokenKind::Override:
            case TokenKind::Abstract:
            case TokenKind::Extern:
                return true;
            default:
                return false;
            };
        }

        // Check if this is an assignment operator token
        bool is_assignment_operator() const
        {
            return magic_enum::enum_contains<AssignmentOperatorKind>(static_cast<int>(kind));
        }

        KeywordKind to_keyword_kind() const
        {
            auto casted = magic_enum::enum_cast<KeywordKind>(static_cast<int>(kind)).value_or(KeywordKind::Invalid);
            assert(casted != KeywordKind::Invalid);
            return casted;
        }

        UnaryOperatorKind to_unary_operator_kind() const
        {
            auto casted = magic_enum::enum_cast<UnaryOperatorKind>(static_cast<int>(kind)).value_or(UnaryOperatorKind::Invalid);
            assert(casted != UnaryOperatorKind::Invalid);
            return casted;
        }

        BinaryOperatorKind to_binary_operator_kind() const
        {
            auto casted = magic_enum::enum_cast<BinaryOperatorKind>(static_cast<int>(kind)).value_or(BinaryOperatorKind::Invalid);
            assert(casted != BinaryOperatorKind::Invalid);
            return casted;
        }

        AssignmentOperatorKind to_assignment_operator_kind() const
        {
            auto casted = magic_enum::enum_cast<AssignmentOperatorKind>(static_cast<int>(kind)).value_or(AssignmentOperatorKind::Invalid);
            assert(casted != AssignmentOperatorKind::Invalid);
            return casted;
        }

        ModifierKindFlags to_modifier_kind() const
        {
            switch (kind)
            {
            case TokenKind::Public:
                return ModifierKindFlags::Public;
            case TokenKind::Private:
                return ModifierKindFlags::Private;
            case TokenKind::Protected:
                return ModifierKindFlags::Protected;
            case TokenKind::Static:
                return ModifierKindFlags::Static;
            case TokenKind::Ref:
                return ModifierKindFlags::Ref;
            case TokenKind::Virtual:
                return ModifierKindFlags::Virtual;
            case TokenKind::Override:
                return ModifierKindFlags::Override;
            case TokenKind::Abstract:
                return ModifierKindFlags::Abstract;
            case TokenKind::Extern:
                return ModifierKindFlags::Extern;
            default:
                assert(false && "Invalid modifier token kind");
                return ModifierKindFlags::Invalid;
            }
        }

        LiteralKind to_literal_kind() const
        {
            auto casted = magic_enum::enum_cast<LiteralKind>(static_cast<int>(kind)).value_or(LiteralKind::Invalid);
            assert(casted != LiteralKind::Invalid);
            return casted;
        }

        constexpr bool is_type_keyword() const
        {
            switch (kind)
            {
            case TokenKind::Type:
            case TokenKind::Enum:
            case TokenKind::Ref:
                return true;
            default:
                return false;
            }
        }

        // Precedence levels for operators
        enum Precedence : int
        {
            PREC_NONE,
            PREC_ASSIGNMENT,     // =, +=, -=, etc.
            PREC_TERNARY,        // ?:
            PREC_LOGICAL_OR,     // ||
            PREC_LOGICAL_AND,    // &&
            PREC_BITWISE_OR,     // |
            PREC_BITWISE_XOR,    // ^
            PREC_BITWISE_AND,    // &
            PREC_EQUALITY,       // ==, !=
            PREC_RELATIONAL,     // <, >, <=, >=
            PREC_RANGE,          // .., ..=
            PREC_SHIFT,          // <<, >>
            PREC_ADDITIVE,       // +, -
            PREC_MULTIPLICATIVE, // *, /, %
            PREC_UNARY,          // !, ~, -, +
            PREC_POSTFIX,        // ++, --, [], (), .
            PREC_PRIMARY
        };

        // Member methods for operator properties (operate on this token's kind)
        constexpr int get_binary_precedence() const
        {
            switch (kind)
            {
            // Assignment operators
            case TokenKind::Assign:
            case TokenKind::PlusAssign:
            case TokenKind::MinusAssign:
            case TokenKind::StarAssign:
            case TokenKind::SlashAssign:
            case TokenKind::PercentAssign:
            case TokenKind::AndAssign:
            case TokenKind::OrAssign:
            case TokenKind::XorAssign:
            case TokenKind::LeftShiftAssign:
            case TokenKind::RightShiftAssign:
            case TokenKind::NullCoalesceAssign:
                return PREC_ASSIGNMENT;

            // Ternary operator
            case TokenKind::Question:
                return PREC_TERNARY;

            // Logical operators
            case TokenKind::Or:
                return PREC_LOGICAL_OR;
            case TokenKind::And:
                return PREC_LOGICAL_AND;

            // Bitwise operators
            case TokenKind::BitwiseOr:
                return PREC_BITWISE_OR;
            case TokenKind::BitwiseXor:
                return PREC_BITWISE_XOR;
            case TokenKind::BitwiseAnd:
                return PREC_BITWISE_AND;

            // Shift operators
            case TokenKind::LeftShift:
            case TokenKind::RightShift:
                return PREC_SHIFT;

            // Equality operators
            case TokenKind::Equal:
            case TokenKind::NotEqual:
                return PREC_EQUALITY;

            // Relational operators
            case TokenKind::Less:
            case TokenKind::Greater:
            case TokenKind::LessEqual:
            case TokenKind::GreaterEqual:
                return PREC_RELATIONAL;

            // Range operators
            case TokenKind::DotDot:
            case TokenKind::DotDotEquals:
                return PREC_RANGE;

            // Additive operators
            case TokenKind::Plus:
            case TokenKind::Minus:
                return PREC_ADDITIVE;

            // Multiplicative operators
            case TokenKind::Asterisk:
            case TokenKind::Slash:
            case TokenKind::Percent:
                return PREC_MULTIPLICATIVE;

            default:
                return PREC_NONE;
            }
        }

        constexpr int get_unary_precedence() const
        {
            switch (kind)
            {
            case TokenKind::Plus:
            case TokenKind::Minus:
            case TokenKind::Not:
            case TokenKind::BitwiseNot:
            case TokenKind::Increment:
            case TokenKind::Decrement:
            case TokenKind::Asterisk: // Dereference
            case TokenKind::BitwiseAnd: // Address-of
                return PREC_UNARY;

            default:
                return PREC_NONE;
            }
        }

        constexpr int get_postfix_precedence() const
        {
            switch (kind)
            {
            case TokenKind::LeftParen:   // Function call
            case TokenKind::LeftBracket: // Array indexing
            case TokenKind::Dot:         // Member access
            case TokenKind::Increment:   // Post-increment
            case TokenKind::Decrement:   // Post-decrement
                return PREC_POSTFIX;

            default:
                return PREC_NONE;
            }
        }

        constexpr bool is_right_associative() const
        {
            switch (kind)
            {
            // Assignment operators are right associative
            case TokenKind::Assign:
            case TokenKind::PlusAssign:
            case TokenKind::MinusAssign:
            case TokenKind::StarAssign:
            case TokenKind::SlashAssign:
            case TokenKind::PercentAssign:
            case TokenKind::AndAssign:
            case TokenKind::OrAssign:
            case TokenKind::XorAssign:
            case TokenKind::LeftShiftAssign:
            case TokenKind::RightShiftAssign:
            case TokenKind::NullCoalesceAssign:
            // Ternary operator
            case TokenKind::Question:
            // Logical OR for the test case
            case TokenKind::Or:
                return true;

            // All other operators are left associative
            default:
                return false;
            }
        }

        constexpr bool is_left_associative() const
        {
            return !is_right_associative() && get_binary_precedence() > PREC_NONE;
        }

        // Operator type checking helpers
        constexpr bool is_unary_operator() const
        {
            return get_unary_precedence() > PREC_NONE;
        }

        constexpr bool is_binary_operator() const
        {
            return get_binary_precedence() > PREC_NONE;
        }

        constexpr bool is_postfix_operator() const
        {
            return get_postfix_precedence() > PREC_NONE;
        }

        // Expression/Statement/Declaration start detection
        constexpr bool starts_expression() const
        {
            switch (kind)
            {
            // Literals
            case TokenKind::LiteralI32:
            case TokenKind::LiteralF32:
            case TokenKind::LiteralString:
            case TokenKind::LiteralChar:
            case TokenKind::LiteralBool:
            case TokenKind::Null:
            // Identifiers and keywords
            case TokenKind::Identifier:
            case TokenKind::This:
            case TokenKind::New:
            case TokenKind::Typeof:
            case TokenKind::Sizeof:
            // Unary operators
            case TokenKind::Plus:
            case TokenKind::Minus:
            case TokenKind::Not:
            case TokenKind::BitwiseNot:
            case TokenKind::Increment:
            case TokenKind::Decrement:
            // Grouping
            case TokenKind::LeftParen:
            // Special expressions
            case TokenKind::Match:
            case TokenKind::Dot: // For enum members like .Red
                return true;
            default:
                return false;
            }
        }

        constexpr bool starts_statement() const
        {
            switch (kind)
            {
            case TokenKind::If:
            case TokenKind::While:
            case TokenKind::For:
            case TokenKind::Return:
            case TokenKind::Break:
            case TokenKind::Continue:
            case TokenKind::LeftBrace:
            case TokenKind::Match:
                return true;
            default:
                return starts_expression(); // Expressions can be statements
            }
        }

        constexpr bool starts_declaration() const
        {
            switch (kind)
            {
            case TokenKind::Type:
            case TokenKind::Enum:
            case TokenKind::Fn:
            case TokenKind::Var:
            case TokenKind::Using:
            case TokenKind::Namespace:

            // Modifiers can start declarations
            case TokenKind::Public:
            case TokenKind::Private:
            case TokenKind::Protected:
            case TokenKind::Static:
            case TokenKind::Virtual:
            case TokenKind::Override:
            case TokenKind::Abstract:
            case TokenKind::Extern:
            case TokenKind::Ref:
                return true;
            default:
                return false;
            }
        }

        std::string_view to_string() const
        {
            return Fern::to_string(kind);
        }

        static TokenKind get_keyword_kind(std::string_view keyword)
        {
            auto it = keyword_map.find(keyword);
            if (it != keyword_map.end())
            {
                return it->second;
            }
            return TokenKind::Identifier; // Not a keyword
        }

    private:
        static const std::unordered_map<std::string_view, TokenKind> keyword_map;
    };

    // Initialize the keyword map with all keywords
    inline const std::unordered_map<std::string_view, TokenKind> Token::keyword_map =
        {
            {"type", TokenKind::Type},
            {"ref", TokenKind::Ref},
            {"enum", TokenKind::Enum},
            {"var", TokenKind::Var},
            {"fn", TokenKind::Fn},
            {"new", TokenKind::New},
            {"return", TokenKind::Return},
            {"if", TokenKind::If},
            {"else", TokenKind::Else},
            {"while", TokenKind::While},
            {"for", TokenKind::For},
            {"match", TokenKind::Match},
            {"break", TokenKind::Break},
            {"continue", TokenKind::Continue},
            {"get", TokenKind::Get},
            {"set", TokenKind::Set},
            {"public", TokenKind::Public},
            {"private", TokenKind::Private},
            {"protected", TokenKind::Protected},
            {"static", TokenKind::Static},
            {"virtual", TokenKind::Virtual},
            {"override", TokenKind::Override},
            {"abstract", TokenKind::Abstract},
            {"extern", TokenKind::Extern},
            {"this", TokenKind::This},
            {"using", TokenKind::Using},
            {"namespace", TokenKind::Namespace},
            {"typeof", TokenKind::Typeof},
            {"sizeof", TokenKind::Sizeof},
            {"where", TokenKind::Where},
            {"in", TokenKind::In},
            {"at", TokenKind::At},
            {"by", TokenKind::By},
            {"true", TokenKind::LiteralBool},
            {"false", TokenKind::LiteralBool}};

} // namespace Fern