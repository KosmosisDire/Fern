#pragma once

#include <magic_enum.hpp>
#include <string_view>
#include <vector>
#include <cstdint>
#include <array>
#include "source_location.hpp"
#include "token_kind.hpp"

namespace Fern
{
    // Trivia structure - positioned relative to associated token
    struct Trivia
    {
        TriviaKind kind;
        uint32_t width; // Character width

        Trivia() : kind(TriviaKind::Whitespace), width(0) {}
        Trivia(TriviaKind k, uint32_t w) : kind(k), width(w) {}
    };

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
            case TokenKind::Enum:
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

        constexpr inline ModifierKindFlags to_modifier_kind() const
        {
            switch (kind)
            {
            case TokenKind::Invalid:
                return ModifierKindFlags::Invalid;
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
            case TokenKind::Enum:
                return ModifierKindFlags::Enum;
            default:
                assert(false && "Token is not a modifier");
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
            case TokenKind::DotDotDot:
            case TokenKind::DotDot:
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
            // Unary operators
            case TokenKind::Plus:
            case TokenKind::Minus:
            case TokenKind::Not:
            case TokenKind::BitwiseNot:
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
            {"where", TokenKind::Where},
            {"in", TokenKind::In},
            {"at", TokenKind::At},
            {"by", TokenKind::By},
            {"true", TokenKind::LiteralBool},
            {"false", TokenKind::LiteralBool},
            {"null", TokenKind::Null}};

} // namespace Fern