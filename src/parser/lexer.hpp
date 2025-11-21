#pragma once

#include "common/token.hpp"
#include "common/error.hpp"
#include <string_view>
#include <memory>

namespace Fern
{
    class TokenStream;
    enum class LexicalContext
    {
        Normal,
        StringLiteral,
        CharLiteral,
        LineComment,
        BlockComment,
    };

    // Main lexer class - converts source text to token stream
    class Lexer : public DiagnosticSystem
    {
    public:
        // Constructor
        Lexer(std::string_view source);

        // Destructor
        ~Lexer() = default;

        // Non-copyable but movable
        Lexer(const Lexer &) = delete;
        Lexer &operator=(const Lexer &) = delete;
        Lexer(Lexer &&) = default;
        Lexer &operator=(Lexer &&) = default;

        // Tokenize entire source and return token stream
        TokenStream tokenize_all();

        // Position and state queries
        SourceLocation current_location() const { return current_location_; }
        bool at_end() const { return current_offset_ >= source_.size(); }

        // Get source text view
        std::string_view source() const { return source_; }

    private:
        // Core tokenization methods (now private - used internally by tokenize_all)
        Token next_token();               // Get next token and advance
        Token peek_token(int offset = 0); // Look ahead without advancing

        // Internal state management
        size_t remaining_chars() const { return source_.size() - current_offset_; }
        void push_context(LexicalContext context);
        void pop_context();
        LexicalContext current_context() const;
        void reset();

        // Source text and position tracking
        std::string_view source_;
        size_t current_offset_;
        SourceLocation current_location_;

        // Context stack for nested constructs
        std::vector<LexicalContext> context_stack_;

        // Token lookahead cache for peek operations
        mutable std::vector<Token> token_cache_;
        mutable size_t cache_start_offset_;

        // Character access and advancement
        char current_char() const;
        char peek_char(int offset = 1) const;
        void advance_char();
        void advance_chars(size_t count);

        // Position tracking helpers
        void update_location(char ch);
        void update_location_bulk(std::string_view text);

        // Core tokenization methods
        Token scan_token();
        Token make_token(TokenKind kind, uint32_t width);
        Token make_invalid_token(const std::string &error_message);

        // Trivia scanning
        std::vector<Trivia> scan_leading_trivia();
        std::vector<Trivia> scan_trailing_trivia();
        Trivia scan_whitespace();
        Trivia scan_newline();
        Trivia scan_line_comment();
        Trivia scan_block_comment();

        // Literal scanning
        Token scan_number();
        Token scan_string_literal();
        Token scan_char_literal();

        // Identifier and keyword scanning
        Token scan_identifier_or_keyword();

        // Operator and punctuation scanning
        Token scan_operator_or_punctuation();

        // Character classification helpers
        bool is_whitespace(char ch) const;
        bool is_newline(char ch) const;
        bool is_alpha(char ch) const;
        bool is_digit(char ch) const;
        bool is_alnum(char ch) const;
        bool is_identifier_start(char ch) const;
        bool is_identifier_continue(char ch) const;
        bool is_hex_digit(char ch) const;
        bool is_octal_digit(char ch) const;
        bool is_binary_digit(char ch) const;

        // Escape sequence processing
        char interpret_escape_sequence();
    };

} // namespace Fern