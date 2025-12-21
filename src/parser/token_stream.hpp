#pragma once

#include "common/token.hpp"
#include <initializer_list>
#include <string>
#include <vector>

namespace Fern
{
    class TokenStream
    {
    public:
        TokenStream(std::vector<Token> tokens);

        // Navigation
        const Token &current() const;
        const Token &peek(int offset = 1) const;
        const Token &previous() const;
        void advance();
        bool at_end() const;

        // Checking
        bool check(TokenKind kind) const;
        bool check_any(std::initializer_list<TokenKind> kinds) const;
        bool check_sequence(std::initializer_list<TokenKind> sequence) const;

        // Consumption
        bool consume(TokenKind kind);
        bool consume_any(std::initializer_list<TokenKind> kinds);
        TokenKind consume_any_get(std::initializer_list<TokenKind> kinds);

        // Recovery
        void skip_to(TokenKind kind);
        void skip_to_any(std::initializer_list<TokenKind> kinds);
        void skip_past(TokenKind kind);

        // Checkpointing
        struct Checkpoint { size_t position; };
        Checkpoint checkpoint() const;
        void restore(Checkpoint cp);

        // Utility
        void split_right_shift();
        SourceRange location() const;
        size_t position() const;
        size_t size() const;

        std::string dump() const;

    private:
        std::vector<Token> tokens_;
        size_t position_;

        void ensure_valid_position() const;
    };

} // namespace Fern