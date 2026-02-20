#pragma once

#include "token.hpp"
#include <span>
#include <string_view>

namespace Fern
{

class TokenWalker
{
public:
    explicit TokenWalker(std::span<const Token> tokens);

    bool is_at_end() const;
    size_t position() const;

    const Token& current() const;
    const Token& peek(size_t offset = 0) const;
    const Token& advance();

    bool check(TokenKind kind) const;
    bool match(TokenKind kind);

    struct Checkpoint
    {
        size_t position;
    };

    Checkpoint checkpoint() const;
    void restore(Checkpoint cp);
    bool check_progress(Checkpoint cp);
    bool synchronize_to(TokenKind target);

    std::string format() const;

private:
    std::span<const Token> tokens;
    size_t tokenIndex = 0;
};

}
