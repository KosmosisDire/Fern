#pragma once

#include "token.hpp"
#include <vector>
#include <string_view>

namespace Fern
{

class TokenWalker
{
public:
    explicit TokenWalker(const std::vector<Token>& tokens);

    bool is_at_end() const;
    size_t position() const;

    const Token& current() const;
    const Token& peek(size_t offset = 0) const;
    const Token& advance();

    bool check(TokenKind kind) const;
    bool match(TokenKind kind);
    const Token& expect(TokenKind kind, std::string_view message);

    struct Checkpoint
    {
        size_t position;
    };

    Checkpoint checkpoint() const;
    void restore(Checkpoint cp);

    std::string format() const;

private:
    const std::vector<Token>& tokens;
    size_t tokenIndex = 0;
};

}
