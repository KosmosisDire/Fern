#include "walker.hpp"
#include <table_builder.hpp>
#include <stdexcept>

namespace Fern
{

TokenWalker::TokenWalker(const std::vector<Token>& tokens)
    : tokens(tokens)
{
}

#pragma region Position

bool TokenWalker::is_at_end() const
{
    return tokenIndex >= tokens.size() || tokens[tokenIndex].kind == TokenKind::EndOfFile;
}

size_t TokenWalker::position() const
{
    return tokenIndex;
}



#pragma region Access

const Token& TokenWalker::current() const
{
    return tokens[tokenIndex];
}

const Token& TokenWalker::peek(size_t offset) const
{
    size_t index = tokenIndex + offset;
    if (index >= tokens.size())
    {
        return tokens.back();
    }
    return tokens[index];
}



#pragma region Matching

bool TokenWalker::check(TokenKind kind) const
{
    return current().kind == kind;
}

bool TokenWalker::match(TokenKind kind)
{
    if (check(kind))
    {
        advance();
        return true;
    }
    return false;
}

const Token& TokenWalker::expect(TokenKind kind, std::string_view message)
{
    if (check(kind))
    {
        return advance();
    }
    throw std::runtime_error(std::string(message));
}



#pragma region Checkpointing

const Token& TokenWalker::advance()
{
    if (!is_at_end())
    {
        tokenIndex++;
    }
    return tokens[tokenIndex - 1];
}

TokenWalker::Checkpoint TokenWalker::checkpoint() const
{
    return Checkpoint{tokenIndex};
}

void TokenWalker::restore(Checkpoint cp)
{
    tokenIndex = cp.position;
}



#pragma region Formatable

std::string TokenWalker::format() const
{
    TableBuilder table;

    table.add_row({"#", "Kind", "Lexeme", "Span"});

    for (size_t i = 0; i < tokens.size(); ++i)
    {
        const Token& token = tokens[i];
        std::string lexemeStr = token.kind == TokenKind::Newline
            ? "\\n"
            : std::string(token.lexeme);
        table.add_row({
            std::to_string(i),
            std::string(to_string(token.kind)),
            "'" + lexemeStr + "'",
            token.span.format()
        });
    }

    return table.build();
}



} 
