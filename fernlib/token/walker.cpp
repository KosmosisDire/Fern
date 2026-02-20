#include "walker.hpp"
#include <table_builder.hpp>

namespace Fern
{

TokenWalker::TokenWalker(std::span<const Token> tokens)
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

bool TokenWalker::check_progress(Checkpoint cp)
{
    if (tokenIndex == cp.position)
    {
        advance();
        return false;
    }
    return true;
}

bool TokenWalker::synchronize_to(TokenKind target)
{
    int parenDepth = 0;
    int braceDepth = 0;

    while (!is_at_end())
    {
        TokenKind kind = current().kind;

        if (kind == target && parenDepth == 0 && braceDepth == 0)
        {
            advance();
            return true;
        }

        switch (kind)
        {
            case TokenKind::LeftParen:
                parenDepth++;
                break;
            case TokenKind::RightParen:
                if (parenDepth > 0)
                {
                    parenDepth--;
                }
                else
                {
                    return false;
                }
                break;
            case TokenKind::LeftBrace:
                braceDepth++;
                break;
            case TokenKind::RightBrace:
                if (braceDepth > 0)
                {
                    braceDepth--;
                }
                else
                {
                    return false;
                }
                break;
            default:
                break;
        }

        advance();
    }

    return false;
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
            std::string(Fern::format(token.kind)),
            "'" + lexemeStr + "'",
            token.span.format()
        });
    }

    return table.build();
}



} 
