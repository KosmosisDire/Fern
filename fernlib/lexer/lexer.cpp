#include "lexer.hpp"
#include <source/file.hpp>

namespace Fern
{

Lexer::Lexer(const SourceFile& file)
    : walker(file)
{
}

std::vector<Token> Lexer::tokenize()
{
    std::vector<Token> tokens;

    while (!walker.is_at_end())
    {
        Token token = scan_token();
        tokens.push_back(token);
    }

    if (tokens.empty() || tokens.back().kind != TokenKind::EndOfFile)
    {
        walker.mark_start();
        tokens.push_back(make_token(TokenKind::EndOfFile));
    }

    return tokens;
}

#pragma region Helpers

void Lexer::skip_whitespace_and_comments()
{
    while (true)
    {
        char c = walker.peek();
        switch (c)
        {
            case ' ':
            case '\t':
                walker.advance();
                break;
            case '-':
                if (walker.peek_next() == '-')
                {
                    walker.advance();
                    walker.advance();
                    if (walker.peek() == '-')
                    {
                        walker.advance();
                        while (!walker.is_at_end())
                        {
                            if (walker.peek() == '-' && walker.peek_next() == '-')
                            {
                                walker.advance();
                                walker.advance();
                                if (walker.peek() == '-')
                                {
                                    walker.advance();
                                    break;
                                }
                            }
                            else
                            {
                                walker.advance();
                            }
                        }
                    }
                    else
                    {
                        while (!walker.is_at_end() && walker.peek() != '\n')
                        {
                            walker.advance();
                        }
                    }
                }
                else
                {
                    return;
                }
                break;
            default:
                return;
        }
    }
}



#pragma region Token Creation

Token Lexer::make_token(TokenKind kind) const
{
    Token token;
    token.kind = kind;
    token.span = walker.make_span();
    token.lexeme = walker.lexeme();
    return token;
}

Token Lexer::make_error_token() const
{
    return make_token(TokenKind::Invalid);
}



#pragma region Scanning

Token Lexer::scan_token()
{
    skip_whitespace_and_comments();
    walker.mark_start();

    if (walker.is_at_end())
    {
        return make_token(TokenKind::EndOfFile);
    }

    char c = walker.peek();
    if (c == '\r' || c == '\n')
    {
        while (walker.peek() == '\r' || walker.peek() == '\n')
        {
            walker.advance();
        }
        return make_token(TokenKind::Newline);
    }

    c = walker.advance();

    if (is_alpha(c))
    {
        return scan_identifier();
    }
    if (is_digit(c))
    {
        return scan_number();
    }

    switch (c)
    {
        case '(':
            return make_token(TokenKind::LeftParen);
        case ')':
            return make_token(TokenKind::RightParen);
        case '{':
            return make_token(TokenKind::LeftBrace);
        case '}':
            return make_token(TokenKind::RightBrace);
        case ':':
            return make_token(TokenKind::Colon);
        case ';':
            return make_token(TokenKind::Semicolon);
        case ',':
            return make_token(TokenKind::Comma);
        case '+':
            return make_token(TokenKind::Plus);
        case '=':
            return make_token(TokenKind::Assign);
        case '-':
            if (walker.match('>'))
            {
                return make_token(TokenKind::ThinArrow);
            }
            return make_error_token();
        default:
            return make_error_token();
    }
}

Token Lexer::scan_identifier()
{
    while (is_alphanumeric(walker.peek()))
    {
        walker.advance();
    }

    std::string_view lexeme = walker.lexeme();

    TokenKind kind = TokenKind::Identifier;

    if (lexeme == "fn")
    {
        kind = TokenKind::Fn;
    }
    else if (lexeme == "var")
    {
        kind = TokenKind::Var;
    }
    if (lexeme == "type")
    {
        kind = TokenKind::Type;
    }
    else if (lexeme == "return")
    {
        kind = TokenKind::Return;
    }
    else if (lexeme == "f32")
    {
        kind = TokenKind::F32Keyword;
    }

    return make_token(kind);
}

Token Lexer::scan_number()
{
    while (is_digit(walker.peek()))
    {
        walker.advance();
    }

    if (walker.peek() == '.' && is_digit(walker.peek_next()))
    {
        walker.advance();
        while (is_digit(walker.peek()))
        {
            walker.advance();
        }
    }

    return make_token(TokenKind::LiteralF32);
}



#pragma region Character Class

bool Lexer::is_digit(char c)
{
    return c >= '0' && c <= '9';
}

bool Lexer::is_alpha(char c)
{
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           c == '_';
}

bool Lexer::is_alphanumeric(char c)
{
    return is_alpha(c) || is_digit(c);
}



} 
