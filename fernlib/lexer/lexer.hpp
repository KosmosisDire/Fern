#pragma once

#include "token/token.hpp"
#include "source/walker.hpp"
#include <vector>

namespace Fern
{

class SourceFile;

class Lexer
{
public:
    explicit Lexer(const SourceFile& file);

    std::vector<Token> tokenize();

private:
    #pragma region Helpers

    void skip_whitespace_and_comments();

    #pragma region Token Creation

    Token make_token(TokenKind kind) const;
    Token make_error_token() const;

    #pragma region Scanning

    Token scan_token();
    Token scan_identifier();
    Token scan_number();

    #pragma region Character Classification

    static bool is_digit(char c);
    static bool is_alpha(char c);
    static bool is_alphanumeric(char c);

    SourceWalker walker;
};

} 
