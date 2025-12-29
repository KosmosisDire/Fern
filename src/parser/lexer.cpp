#include "parser/lexer.hpp"
#include "parser/token_stream.hpp"
#include <unordered_map>
#include <cctype>
#include <algorithm>

#define TAB_SIZE 4

namespace Fern
{

    Lexer::Lexer(std::string_view source, int file_id):
        DiagnosticSystem("Lexer"), source_(source), file_id_(file_id),
        current_offset_(0), current_location_(file_id, 0, 1, 1), cache_start_offset_(0)
    {
        context_stack_.push_back(LexicalContext::Normal);
    }

    Token Lexer::next_token()
    {
        // If we have cached tokens and we're at the start of the cache, use the first cached token
        if (!token_cache_.empty() && cache_start_offset_ == current_offset_)
        {
            Token token = std::move(token_cache_.front());
            token_cache_.erase(token_cache_.begin());

            // Advance position
            current_offset_ = token.location.end_offset();
            current_location_ = token.location.end();

            // Update cache start
            cache_start_offset_ = current_offset_;

            return token;
        }

        // Clear cache if we're not aligned
        if (cache_start_offset_ != current_offset_)
        {
            token_cache_.clear();
            cache_start_offset_ = current_offset_;
        }

        return scan_token();
    }

    Token Lexer::peek_token(int offset)
    {
        // Clear cache if we're not aligned
        if (cache_start_offset_ != current_offset_)
        {
            token_cache_.clear();
            cache_start_offset_ = current_offset_;
        }

        // Ensure we have enough tokens cached
        while (static_cast<int>(token_cache_.size()) <= offset)
        {
            // Calculate the position where we should scan the next token
            size_t scan_pos = cache_start_offset_;
            for (const auto &cached_token : token_cache_)
            {
                scan_pos = cached_token.location.start.offset + cached_token.location.width;
            }

            if (scan_pos >= source_.size())
            {
                // Return EOF if we're past the end
                return Token(TokenKind::EndOfFile, SourceRange(SourceLocation(file_id_, scan_pos, 1, 1), 0), source_);
            }

            // Temporarily set position to scan position
            size_t saved_offset = current_offset_;
            SourceLocation saved_location = current_location_;

            current_offset_ = scan_pos;
            // For simplicity, use the offset to calculate location (not fully accurate for line/column)
            current_location_ = SourceLocation(file_id_, current_offset_, 1, current_offset_ + 1);

            Token token = scan_token();
            token_cache_.push_back(token);

            // Restore position
            current_offset_ = saved_offset;
            current_location_ = saved_location;
        }

        return token_cache_[offset];
    }

    void Lexer::push_context(LexicalContext context)
    {
        context_stack_.push_back(context);
    }

    void Lexer::pop_context()
    {
        if (context_stack_.size() > 1)
        {
            context_stack_.pop_back();
        }
    }

    LexicalContext Lexer::current_context() const
    {
        return context_stack_.back();
    }

    void Lexer::reset()
    {
        current_offset_ = 0;
        current_location_ = SourceLocation(file_id_, 0, 1, 1);
        context_stack_.clear();
        context_stack_.push_back(LexicalContext::Normal);
        token_cache_.clear();
        cache_start_offset_ = 0;
    }

    char Lexer::current_char() const
    {
        if (current_offset_ >= source_.size())
        {
            return '\0';
        }
        return source_[current_offset_];
    }

    char Lexer::peek_char(int offset) const
    {
        size_t pos = current_offset_ + offset;
        if (pos >= source_.size())
        {
            return '\0';
        }
        return source_[pos];
    }

    void Lexer::advance_char()
    {
        if (current_offset_ < source_.size())
        {
            update_location(source_[current_offset_]);
            current_offset_++;
        }
    }

    void Lexer::advance_chars(size_t count)
    {
        for (size_t i = 0; i < count && current_offset_ < source_.size(); ++i)
        {
            advance_char();
        }
    }

    void Lexer::update_location(char ch)
    {
        if (ch == '\n')
        {
            current_location_.line++;
            current_location_.column = 1;
        }
        else if (ch == '\t')
        {
            current_location_.column += TAB_SIZE - ((current_location_.column - 1) % TAB_SIZE);
        }
        else
        {
            current_location_.column++;
        }
        current_location_.offset++;
    }

    void Lexer::update_location_bulk(std::string_view text)
    {
        for (char ch : text)
        {
            if (ch == '\n')
            {
                current_location_.line++;
                current_location_.column = 1;
            }
            else if (ch == '\t')
            {
                current_location_.column += TAB_SIZE - ((current_location_.column - 1) % TAB_SIZE);
            }
            else
            {
                current_location_.column++;
            }
        }
        current_location_.offset += text.size();
    }

    Token Lexer::scan_token()
    {
        // Skip leading trivia
        std::vector<Trivia> leading_trivia;
        leading_trivia = scan_leading_trivia();

        // Check for end of file
        if (at_end())
        {
            Token token(TokenKind::EndOfFile, SourceRange(current_location_, 0), source_);
            token.leading_trivia = std::move(leading_trivia);
            return token;
        }

        SourceLocation token_start = current_location_;
        char ch = current_char();

        Token token;

        // Determine token type based on first character
        // TODO: Implement smarter identifier checking (ex __hello__). Right now it only supports one leading underscore _
        if (is_alpha(ch) || (ch == '_' && is_alpha(peek_char())))
        {
            token = scan_identifier_or_keyword();
        }
        else if (is_digit(ch))
        {
            token = scan_number();
        }
        else if (ch == '"')
        {
            token = scan_string_literal();
        }
        else if (ch == '\'')
        {
            token = scan_char_literal();
        }
        else
        {
            token = scan_operator_or_punctuation();
        }

        // Add leading trivia
        token.leading_trivia = std::move(leading_trivia);
        token.trailing_trivia = scan_trailing_trivia();
        
        return token;
    }

    Token Lexer::make_token(TokenKind kind, uint32_t width)
    {
        Token token(kind, SourceRange(current_location_, width), source_);
        advance_chars(width);
        return token;
    }

    Token Lexer::make_invalid_token(const std::string &error_message)
    {
        Token token(TokenKind::Invalid, SourceRange(current_location_, 1), source_);
        error(error_message, SourceRange(current_location_, 1));
        advance_char();
        return token;
    }

    std::vector<Trivia> Lexer::scan_leading_trivia()
    {
        std::vector<Trivia> trivia;

        while (!at_end())
        {
            char ch = current_char();

            if (is_whitespace(ch))
            {
                trivia.push_back(scan_whitespace());
            }
            else if (is_newline(ch))
            {
                trivia.push_back(scan_newline());
            }
            else if (ch == '-' && peek_char() == '-')
            {
                trivia.push_back(scan_line_comment());
            }
            else if (ch == '-' && peek_char() == '-' && peek_char(2) == '-')
            {
                trivia.push_back(scan_block_comment());
            }
            else
            {
                break;
            }
        }

        return trivia;
    }

    std::vector<Trivia> Lexer::scan_trailing_trivia()
    {
        std::vector<Trivia> trivia;

        // Only scan whitespace and comments on the same line for trailing trivia
        while (!at_end())
        {
            char ch = current_char();

            if (ch == ' ' || ch == '\t')
            {
                trivia.push_back(scan_whitespace());
            }
            else if (ch == '/' && peek_char() == '/')
            {
                trivia.push_back(scan_line_comment());
                break; // Line comment ends the line
            }
            else if (ch == '/' && peek_char() == '*')
            {
                trivia.push_back(scan_block_comment());
            }
            else
            {
                break;
            }
        }

        return trivia;
    }

    Trivia Lexer::scan_whitespace()
    {
        size_t start = current_offset_;

        while (!at_end() && is_whitespace(current_char()) && !is_newline(current_char()))
        {
            advance_char();
        }

        return Trivia(TriviaKind::Whitespace, current_offset_ - start);
    }

    Trivia Lexer::scan_newline()
    {
        size_t start = current_offset_;

        if (current_char() == '\r')
        {
            advance_char();
            if (!at_end() && current_char() == '\n')
            {
                advance_char();
            }
        }
        else if (current_char() == '\n')
        {
            advance_char();
        }

        return Trivia(TriviaKind::Newline, current_offset_ - start);
    }

    Trivia Lexer::scan_line_comment()
    {
        size_t start = current_offset_;

        // Skip "//"
        advance_chars(2);

        // Read until end of line
        while (!at_end() && !is_newline(current_char()))
        {
            advance_char();
        }

        return Trivia(TriviaKind::LineComment, current_offset_ - start);
    }

    Trivia Lexer::scan_block_comment()
    {
        size_t start = current_offset_;

        // Skip "---"
        advance_chars(3);

        // Read until "---"
        while (!at_end())
        {
            if (current_char() == '-' && peek_char() == '-' && peek_char(2) == '-')
            {
                advance_chars(3);
                break;
            }
            advance_char();
        }

        return Trivia(TriviaKind::BlockComment, current_offset_ - start);
    }

    Token Lexer::scan_number()
    {
        size_t start = current_offset_;
        SourceLocation start_location = current_location_;

        // Handle different number formats
        if (current_char() == '0')
        {
            char next = peek_char();
            if (next == 'x' || next == 'X')
            {
                // Hexadecimal
                advance_chars(2);
                while (!at_end() && is_hex_digit(current_char()))
                {
                    advance_char();
                }
            }
            else if (next == 'b' || next == 'B')
            {
                // Binary
                advance_chars(2);
                while (!at_end() && is_binary_digit(current_char()))
                {
                    advance_char();
                }
            }
            else if (is_octal_digit(next))
            {
                // Octal
                advance_char();
                while (!at_end() && is_octal_digit(current_char()))
                {
                    advance_char();
                }
            }
            else
            {
                // Decimal starting with 0
                advance_char();
            }
        }
        else
        {
            // Regular decimal
            while (!at_end() && is_digit(current_char()))
            {
                advance_char();
            }
        }

        bool is_float = false;

        // Check for floating point
        if (!at_end() && current_char() == '.' && is_digit(peek_char()))
        {
            is_float = true;
            advance_char(); // Skip '.'
            while (!at_end() && is_digit(current_char()))
            {
                advance_char();
            }

            // Check for exponent
            if (!at_end() && (current_char() == 'e' || current_char() == 'E'))
            {
                advance_char();
                if (!at_end() && (current_char() == '+' || current_char() == '-'))
                {
                    advance_char();
                }
                while (!at_end() && is_digit(current_char()))
                {
                    advance_char();
                }
            }
        }

        // Save where numeric part ends (before suffix)
        size_t numeric_end = current_offset_;

        // Check for type suffix (i8, u8, i16, u16, i32, u32, i64, u64, f16, f32, f64)
        TokenKind literal_kind = is_float ? TokenKind::LiteralF32 : TokenKind::LiteralI32;

        if (!at_end() && (current_char() == 'i' || current_char() == 'u' || current_char() == 'f'))
        {
            char suffix_start = current_char();
            advance_char();

            // Parse the bit width
            std::string bit_width_str;
            while (!at_end() && is_digit(current_char()))
            {
                bit_width_str += current_char();
                advance_char();
            }

            if (!bit_width_str.empty())
            {
                if (suffix_start == 'i')
                {
                    if (bit_width_str == "8") literal_kind = TokenKind::LiteralI8;
                    else if (bit_width_str == "16") literal_kind = TokenKind::LiteralI16;
                    else if (bit_width_str == "32") literal_kind = TokenKind::LiteralI32;
                    else if (bit_width_str == "64") literal_kind = TokenKind::LiteralI64;
                }
                else if (suffix_start == 'u')
                {
                    if (bit_width_str == "8") literal_kind = TokenKind::LiteralU8;
                    else if (bit_width_str == "16") literal_kind = TokenKind::LiteralU16;
                    else if (bit_width_str == "32") literal_kind = TokenKind::LiteralU32;
                    else if (bit_width_str == "64") literal_kind = TokenKind::LiteralU64;
                }
                else if (suffix_start == 'f')
                {
                    if (bit_width_str == "16") literal_kind = TokenKind::LiteralF16;
                    else if (bit_width_str == "32") literal_kind = TokenKind::LiteralF32;
                    else if (bit_width_str == "64") literal_kind = TokenKind::LiteralF64;
                }
            }
        }

        return Token(literal_kind, SourceRange(start_location, numeric_end - start), source_);
    }

    Token Lexer::scan_string_literal()
    {
        size_t start = current_offset_;
        SourceLocation start_location = current_location_;

        // Skip opening quote
        advance_char();

        std::string processed_string;
        
        while (!at_end() && current_char() != '"')
        {
            if (current_char() == '\\')
            {
                // Process escape sequence and add the interpreted character
                char escaped_char = interpret_escape_sequence();
                processed_string += escaped_char;
            }
            else if (current_char() == '\n')
            {
                error("Unterminated string literal", SourceRange(start_location, current_offset_ - start));
                break;
            }
            else
            {
                processed_string += current_char();
                advance_char();
            }
        }

        if (!at_end() && current_char() == '"')
        {
            advance_char(); // Skip closing quote
        }
        else
        {
            error("Unterminated string literal", SourceRange(start_location, current_offset_ - start));
        }

        Token token(TokenKind::LiteralString, SourceRange(start_location, current_offset_ - start), source_);
        token.text = std::move(processed_string);
        return token;
    }

    Token Lexer::scan_char_literal()
    {
        size_t start = current_offset_;
        SourceLocation start_location = current_location_;

        // Skip opening quote
        advance_char();

        std::string processed_char;
        
        if (!at_end() && current_char() != '\'')
        {
            if (current_char() == '\\')
            {
                // Process escape sequence and add the interpreted character
                char escaped_char = interpret_escape_sequence();
                processed_char += escaped_char;
            }
            else
            {
                processed_char += current_char();
                advance_char();
            }
        }
        else
        {
            error("Empty character literal", SourceRange(start_location, current_offset_ - start));
        }

        // Check for additional characters (invalid)
        if (!at_end() && current_char() != '\'')
        {
            error("Character literal contains multiple characters", SourceRange(start_location, current_offset_ - start));
            // Skip to closing quote or end
            while (!at_end() && current_char() != '\'')
            {
                advance_char();
            }
        }

        if (!at_end() && current_char() == '\'')
        {
            advance_char(); // Skip closing quote
        }
        else
        {
            error("Unterminated character literal", SourceRange(start_location, current_offset_ - start));
        }

        Token token(TokenKind::LiteralChar, SourceRange(start_location, current_offset_ - start), source_);
        token.text = std::move(processed_char);
        return token;
    }

    Token Lexer::scan_identifier_or_keyword()
    {
        size_t start = current_offset_;
        SourceLocation start_location = current_location_;

        // Read identifier characters
        while (!at_end() && is_identifier_continue(current_char()))
        {
            advance_char();
        }

        std::string_view text = source_.substr(start, current_offset_ - start);
        TokenKind kind = Token::get_keyword_kind(text);

        return Token(kind, SourceRange(start_location, current_offset_ - start), source_);
    }

    Token Lexer::scan_operator_or_punctuation()
    {
        char ch = current_char();
        SourceLocation start_location = current_location_;

        switch (ch)
        {
        case '+':
            if (peek_char() == '=')
                return make_token(TokenKind::PlusAssign, 2);
            return make_token(TokenKind::Plus, 1);

        case '-':
            if (peek_char() == '=')
                return make_token(TokenKind::MinusAssign, 2);
            if (peek_char() == '>')
                return make_token(TokenKind::ThinArrow, 2);
            return make_token(TokenKind::Minus, 1);

        case '*':
            if (peek_char() == '=')
                return make_token(TokenKind::StarAssign, 2);
            return make_token(TokenKind::Asterisk, 1);

        case '/':
            if (peek_char() == '=')
                return make_token(TokenKind::SlashAssign, 2);
            return make_token(TokenKind::Slash, 1);

        case '%':
            if (peek_char() == '=')
                return make_token(TokenKind::PercentAssign, 2);
            return make_token(TokenKind::Percent, 1);

        case '=':
            if (peek_char() == '=')
                return make_token(TokenKind::Equal, 2);
            if (peek_char() == '>')
                return make_token(TokenKind::FatArrow, 2);
            return make_token(TokenKind::Assign, 1);

        case '!':
            if (peek_char() == '=')
                return make_token(TokenKind::NotEqual, 2);
            return make_token(TokenKind::Not, 1);

        case '<':
            if (peek_char() == '=')
                return make_token(TokenKind::LessEqual, 2);
            if (peek_char() == '<')
                return make_token(TokenKind::LeftShift, 2);
            return make_token(TokenKind::Less, 1);

        case '>':
            if (peek_char() == '=')
                return make_token(TokenKind::GreaterEqual, 2);
            if (peek_char() == '>')
                return make_token(TokenKind::RightShift, 2);
            return make_token(TokenKind::Greater, 1);

        case '&':
            if (peek_char() == '&')
                return make_token(TokenKind::And, 2);
            return make_token(TokenKind::BitwiseAnd, 1);

        case '|':
            if (peek_char() == '|')
                return make_token(TokenKind::Or, 2);
            return make_token(TokenKind::BitwiseOr, 1);

        case '^':
            return make_token(TokenKind::BitwiseXor, 1);

        case '~':
            return make_token(TokenKind::BitwiseNot, 1);

        case ':':
            return make_token(TokenKind::Colon, 1);

        case '.':
            if (peek_char() == '.' && peek_char(2) == '=')
                return make_token(TokenKind::DotDot, 3);
            if (peek_char() == '.')
                return make_token(TokenKind::DotDotDot, 2);
            return make_token(TokenKind::Dot, 1);

        case '?':
            return make_token(TokenKind::Question, 1);

        case '(':
            return make_token(TokenKind::LeftParen, 1);

        case ')':
            return make_token(TokenKind::RightParen, 1);

        case '{':
            return make_token(TokenKind::LeftBrace, 1);

        case '}':
            return make_token(TokenKind::RightBrace, 1);

        case '[':
            return make_token(TokenKind::LeftBracket, 1);

        case ']':
            return make_token(TokenKind::RightBracket, 1);

        case ';':
            return make_token(TokenKind::Semicolon, 1);

        case ',':
            return make_token(TokenKind::Comma, 1);

        default:
            return make_invalid_token("Unexpected character");
        }
    }

    bool Lexer::is_whitespace(char ch) const
    {
        return ch == ' ' || ch == '\t' || ch == '\v' || ch == '\f';
    }

    bool Lexer::is_newline(char ch) const
    {
        return ch == '\n' || ch == '\r';
    }

    bool Lexer::is_alpha(char ch) const
    {
        return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
    }

    bool Lexer::is_digit(char ch) const
    {
        return ch >= '0' && ch <= '9';
    }

    bool Lexer::is_alnum(char ch) const
    {
        return is_alpha(ch) || is_digit(ch);
    }

    bool Lexer::is_identifier_start(char ch) const
    {
        return is_alpha(ch) || ch == '_';
    }

    bool Lexer::is_identifier_continue(char ch) const
    {
        return is_alnum(ch) || ch == '_';
    }

    bool Lexer::is_hex_digit(char ch) const
    {
        return is_digit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F');
    }

    bool Lexer::is_octal_digit(char ch) const
    {
        return ch >= '0' && ch <= '7';
    }

    bool Lexer::is_binary_digit(char ch) const
    {
        return ch == '0' || ch == '1';
    }

    char Lexer::interpret_escape_sequence()
    {
        // Current character should be backslash
        if (current_char() != '\\')
        {
            error("Expected escape sequence", SourceRange(current_location_, 1));
            return '\0';
        }

        advance_char(); // Skip backslash

        if (at_end())
        {
            error("Unexpected end of file in escape sequence", SourceRange(current_location_, 0));
            return '\0';
        }

        char escaped_char = current_char();
        advance_char(); // Skip the escaped character

        switch (escaped_char)
        {
        case 'n':
            return '\n';
        case 't':
            return '\t';
        case 'r':
            return '\r';
        case 'b':
            return '\b';
        case 'f':
            return '\f';
        case 'v':
            return '\v';
        case 'a':
            return '\a';
        case '0':
            return '\0';
        case '\\':
            return '\\';
        case '\'':
            return '\'';
        case '\"':
            return '\"';
        case 'x':
        {
            // Hexadecimal escape sequence \xHH
            if (at_end() || !is_hex_digit(current_char()))
            {
                error("Invalid hexadecimal escape sequence", SourceRange(current_location_, 1));
                return '\0';
            }
            
            int hex_value = 0;
            for (int i = 0; i < 2 && !at_end() && is_hex_digit(current_char()); ++i)
            {
                char hex_char = current_char();
                advance_char();
                
                if (hex_char >= '0' && hex_char <= '9')
                    hex_value = hex_value * 16 + (hex_char - '0');
                else if (hex_char >= 'a' && hex_char <= 'f')
                    hex_value = hex_value * 16 + (hex_char - 'a' + 10);
                else if (hex_char >= 'A' && hex_char <= 'F')
                    hex_value = hex_value * 16 + (hex_char - 'A' + 10);
            }
            
            return static_cast<char>(hex_value);
        }
        default:
            error("Invalid escape sequence: \\" + std::string(1, escaped_char), SourceRange(current_location_, 1));
            return escaped_char; // Return the character as-is
        }
    }

    TokenStream Lexer::tokenize_all()
    {
        // Reset to beginning
        reset();

        // Tokenize entire source
        std::vector<Token> tokens;

        while (!at_end())
        {
            Token token = next_token();
            tokens.push_back(std::move(token));

            // Stop when we hit EOF
            if (tokens.back().kind == TokenKind::EndOfFile)
            {
                break;
            }
        }

        // Ensure we always have an EOF token
        if (tokens.empty() || tokens.back().kind != TokenKind::EndOfFile)
        {
            tokens.push_back(make_token(TokenKind::EndOfFile, 0));
        }

        return TokenStream(std::move(tokens));
    }

} // namespace Fern