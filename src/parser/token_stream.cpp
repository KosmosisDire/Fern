#include "parser/token_stream.hpp"
#include <algorithm>
#include <iomanip>
#include <sstream>
#include <stdexcept>

namespace Fern
{
    TokenStream::TokenStream(std::vector<Token> tokens)
        : tokens_(std::move(tokens))
        , position_(0)
    {
    }

    const Token &TokenStream::current() const
    {
        ensure_valid_position();
        return tokens_[position_];
    }

    const Token &TokenStream::peek(int offset) const
    {
        if (offset < 0)
        {
            size_t back_offset = static_cast<size_t>(-offset);
            if (back_offset > position_)
                return tokens_[0];
            return tokens_[position_ - back_offset];
        }

        size_t target = position_ + static_cast<size_t>(offset);
        if (target >= tokens_.size())
            return tokens_.back();
        return tokens_[target];
    }

    const Token &TokenStream::previous() const
    {
        if (position_ == 0)
            return tokens_[0];
        return tokens_[position_ - 1];
    }

    void TokenStream::advance()
    {
        if (!at_end())
            position_++;
    }

    bool TokenStream::at_end() const
    {
        return position_ >= tokens_.size() ||
               tokens_[position_].kind == TokenKind::EndOfFile;
    }

    bool TokenStream::check(TokenKind kind) const
    {
        if (at_end())
            return false;
        return current().kind == kind;
    }

    bool TokenStream::check_any(std::initializer_list<TokenKind> kinds) const
    {
        if (at_end())
            return false;
        TokenKind current_kind = current().kind;
        return std::any_of(kinds.begin(), kinds.end(),
                           [current_kind](TokenKind k) { return k == current_kind; });
    }

    bool TokenStream::check_sequence(std::initializer_list<TokenKind> sequence) const
    {
        size_t offset = 0;
        for (TokenKind kind : sequence)
        {
            if (peek(static_cast<int>(offset)).kind != kind)
                return false;
            offset++;
        }
        return true;
    }

    bool TokenStream::consume(TokenKind kind)
    {
        if (check(kind))
        {
            advance();
            return true;
        }
        return false;
    }

    bool TokenStream::consume_any(std::initializer_list<TokenKind> kinds)
    {
        if (check_any(kinds))
        {
            advance();
            return true;
        }
        return false;
    }

    TokenKind TokenStream::consume_any_get(std::initializer_list<TokenKind> kinds)
    {
        if (at_end())
            return TokenKind::EndOfFile;

        TokenKind current_kind = current().kind;
        if (std::any_of(kinds.begin(), kinds.end(),
                        [current_kind](TokenKind k) { return k == current_kind; }))
        {
            advance();
            return current_kind;
        }
        return TokenKind::EndOfFile;
    }

    void TokenStream::skip_to(TokenKind kind)
    {
        while (!at_end() && !check(kind))
            advance();
    }

    void TokenStream::skip_to_any(std::initializer_list<TokenKind> kinds)
    {
        while (!at_end() && !check_any(kinds))
            advance();
    }

    void TokenStream::skip_past(TokenKind kind)
    {
        skip_to(kind);
        if (check(kind))
            advance();
    }

    TokenStream::Checkpoint TokenStream::checkpoint() const
    {
        return {position_};
    }

    void TokenStream::restore(Checkpoint cp)
    {
        position_ = cp.position;
    }

    void TokenStream::split_right_shift()
    {
        ensure_valid_position();
        if (tokens_[position_].kind != TokenKind::RightShift)
            return;

        Token second = tokens_[position_];
        second.kind = TokenKind::Greater;
        second.text = ">";

        tokens_[position_].kind = TokenKind::Greater;
        tokens_[position_].text = ">";

        tokens_.insert(tokens_.begin() + static_cast<ptrdiff_t>(position_) + 1, second);
    }

    SourceRange TokenStream::location() const
    {
        ensure_valid_position();
        return tokens_[position_].location;
    }

    size_t TokenStream::position() const
    {
        return position_;
    }

    size_t TokenStream::size() const
    {
        return tokens_.size();
    }

    void TokenStream::ensure_valid_position() const
    {
        if (tokens_.empty())
            throw std::runtime_error("TokenStream is empty");
        if (position_ >= tokens_.size())
            throw std::runtime_error("TokenStream position out of bounds");
    }

    std::string TokenStream::dump() const
    {
        std::ostringstream ss;
        ss << "TokenStream (" << tokens_.size() << " tokens, position=" << position_ << "):\n";

        for (size_t i = 0; i < tokens_.size(); ++i)
        {
            const Token &token = tokens_[i];

            if (i == position_)
                ss << " --> ";
            else
                ss << "     ";

            ss << "[" << std::setw(3) << i << "] ";
            ss << "(" << std::setw(4) << token.location.start.line
               << ":" << std::setw(3) << token.location.start.column << ") ";
            ss << std::setw(20) << std::left << Fern::to_string(token.kind);

            if (!token.text.empty() && token.text.length() <= 30)
                ss << " \"" << token.text << "\"";
            else if (!token.text.empty())
                ss << " \"" << token.text.substr(0, 27) << "...\"";

            ss << "\n";
        }

        return ss.str();
    }

} // namespace Fern
