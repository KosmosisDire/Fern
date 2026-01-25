#pragma once

#include <string_view>
#include <cstdint>

#include <source/span.hpp>
#include <source/file.hpp>

namespace Fern
{

class SourceWalker
{
public:
    SourceWalker(const SourceFile& file);

    bool is_at_end() const;
    char peek() const;
    char peek_next() const;
    char advance();
    bool match(char expected);

    uint32_t line() const;
    uint32_t column() const;
    size_t position() const;

    void mark_start();
    std::string_view lexeme() const;
    Span make_span() const;

    void reset();

private:
    const SourceFile& m_file;

    size_t m_current = 0;
    uint32_t m_line = 1;
    uint32_t m_column = 1;

    size_t m_start = 0;
    uint32_t m_startLine = 1;
    uint32_t m_startColumn = 1;
};

}
