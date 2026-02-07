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
    const SourceFile& sourceFile;

    size_t currentPos = 0;
    uint32_t currentLine = 1;
    uint32_t currentColumn = 1;

    size_t startPos = 0;
    uint32_t startLine = 1;
    uint32_t startColumn = 1;
};

}
