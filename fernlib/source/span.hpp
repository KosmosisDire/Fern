#pragma once

#include <cstdint>
#include <string>

namespace Fern
{

struct Span
{
    uint32_t startLine = 0;
    uint32_t startColumn = 0;
    uint32_t endLine = 0;
    uint32_t endColumn = 0;

    uint32_t fileId = 0;

    Span() = default;
    Span(uint32_t startLine, uint32_t startColumn, uint32_t endLine, uint32_t endColumn, uint32_t fileId = 0);

    bool contains(uint32_t line, uint32_t col) const;
    Span at_start() const;
    Span at_end() const;
    Span merge(const Span& other) const;
    Span expand_until(uint32_t line, uint32_t column) const;
    std::string format() const;
};

} 
