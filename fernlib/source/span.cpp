#include "span.hpp"

#include <format>

namespace Fern
{

Span::Span(uint32_t startLine, uint32_t startColumn, uint32_t endLine, uint32_t endColumn, uint32_t fileId)
    : startLine(startLine)
    , startColumn(startColumn)
    , endLine(endLine)
    , endColumn(endColumn)
    , fileId(fileId)
{
}

bool Span::contains(uint32_t line, uint32_t col) const
{
    if (line < startLine || line > endLine) return false;
    if (line == startLine && col < startColumn) return false;
    if (line == endLine && col >= endColumn) return false;
    return true;
}

Span Span::at_start() const
{
    return Span(startLine, startColumn, startLine, startColumn, fileId);
}

Span Span::at_end() const
{
    return Span(endLine, endColumn, endLine, endColumn, fileId);
}

Span Span::merge(const Span& other) const
{
    uint32_t newStartLine = startLine;
    uint32_t newStartColumn = startColumn;
    uint32_t newEndLine = endLine;
    uint32_t newEndColumn = endColumn;

    if (other.startLine < newStartLine ||
        (other.startLine == newStartLine && other.startColumn < newStartColumn))
    {
        newStartLine = other.startLine;
        newStartColumn = other.startColumn;
    }

    if (other.endLine > newEndLine ||
        (other.endLine == newEndLine && other.endColumn > newEndColumn))
    {
        newEndLine = other.endLine;
        newEndColumn = other.endColumn;
    }

    return Span(newStartLine, newStartColumn, newEndLine, newEndColumn, fileId);
}

Span Span::expand_until(uint32_t line, uint32_t column) const
{
    uint32_t newEndLine = endLine;
    uint32_t newEndColumn = endColumn;

    if (line > newEndLine || (line == newEndLine && column > newEndColumn))
    {
        newEndLine = line;
        newEndColumn = column;
    }

    return Span(startLine, startColumn, newEndLine, newEndColumn, fileId);
}

std::string Span::format() const
{
    if (startLine == endLine && startColumn == endColumn)
    {
        return std::format("{}:{}", startLine + 1, startColumn + 1);
    }
    return std::format("{}:{} - {}:{}",
        startLine + 1, startColumn + 1,
        endLine + 1, endColumn + 1);
}

} 
