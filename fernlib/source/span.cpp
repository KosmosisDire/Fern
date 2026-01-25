#include "span.hpp"

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

Span Span::at_start() const
{
    return Span(startLine, startColumn, startLine, startColumn, fileId);
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

std::string Span::format() const
{
    if (startLine == endLine && startColumn == endColumn)
    {
        return std::to_string(startLine) + ":" + std::to_string(startColumn);
    }
    return std::to_string(startLine) + ":" + std::to_string(startColumn) +
           " - " +
           std::to_string(endLine) + ":" + std::to_string(endColumn);
}

} 
