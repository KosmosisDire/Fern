#include "walker.hpp"

namespace Fern
{

SourceWalker::SourceWalker(const SourceFile& file)
    : sourceFile(file)
{
}

bool SourceWalker::is_at_end() const
{
    return currentPos >= sourceFile.source().size();
}

char SourceWalker::peek() const
{
    if (is_at_end())
    {
        return '\0';
    }
    return sourceFile.source()[currentPos];
}

char SourceWalker::peek_next() const
{
    if (currentPos + 1 >= sourceFile.source().size())
    {
        return '\0';
    }
    return sourceFile.source()[currentPos + 1];
}

char SourceWalker::advance()
{
    char c = sourceFile.source()[currentPos++];
    if (c == '\n')
    {
        currentLine++;
        currentColumn = 0;
    }
    else
    {
        currentColumn++;
    }
    return c;
}

bool SourceWalker::match(char expected)
{
    if (is_at_end())
    {
        return false;
    }
    if (sourceFile.source()[currentPos] != expected)
    {
        return false;
    }
    advance();
    return true;
}

uint32_t SourceWalker::line() const
{
    return currentLine;
}

uint32_t SourceWalker::column() const
{
    return currentColumn;
}

size_t SourceWalker::position() const
{
    return currentPos;
}

void SourceWalker::mark_start()
{
    startPos = currentPos;
    startLine = currentLine;
    startColumn = currentColumn;
}

std::string_view SourceWalker::lexeme() const
{
    return sourceFile.source().substr(startPos, currentPos - startPos);
}

Span SourceWalker::make_span() const
{
    return Span(startLine, startColumn, currentLine, currentColumn, sourceFile.file_id());
}

void SourceWalker::reset()
{
    currentPos = 0;
    currentLine = 0;
    currentColumn = 0;
    startPos = 0;
    startLine = 0;
    startColumn = 0;
}

}
