#include "walker.hpp"

namespace Fern
{

SourceWalker::SourceWalker(const SourceFile& file)
    : m_file(file)
{
}

bool SourceWalker::is_at_end() const
{
    return m_current >= m_file.source().size();
}

char SourceWalker::peek() const
{
    if (is_at_end())
    {
        return '\0';
    }
    return m_file.source()[m_current];
}

char SourceWalker::peek_next() const
{
    if (m_current + 1 >= m_file.source().size())
    {
        return '\0';
    }
    return m_file.source()[m_current + 1];
}

char SourceWalker::advance()
{
    char c = m_file.source()[m_current++];
    if (c == '\n')
    {
        m_line++;
        m_column = 1;
    }
    else
    {
        m_column++;
    }
    return c;
}

bool SourceWalker::match(char expected)
{
    if (is_at_end())
    {
        return false;
    }
    if (m_file.source()[m_current] != expected)
    {
        return false;
    }
    advance();
    return true;
}

uint32_t SourceWalker::line() const
{
    return m_line;
}

uint32_t SourceWalker::column() const
{
    return m_column;
}

size_t SourceWalker::position() const
{
    return m_current;
}

void SourceWalker::mark_start()
{
    m_start = m_current;
    m_startLine = m_line;
    m_startColumn = m_column;
}

std::string_view SourceWalker::lexeme() const
{
    return m_file.source().substr(m_start, m_current - m_start);
}

Span SourceWalker::make_span() const
{
    return Span(m_startLine, m_startColumn, m_line, m_column, m_file.file_id());
}

void SourceWalker::reset()
{
    m_current = 0;
    m_line = 1;
    m_column = 1;
    m_start = 0;
    m_startLine = 1;
    m_startColumn = 1;
}

}
