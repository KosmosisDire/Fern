#include "file.hpp"

namespace Fern
{

SourceFile::SourceFile(std::string source, std::string path, uint32_t fileId)
    : m_source(std::move(source))
    , m_path(std::move(path))
    , m_fileId(fileId)
{
}

SourceFile::SourceFile(std::string_view source, std::string path, uint32_t fileId)
    : m_source(source)
    , m_path(std::move(path))
    , m_fileId(fileId)
{
}

std::string_view SourceFile::source() const
{
    return m_source;
}

std::string_view SourceFile::path() const
{
    return m_path;
}

uint32_t SourceFile::file_id() const
{
    return m_fileId;
}

std::string_view SourceFile::get_text(const Span& span) const
{
    size_t startOffset = line_column_to_offset(span.startLine, span.startColumn);
    size_t endOffset = line_column_to_offset(span.endLine, span.endColumn);

    if (startOffset > m_source.size() || endOffset > m_source.size())
    {
        return {};
    }

    return std::string_view(m_source).substr(startOffset, endOffset - startOffset);
}

size_t SourceFile::line_column_to_offset(uint32_t targetLine, uint32_t targetColumn) const
{
    size_t offset = 0;
    uint32_t currentLine = 1;

    while (offset < m_source.size() && currentLine < targetLine)
    {
        if (m_source[offset] == '\n')
        {
            currentLine++;
        }
        offset++;
    }

    offset += targetColumn - 1;
    return offset;
}

}
