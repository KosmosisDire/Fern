#include "file.hpp"

namespace Fern
{

SourceFile::SourceFile(std::string source, std::string path, uint32_t id)
    : sourceText(std::move(source))
    , filePath(std::move(path))
    , fileId(id)
{
}

std::string_view SourceFile::get_text(const Span& span) const
{
    size_t startOffset = line_column_to_offset(span.startLine, span.startColumn);
    size_t endOffset = line_column_to_offset(span.endLine, span.endColumn);

    if (startOffset > sourceText.size() || endOffset > sourceText.size() || startOffset > endOffset)
    {
        return {};
    }

    return std::string_view(sourceText).substr(startOffset, endOffset - startOffset);
}

size_t SourceFile::line_column_to_offset(uint32_t targetLine, uint32_t targetColumn) const
{
    size_t offset = 0;
    uint32_t currentLine = 0;

    while (offset < sourceText.size() && currentLine < targetLine)
    {
        if (sourceText[offset] == '\n')
        {
            currentLine++;
        }
        offset++;
    }

    offset += targetColumn;
    return offset;
}

}
