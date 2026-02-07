#pragma once

#include <string>
#include <string_view>
#include <cstdint>

#include <source/span.hpp>

namespace Fern
{

class SourceFile
{
public:
    SourceFile(std::string sourceText, std::string filePath, uint32_t id);

    std::string_view source() const { return sourceText; }
    std::string_view path() const { return filePath; }
    uint32_t file_id() const { return fileId; }

    std::string_view get_text(const Span& span) const;

private:
    std::string sourceText;
    std::string filePath;
    uint32_t fileId;

    size_t line_column_to_offset(uint32_t targetLine, uint32_t targetColumn) const;
};

}
