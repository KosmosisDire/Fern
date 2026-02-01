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
    SourceFile(std::string source, std::string path, uint32_t fileId);
    SourceFile(std::string_view source, std::string path, uint32_t fileId);

    std::string_view source() const;
    std::string_view path() const;
    uint32_t file_id() const;

    std::string_view get_text(const Span& span) const;

private:
    std::string m_source;
    std::string m_path;
    uint32_t m_fileId;

    size_t line_column_to_offset(uint32_t targetLine, uint32_t targetColumn) const;
};

}
