#pragma once

#include <string>

namespace Fern
{
    // Source range with absolute positioning
    struct SourceLocation
    {
        int offset; // Byte offset in source file
        int line;   // 1-based line number
        int column; // 1-based column number

        SourceLocation() : offset(0), line(1), column(1) {}
        SourceLocation(int off, int ln, int col)
            : offset(off), line(ln), column(col) {}

        SourceLocation operator+(int delta) const
        {
            return SourceLocation(offset + delta, line, column + delta);
        }

        bool operator==(const SourceLocation &other) const
        {
            return offset == other.offset && line == other.line && column == other.column;
        }

        std::string to_string() const
        {
            return "Line " + std::to_string(line) + ", Column " + std::to_string(column);
        }
    };

    // Source range for diagnostics
    struct SourceRange
    {
        SourceLocation start;
        int width;

        SourceRange() = default;
        SourceRange(SourceLocation start_loc, SourceLocation end_loc)
            : start(start_loc), width(end_loc.offset - start_loc.offset) {}
        SourceRange(SourceLocation loc, int width)
            : start(loc), width(width) {}

        bool contains(SourceLocation loc) const
        {
            return loc.offset >= start.offset && loc.offset < start.offset + width;
        }

        int end_offset() const
        {
            return start.offset + width;
        }

        SourceLocation end() const
        {
            return SourceLocation(start.offset + width, start.line, start.column + width);
        }
    };
} // namespace Fern