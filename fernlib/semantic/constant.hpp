#pragma once

#include <cstdint>
#include <format>
#include <string>
#include <string_view>

namespace Fern
{

struct TypeSymbol;

struct ConstantValue
{
    enum class Kind { Int, Float, Bool, String };

    Kind kind = Kind::Int;
    union
    {
        int64_t intValue;
        double floatValue;
        bool boolValue;
        std::string_view stringValue;
    };

    ConstantValue() : kind(Kind::Int), intValue(0) {}

    static ConstantValue make_int(int64_t v)
    {
        ConstantValue cv;
        cv.kind = Kind::Int;
        cv.intValue = v;
        return cv;
    }

    static ConstantValue make_float(double v)
    {
        ConstantValue cv;
        cv.kind = Kind::Float;
        cv.floatValue = v;
        return cv;
    }

    static ConstantValue make_bool(bool v)
    {
        ConstantValue cv;
        cv.kind = Kind::Bool;
        cv.boolValue = v;
        return cv;
    }

    static ConstantValue make_string(std::string_view v)
    {
        ConstantValue cv;
        cv.kind = Kind::String;
        cv.stringValue = v;
        return cv;
    }

    // Renders a string constant for debug output with control characters escaped, so a newline shows as
    // \n rather than breaking the line. Program output never uses this.
    static std::string escape_string(std::string_view value)
    {
        std::string out;
        out.reserve(value.size() + 2);
        out.push_back('"');
        for (char c : value)
        {
            switch (c)
            {
                case '\n': out += "\\n"; break;
                case '\t': out += "\\t"; break;
                case '\r': out += "\\r"; break;
                case '"':  out += "\\\""; break;
                case '\\': out += "\\\\"; break;
                default:   out.push_back(c); break;
            }
        }
        out.push_back('"');
        return out;
    }

    std::string format() const
    {
        switch (kind)
        {
            case Kind::Int:     return std::to_string(intValue);
            case Kind::Float:   return std::to_string(floatValue);
            case Kind::Bool:    return boolValue ? "true" : "false";
            case Kind::String:  return escape_string(stringValue);
        }
    }

    bool range_fits(TypeSymbol* target) const;
};

}
