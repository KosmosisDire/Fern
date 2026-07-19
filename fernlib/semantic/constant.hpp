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

    std::string format() const
    {
        switch (kind)
        {
            case Kind::Int:     return std::to_string(intValue);
            case Kind::Float:   return std::to_string(floatValue);
            case Kind::Bool:    return boolValue ? "true" : "false";
            case Kind::String:  return std::format("\"{}\"", stringValue);
        }
    }

    bool range_fits(TypeSymbol* target) const;
};

}
