#pragma once

#include <cstdint>
#include <format>
#include <string>
#include <string_view>

#ifdef FERN_DEBUG
#include <cassert>
#endif

namespace Fern
{

// An interpreter value. Addr covers frame and heap addresses as well as ref and string handles.
struct Value
{
    enum class Kind { I32, F32, U8, Bool, C8, Addr };

    Kind kind;
    union
    {
        int32_t intValue;
        float floatValue;
        uint8_t byteValue;
        bool boolValue;
        uint8_t charValue;
        uint64_t addrValue;
    };

    Value() : kind(Kind::I32), intValue(0) {}

    static Value make_i32(int32_t v)   { Value x; x.kind = Kind::I32;  x.intValue = v;   return x; }
    static Value make_f32(float v)     { Value x; x.kind = Kind::F32;  x.floatValue = v; return x; }
    static Value make_u8(uint8_t v)    { Value x; x.kind = Kind::U8;   x.byteValue = v;  return x; }
    static Value make_bool(bool v)     { Value x; x.kind = Kind::Bool; x.boolValue = v;  return x; }
    static Value make_c8(uint8_t v)    { Value x; x.kind = Kind::C8;   x.charValue = v;  return x; }
    static Value make_addr(uint64_t v) { Value x; x.kind = Kind::Addr; x.addrValue = v;  return x; }

    int32_t as_i32()   const { check(Kind::I32);  return intValue; }
    float as_f32()     const { check(Kind::F32);  return floatValue; }
    uint8_t as_u8()    const { check(Kind::U8);   return byteValue; }
    bool as_bool()     const { check(Kind::Bool); return boolValue; }
    uint8_t as_c8()    const { check(Kind::C8);   return charValue; }
    uint64_t as_addr() const { check(Kind::Addr); return addrValue; }

    std::string format() const
    {
        switch (kind)
        {
            case Kind::I32:  return std::format("i32 {}", intValue);
            case Kind::F32:  return std::format("f32 {}", floatValue);
            case Kind::U8:   return std::format("u8 {}", byteValue);
            case Kind::Bool: return std::format("bool {}", boolValue ? "true" : "false");
            case Kind::C8:   return std::format("c8 {}", charValue);
            case Kind::Addr: return std::format("addr {}", addrValue);
        }
        return "";
    }

private:
    void check(Kind expected) const
    {
#ifdef FERN_DEBUG
        assert(kind == expected);
#endif
        (void)expected;
    }
};

inline std::string_view format(Value::Kind kind)
{
    switch (kind)
    {
        case Value::Kind::I32:  return "i32";
        case Value::Kind::F32:  return "f32";
        case Value::Kind::U8:   return "u8";
        case Value::Kind::Bool: return "bool";
        case Value::Kind::C8:   return "c8";
        case Value::Kind::Addr: return "addr";
    }
    return "";
}

}
