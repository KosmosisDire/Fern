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
    enum class Kind { I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, Bool, C8, Addr };

    Kind kind;
    union
    {
        int8_t sbyteValue;
        int16_t shortValue;
        int32_t intValue;
        int64_t longValue;
        uint8_t byteValue;
        uint16_t ushortValue;
        uint32_t uintValue;
        uint64_t ulongValue;
        float floatValue;
        double doubleValue;
        bool boolValue;
        uint8_t charValue;
        uint64_t addrValue;
    };

    Value() : kind(Kind::I32), intValue(0) {}

    static Value make_i8(int8_t v)     { Value x; x.kind = Kind::I8;   x.sbyteValue = v;  return x; }
    static Value make_i16(int16_t v)   { Value x; x.kind = Kind::I16;  x.shortValue = v;  return x; }
    static Value make_i32(int32_t v)   { Value x; x.kind = Kind::I32;  x.intValue = v;    return x; }
    static Value make_i64(int64_t v)   { Value x; x.kind = Kind::I64;  x.longValue = v;   return x; }
    static Value make_u8(uint8_t v)    { Value x; x.kind = Kind::U8;   x.byteValue = v;   return x; }
    static Value make_u16(uint16_t v)  { Value x; x.kind = Kind::U16;  x.ushortValue = v; return x; }
    static Value make_u32(uint32_t v)  { Value x; x.kind = Kind::U32;  x.uintValue = v;   return x; }
    static Value make_u64(uint64_t v)  { Value x; x.kind = Kind::U64;  x.ulongValue = v;  return x; }
    static Value make_f32(float v)     { Value x; x.kind = Kind::F32;  x.floatValue = v;  return x; }
    static Value make_f64(double v)    { Value x; x.kind = Kind::F64;  x.doubleValue = v; return x; }
    static Value make_bool(bool v)     { Value x; x.kind = Kind::Bool; x.boolValue = v;   return x; }
    static Value make_c8(uint8_t v)    { Value x; x.kind = Kind::C8;   x.charValue = v;   return x; }
    static Value make_addr(uint64_t v) { Value x; x.kind = Kind::Addr; x.addrValue = v;   return x; }

    int8_t as_i8()     const { check(Kind::I8);   return sbyteValue; }
    int16_t as_i16()   const { check(Kind::I16);  return shortValue; }
    int32_t as_i32()   const { check(Kind::I32);  return intValue; }
    int64_t as_i64()   const { check(Kind::I64);  return longValue; }
    uint8_t as_u8()    const { check(Kind::U8);   return byteValue; }
    uint16_t as_u16()  const { check(Kind::U16);  return ushortValue; }
    uint32_t as_u32()  const { check(Kind::U32);  return uintValue; }
    uint64_t as_u64()  const { check(Kind::U64);  return ulongValue; }
    float as_f32()     const { check(Kind::F32);  return floatValue; }
    double as_f64()    const { check(Kind::F64);  return doubleValue; }
    bool as_bool()     const { check(Kind::Bool); return boolValue; }
    uint8_t as_c8()    const { check(Kind::C8);   return charValue; }
    uint64_t as_addr() const { check(Kind::Addr); return addrValue; }

    std::string format() const
    {
        switch (kind)
        {
            case Kind::I8:   return std::format("i8 {}", sbyteValue);
            case Kind::I16:  return std::format("i16 {}", shortValue);
            case Kind::I32:  return std::format("i32 {}", intValue);
            case Kind::I64:  return std::format("i64 {}", longValue);
            case Kind::U8:   return std::format("u8 {}", byteValue);
            case Kind::U16:  return std::format("u16 {}", ushortValue);
            case Kind::U32:  return std::format("u32 {}", uintValue);
            case Kind::U64:  return std::format("u64 {}", ulongValue);
            case Kind::F32:  return std::format("f32 {}", floatValue);
            case Kind::F64:  return std::format("f64 {}", doubleValue);
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
        case Value::Kind::I8:   return "i8";
        case Value::Kind::I16:  return "i16";
        case Value::Kind::I32:  return "i32";
        case Value::Kind::I64:  return "i64";
        case Value::Kind::U8:   return "u8";
        case Value::Kind::U16:  return "u16";
        case Value::Kind::U32:  return "u32";
        case Value::Kind::U64:  return "u64";
        case Value::Kind::F32:  return "f32";
        case Value::Kind::F64:  return "f64";
        case Value::Kind::Bool: return "bool";
        case Value::Kind::C8:   return "c8";
        case Value::Kind::Addr: return "addr";
    }
    return "";
}

}
