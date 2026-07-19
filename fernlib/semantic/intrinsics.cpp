#include "intrinsics.hpp"

#include <array>

namespace Fern
{

namespace
{

constexpr std::array<std::string_view, 66> kNames = {{
    // i32
    "i32.literal_i32",
    "i32.from_u8",
    "i32.from_f32",
    "i32.from_bool",
    "i32.neg",
    "i32.pos",
    "i32.add",
    "i32.sub",
    "i32.mul",
    "i32.div",
    "i32.eq",
    "i32.ne",
    "i32.gt",
    "i32.lt",
    "i32.ge",
    "i32.le",

    // f32
    "f32.literal_i32",
    "f32.literal_f32",
    "f32.from_i32",
    "f32.from_u8",
    "f32.neg",
    "f32.pos",
    "f32.add",
    "f32.sub",
    "f32.mul",
    "f32.div",
    "f32.eq",
    "f32.ne",
    "f32.gt",
    "f32.lt",
    "f32.ge",
    "f32.le",

    // u8
    "u8.literal_i32",
    "u8.from_i32",
    "u8.from_f32",
    "u8.add",
    "u8.sub",
    "u8.mul",
    "u8.div",
    "u8.eq",
    "u8.ne",
    "u8.gt",
    "u8.lt",
    "u8.ge",
    "u8.le",

    // bool
    "bool.from_i32",
    "bool.not",
    "bool.eq",
    "bool.ne",
    "bool.and",
    "bool.or",

    // char
    "char.eq",
    "char.ne",
    "char.gt",
    "char.lt",
    "char.ge",
    "char.le",

    // string
    "string.init",
    "string.eq",
    "string.ne",
    "string.concat",
    "string.get",

    // array
    "array.init",
    "array.get",
    "array.set",
    "array.copy_to",
}};

}

std::span<const std::string_view> Intrinsics::all()
{
    return kNames;
}

bool Intrinsics::is_known(std::string_view name)
{
    for (auto known : kNames)
    {
        if (known == name) return true;
    }
    return false;
}

}
