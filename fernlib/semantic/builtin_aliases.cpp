#include "builtin_aliases.hpp"

#include <array>

namespace Fern
{

namespace
{

constexpr std::array<BuiltinAlias, 16> kAliases = {{
    {"i8",     "Core", "I8"},
    {"i16",    "Core", "I16"},
    {"i32",    "Core", "I32"},
    {"i64",    "Core", "I64"},
    {"u8",     "Core", "U8"},
    {"u16",    "Core", "U16"},
    {"u32",    "Core", "U32"},
    {"u64",    "Core", "U64"},
    {"isize",  "Core", "ISize"},
    {"usize",  "Core", "USize"},
    {"f16",    "Core", "F16"},
    {"f32",    "Core", "F32"},
    {"f64",    "Core", "F64"},
    {"bool",   "Core", "Bool"},
    {"c8",     "Core", "C8"},
    {"string", "Core", "String"},
}};

}

std::span<const BuiltinAlias> BuiltinAliases::all()
{
    return kAliases;
}

const BuiltinAlias* BuiltinAliases::find_by_alias(std::string_view alias)
{
    for (const auto& entry : kAliases)
    {
        if (entry.alias == alias) return &entry;
    }
    return nullptr;
}

std::string_view BuiltinAliases::find_alias_for(std::string_view namespaceName, std::string_view typeName)
{
    for (const auto& entry : kAliases)
    {
        if (entry.namespaceName == namespaceName && entry.typeName == typeName) return entry.alias;
    }
    return {};
}

}
