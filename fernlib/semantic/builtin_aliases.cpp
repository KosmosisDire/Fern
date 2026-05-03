#include "builtin_aliases.hpp"

#include <array>

namespace Fern
{

namespace
{

constexpr std::array<BuiltinAlias, 6> kAliases = {{
    {"i32",    "Core", "I32"},
    {"f32",    "Core", "F32"},
    {"bool",   "Core", "Bool"},
    {"u8",     "Core", "U8"},
    {"char",   "Core", "Char"},
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
