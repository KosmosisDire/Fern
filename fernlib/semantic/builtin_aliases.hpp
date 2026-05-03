#pragma once

#include <span>
#include <string_view>

namespace Fern
{

struct BuiltinAlias
{
    std::string_view alias;
    std::string_view namespaceName;
    std::string_view typeName;
};

class BuiltinAliases
{
public:
    static std::span<const BuiltinAlias> all();
    static const BuiltinAlias* find_by_alias(std::string_view alias);
    static std::string_view find_alias_for(std::string_view namespaceName, std::string_view typeName);
};

}
