#pragma once

#include <string_view>
#include <unordered_map>

namespace Fern
{

struct Symbol;

struct Scope
{
    // string views all point to parts of the original source text, so they remain valid as long as the source file is alive
    std::unordered_map<std::string_view, Symbol*> names;

    Symbol* find(std::string_view name) const
    {
        auto it = names.find(name);
        if (it != names.end())
        {
            return it->second;
        }
        return nullptr;
    }

    void add(std::string_view name, Symbol* symbol)
    {
        names[name] = symbol;
    }
};

}

