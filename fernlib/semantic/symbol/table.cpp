#include <symbol/table.hpp>

namespace Fern
{

SymbolTable::SymbolTable()
{
    auto globalPtr = std::make_unique<NamespaceSymbol>();
    globalPtr->name = "<global>";
    globalNamespace = globalPtr.get();
    own(std::move(globalPtr));
}

NamespaceSymbol* SymbolTable::get_or_create_namespace(NamespaceSymbol* parent, std::string_view name)
{
    if (!parent)
    {
        return nullptr;
    }

    if (Symbol* existing = parent->find_member(name))
    {
        if (auto* ns = existing->as<NamespaceSymbol>())
        {
            return ns;
        }
    }

    auto nsPtr = std::make_unique<NamespaceSymbol>();
    nsPtr->name = std::string(name);
    nsPtr->parent = parent;
    auto* ns = own(std::move(nsPtr));
    parent->add_member(ns);
    return ns;
}

#pragma region Lookup

Symbol* SymbolTable::lookup(std::span<const std::string_view> path)
{
    if (path.empty())
    {
        return nullptr;
    }

    Symbol* current = globalNamespace;

    for (size_t i = 0; i < path.size(); ++i)
    {
        if (auto* ns = current->as<NamespaceSymbol>())
        {
            current = ns->find_member(path[i]);
            if (!current)
            {
                return nullptr;
            }
        }
        else if (auto* type = current->as<NamedTypeSymbol>())
        {
            Symbol* found = nullptr;
            for (auto* nested : type->nestedTypes)
            {
                if (nested->name == path[i])
                {
                    found = nested;
                    break;
                }
            }
            if (!found)
            {
                return nullptr;
            }
            current = found;
        }
        else
        {
            return nullptr;
        }
    }

    return current;
}

Symbol* SymbolTable::lookup(std::initializer_list<std::string_view> path)
{
    return lookup(std::span<const std::string_view>(path.begin(), path.size()));
}

Symbol* SymbolTable::lookup_from(Symbol* start, std::span<const std::string_view> path)
{
    if (path.empty())
    {
        return nullptr;
    }

    for (auto* scope = start; scope != nullptr; scope = scope->parent)
    {
        Symbol* current = nullptr;

        if (auto* ns = scope->as<NamespaceSymbol>())
        {
            current = ns->find_member(path[0]);
        }
        else if (auto* type = scope->as<NamedTypeSymbol>())
        {
            for (auto* nested : type->nestedTypes)
            {
                if (nested->name == path[0])
                {
                    current = nested;
                    break;
                }
            }
        }

        if (!current)
        {
            continue;
        }

        bool resolved = true;
        for (size_t i = 1; i < path.size(); ++i)
        {
            if (auto* innerNs = current->as<NamespaceSymbol>())
            {
                current = innerNs->find_member(path[i]);
                if (!current)
                {
                    resolved = false;
                    break;
                }
            }
            else if (auto* innerType = current->as<NamedTypeSymbol>())
            {
                Symbol* found = nullptr;
                for (auto* nested : innerType->nestedTypes)
                {
                    if (nested->name == path[i])
                    {
                        found = nested;
                        break;
                    }
                }
                if (!found)
                {
                    resolved = false;
                    break;
                }
                current = found;
            }
            else
            {
                resolved = false;
                break;
            }
        }

        if (resolved)
        {
            return current;
        }
    }

    return nullptr;
}

#pragma region Format

std::string SymbolTable::format() const
{
    std::string result;
    result += "---- Symbols ----\n";

    if (globalNamespace)
    {
        result += globalNamespace->format();
        result += "\n";
    }

    return result;
}

}
