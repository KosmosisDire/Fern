#pragma once

#include <initializer_list>
#include <memory>
#include <span>
#include <string_view>
#include <vector>

#include "symbol.hpp"

namespace Fern
{

class SymbolTable
{
public:
    NamespaceSymbol* globalNamespace = nullptr;

    SymbolTable();

    template<typename T>
    T* own(std::unique_ptr<T> symbol)
    {
        T* raw = symbol.get();
        raw->table = this;
        allSymbols.push_back(std::move(symbol));
        return raw;
    }

    NamespaceSymbol* get_or_create_namespace(NamespaceSymbol* parent, std::string_view name);
    NamedTypeSymbol* get_or_create_instantiation(NamedTypeSymbol* templ, const std::vector<TypeSymbol*>& typeArgs);
    TypeSymbol* substitute_type(TypeSymbol* type, NamedTypeSymbol* origin, const std::vector<TypeSymbol*>& typeArgs);
    void populate_instantiation_members(NamedTypeSymbol* inst);
    void ensure_members_populated(NamedTypeSymbol* type);

    Symbol* lookup(std::span<const std::string_view> path);
    Symbol* lookup(std::initializer_list<std::string_view> path);
    Symbol* lookup_from(Symbol* start, std::span<const std::string_view> path);

    std::string format() const;

private:
    std::vector<std::unique_ptr<Symbol>> allSymbols;
};

}
