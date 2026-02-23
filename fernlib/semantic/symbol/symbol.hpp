#pragma once

#include <string>
#include <string_view>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <token/kind.hpp>

namespace Fern
{

struct StringHash
{
    using is_transparent = void;
    size_t operator()(std::string_view sv) const { return std::hash<std::string_view>{}(sv); }
};

struct StringEqual
{
    using is_transparent = void;
    bool operator()(std::string_view a, std::string_view b) const { return a == b; }
};

struct BaseSyntax;

struct Symbol;
struct NamespaceSymbol;
struct TypeSymbol;
struct NamedTypeSymbol;
struct FieldSymbol;
struct MethodSymbol;
struct ParameterSymbol;
struct LocalSymbol;

struct ResolvedAttribute
{
    NamedTypeSymbol* type = nullptr;
    MethodSymbol* constructor = nullptr;
};

enum class SymbolKind
{
    Namespace,
    Type,
    Field,
    Method,
    Parameter,
    Local,
};

#pragma region Symbol Base

struct Symbol
{
    std::string name;
    SymbolKind kind;
    Symbol* parent = nullptr;
    BaseSyntax* syntax = nullptr;

    virtual ~Symbol() = default;

    template<typename T>
    bool is() const { return kind == T::Kind; }

    template<typename T>
    T* as() { return is<T>() ? static_cast<T*>(this) : nullptr; }

    template<typename T>
    const T* as() const { return is<T>() ? static_cast<const T*>(this) : nullptr; }

    bool is_auto_generated() const { return syntax == nullptr; }

    NamespaceSymbol* find_enclosing_namespace();

    std::string qualified_name() const
    {
        if (parent && parent->parent) // Don't include global namespace
        {
            return parent->qualified_name() + "." + name;
        }
        return name;
    }

    virtual std::string format(int indent = 0) const
    {
        return std::string(indent, ' ') + name;
    }
};

#pragma region Namespace Symbol

struct NamespaceSymbol : Symbol
{
    static constexpr SymbolKind Kind = SymbolKind::Namespace;
    std::unordered_map<std::string, Symbol*, StringHash, StringEqual> members;

    NamespaceSymbol() { kind = Kind; }

    Symbol* find_member(std::string_view name)
    {
        auto it = members.find(name);
        return it != members.end() ? it->second : nullptr;
    }

    void add_member(Symbol* symbol)
    {
        members[symbol->name] = symbol;
    }

    std::string format(int indent = 0) const override
    {
        std::ostringstream ss;
        std::string pad(indent, ' ');
        ss << pad << "namespace " << name;
        if (!members.empty())
        {
            ss << "\n" << pad << "{\n";
            for (const auto& [name, member] : members)
            {
                ss << member->format(indent + 4) << "\n";
            }
            ss << pad << "}";
        }
        return ss.str();
    }
};

#pragma region Type Symbols

struct TypeSymbol : Symbol
{
    static constexpr SymbolKind Kind = SymbolKind::Type;
    TypeSymbol() { kind = Kind; }
};

struct NamedTypeSymbol : TypeSymbol
{
    Modifier modifiers = Modifier::None;
    bool isAttribute = false;
    std::vector<FieldSymbol*> fields;
    std::vector<MethodSymbol*> methods;
    std::vector<NamedTypeSymbol*> nestedTypes;
    std::vector<ResolvedAttribute> resolvedAttributes;

    FieldSymbol* find_field(std::string_view name);
    MethodSymbol* find_method(std::string_view name);
    NamedTypeSymbol* find_nested_type(std::string_view name);

    MethodSymbol* resolve_method(std::string_view name, const std::vector<TypeSymbol*>& argTypes);
    MethodSymbol* resolve_constructor(const std::vector<TypeSymbol*>& argTypes);
    bool has_constructor_with_count(size_t count) const;
    bool has_method_with_count(std::string_view name, size_t count) const;

    std::string format(int indent = 0) const override;
};

#pragma region Member Symbols

struct FieldSymbol : Symbol
{
    static constexpr SymbolKind Kind = SymbolKind::Field;
    Modifier modifiers = Modifier::None;
    TypeSymbol* type = nullptr;
    int index = 0;
    std::vector<ResolvedAttribute> resolvedAttributes;

    FieldSymbol() { kind = Kind; }

    std::string format(int indent = 0) const override;
};

struct MethodSymbol : Symbol
{
    static constexpr SymbolKind Kind = SymbolKind::Method;
    Modifier modifiers = Modifier::None;
    TokenKind operatorKind = TokenKind::Invalid;
    bool isConstructor = false;
    std::vector<ParameterSymbol*> parameters;
    TypeSymbol* returnType = nullptr;
    std::vector<ResolvedAttribute> resolvedAttributes;

    MethodSymbol() { kind = Kind; }

    bool is_operator() const { return operatorKind != TokenKind::Invalid; }

    std::string format(int indent = 0) const override;
};

struct ParameterSymbol : Symbol
{
    static constexpr SymbolKind Kind = SymbolKind::Parameter;
    TypeSymbol* type = nullptr;
    int index = 0;

    ParameterSymbol() { kind = Kind; }

    std::string format(int indent = 0) const override
    {
        std::string typeName = type ? type->name : "?";
        return std::string(indent, ' ') + name + ": " + typeName;
    }
};

struct LocalSymbol : Symbol
{
    static constexpr SymbolKind Kind = SymbolKind::Local;
    TypeSymbol* type = nullptr;

    LocalSymbol() { kind = Kind; }

    std::string format(int indent = 0) const override
    {
        std::string typeName = type ? type->name : "?";
        return std::string(indent, ' ') + name + ": " + typeName;
    }
};

}
