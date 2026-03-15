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
class SymbolTable;

struct Symbol;
struct NamespaceSymbol;
struct TypeSymbol;
struct NamedTypeSymbol;
struct TypeParamSymbol;
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
    TypeParam,
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
    SymbolTable* table = nullptr;

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
    std::unordered_map<std::string, NamespaceSymbol*, StringHash, StringEqual> namespaces;
    std::vector<NamedTypeSymbol*> types;

    NamespaceSymbol() { kind = Kind; }

    NamespaceSymbol* find_namespace(std::string_view name)
    {
        auto it = namespaces.find(name);
        return it != namespaces.end() ? it->second : nullptr;
    }

    void add_namespace(NamespaceSymbol* ns)
    {
        namespaces[ns->name] = ns;
    }

    NamedTypeSymbol* find_type(std::string_view name, size_t arity = 0);

    void add_type(NamedTypeSymbol* type)
    {
        types.push_back(type);
    }

    Symbol* find_member(std::string_view name);

    std::string format(int indent = 0) const override;
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
    std::vector<FieldSymbol*> fields;
    std::vector<MethodSymbol*> methods;
    std::vector<NamedTypeSymbol*> nestedTypes;
    std::vector<ResolvedAttribute> resolvedAttributes;

    std::vector<std::string> typeParams;
    std::vector<TypeParamSymbol*> typeParamSymbols;
    NamedTypeSymbol* genericOrigin = nullptr;
    std::vector<TypeSymbol*> typeArguments;
    std::vector<NamedTypeSymbol*> instantiations;
    bool membersPopulated = false;

    bool is_attribute() const { return has_modifier(modifiers, Modifier::Attr); }
    bool is_generic_definition() const { return !typeParams.empty(); }
    bool is_generic_instantiation() const { return genericOrigin != nullptr; }
    bool is_concrete_instantiation() const;
    bool is_builtin() const;
    NamedTypeSymbol* find_instantiation(const std::vector<TypeSymbol*>& args) const;

    FieldSymbol* find_field(std::string_view name);
    MethodSymbol* find_method(std::string_view name);
    NamedTypeSymbol* find_nested_type(std::string_view name);

    MethodSymbol* resolve_method(std::string_view name, const std::vector<TypeSymbol*>& argTypes);
    MethodSymbol* resolve_constructor(const std::vector<TypeSymbol*>& argTypes);
    MethodSymbol* find_binary_operator(TokenKind opKind, TypeSymbol* leftType, TypeSymbol* rightType);
    MethodSymbol* find_unary_operator(TokenKind opKind, TypeSymbol* operandType);
    bool has_constructor_with_count(size_t count) const;
    bool has_method_with_count(std::string_view name, size_t count) const;

    std::string format(int indent = 0) const override;
};

struct TypeParamSymbol : TypeSymbol
{
    static constexpr SymbolKind Kind = SymbolKind::TypeParam;
    int index = 0;
    NamedTypeSymbol* owningType = nullptr;

    TypeParamSymbol() { kind = Kind; }

    std::string format(int indent = 0) const override
    {
        return std::string(indent, ' ') + name;
    }
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
    CallableKind callableKind = CallableKind::Function;
    TokenKind operatorKind = TokenKind::Invalid;
    std::vector<ParameterSymbol*> parameters;
    std::vector<ResolvedAttribute> resolvedAttributes;

    MethodSymbol() { kind = Kind; }

    bool is_constructor() const { return callableKind == CallableKind::Constructor; }
    bool is_operator() const { return callableKind == CallableKind::Operator; }
    virtual TypeSymbol* get_return_type() const;
    void set_return_type(TypeSymbol* type) { returnType = type; }

    std::string format(int indent = 0) const override;

protected:
    mutable TypeSymbol* returnType = nullptr;
};

std::string format_type_name(TypeSymbol* type);

struct ParameterSymbol : Symbol
{
    static constexpr SymbolKind Kind = SymbolKind::Parameter;
    TypeSymbol* type = nullptr;
    int index = 0;

    ParameterSymbol() { kind = Kind; }

    std::string format(int indent = 0) const override
    {
        return std::string(indent, ' ') + name + ": " + format_type_name(type);
    }
};

struct LocalSymbol : Symbol
{
    static constexpr SymbolKind Kind = SymbolKind::Local;
    TypeSymbol* type = nullptr;

    LocalSymbol() { kind = Kind; }

    std::string format(int indent = 0) const override
    {
        return std::string(indent, ' ') + name + ": " + format_type_name(type);
    }
};

#pragma region Substituted Symbols

struct SubstitutedFieldSymbol : FieldSymbol
{
    FieldSymbol* originalField = nullptr;
};

class SymbolTable;

struct SubstitutedMethodSymbol : MethodSymbol
{
    MethodSymbol* originalMethod = nullptr;
    mutable bool returnTypeResolved = false;

    TypeSymbol* get_return_type() const override;
};

struct SubstitutedParameterSymbol : ParameterSymbol
{
    ParameterSymbol* originalParameter = nullptr;
};

}
