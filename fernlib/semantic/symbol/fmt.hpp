#pragma once

#include <cstdint>
#include <string>
#include <string_view>

namespace Fern
{

struct Symbol;
struct TypeSymbol;
struct NamedTypeSymbol;
struct TypeParamSymbol;
struct MethodSymbol;
struct FieldSymbol;
struct LocalSymbol;
struct ParameterSymbol;
struct NamespaceSymbol;
enum class SymbolKind;

#pragma region SymbolFormatOption

enum class SymbolFormatOption : uint32_t
{
    None                    = 0,
    QualifyTypes            = 1u << 0,
    UseBuiltinAliases       = 1u << 1,
    IncludeKindKeyword      = 1u << 2,
    IncludeContainingType   = 1u << 3,
    IncludeParameterNames   = 1u << 4,
    IncludeReturnType       = 1u << 5,
    IncludeTypeOnLocals     = 1u << 6,
    IncludeTypeOnParameters = 1u << 7,
    IncludeTypeOnFields     = 1u << 8,
    IncludeAttributes       = 1u << 9,
    IncludeModifiers        = 1u << 10,
    IncludeMembers          = 1u << 11,
    IncludeInstantiations   = 1u << 12,
};

constexpr SymbolFormatOption operator|(SymbolFormatOption a, SymbolFormatOption b)
{
    return static_cast<SymbolFormatOption>(static_cast<uint32_t>(a) | static_cast<uint32_t>(b));
}

constexpr SymbolFormatOption operator&(SymbolFormatOption a, SymbolFormatOption b)
{
    return static_cast<SymbolFormatOption>(static_cast<uint32_t>(a) & static_cast<uint32_t>(b));
}

constexpr SymbolFormatOption& operator|=(SymbolFormatOption& a, SymbolFormatOption b)
{
    a = a | b;
    return a;
}

#pragma region SymbolFormat

struct SymbolFormat
{
    SymbolFormatOption options = SymbolFormatOption::None;
    int indent = 0;

    bool has(SymbolFormatOption opt) const
    {
        return (options & opt) == opt;
    }

    SymbolFormat without(SymbolFormatOption opt) const
    {
        SymbolFormat copy = *this;
        copy.options = static_cast<SymbolFormatOption>(
            static_cast<uint32_t>(options) & ~static_cast<uint32_t>(opt));
        return copy;
    }

    // Strips flags that only apply to a type definition (members, attributes,
    // modifiers, the kind keyword, instantiations list). Use this when emitting
    // a type as a reference so we never recurse into the referenced type's
    // body, which would otherwise loop on signatures like `fn foo() -> Self`.
    SymbolFormat without_def_only() const
    {
        return without(
            SymbolFormatOption::IncludeKindKeyword |
            SymbolFormatOption::IncludeMembers |
            SymbolFormatOption::IncludeInstantiations |
            SymbolFormatOption::IncludeAttributes |
            SymbolFormatOption::IncludeModifiers);
    }

    static SymbolFormat diagnostic();
    static SymbolFormat signature();
    static SymbolFormat hover_short();
    static SymbolFormat hover_long();
    static SymbolFormat tree_dump();
};

#pragma region Public API

std::string_view kind_noun(SymbolKind kind);

std::string format_symbol(Symbol* sym, const SymbolFormat& fmt = SymbolFormat::diagnostic());
std::string format_type(TypeSymbol* type, const SymbolFormat& fmt = SymbolFormat::diagnostic());
std::string format_namespace(NamespaceSymbol* ns, const SymbolFormat& fmt = SymbolFormat::diagnostic());
std::string format_method(MethodSymbol* method, const SymbolFormat& fmt = SymbolFormat::diagnostic());
std::string format_field(FieldSymbol* field, const SymbolFormat& fmt = SymbolFormat::diagnostic());
std::string format_local(LocalSymbol* local, const SymbolFormat& fmt = SymbolFormat::diagnostic());
std::string format_parameter(ParameterSymbol* param, const SymbolFormat& fmt = SymbolFormat::diagnostic());
std::string format_parameter_list(MethodSymbol* method, const SymbolFormat& fmt = SymbolFormat::diagnostic());

}
