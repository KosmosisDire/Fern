#pragma once

#include <cstdint>
#include <string>

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
}

namespace FernDisplay
{

enum class DisplayOption : uint32_t
{
    None                    = 0,
    QualifyTypes            = 1 << 0,
    UseBuiltinAliases       = 1 << 1,
    IncludeKindKeyword      = 1 << 2,
    IncludeContainingType   = 1 << 3,
    IncludeParameterNames   = 1 << 4,
    IncludeReturnType       = 1 << 5,
    IncludeTypeOnLocals     = 1 << 6,
    IncludeTypeOnParameters = 1 << 7,
    IncludeTypeOnFields     = 1 << 8,
};

constexpr DisplayOption operator|(DisplayOption a, DisplayOption b)
{
    return static_cast<DisplayOption>(static_cast<uint32_t>(a) | static_cast<uint32_t>(b));
}

constexpr DisplayOption operator&(DisplayOption a, DisplayOption b)
{
    return static_cast<DisplayOption>(static_cast<uint32_t>(a) & static_cast<uint32_t>(b));
}

constexpr bool has_option(DisplayOption set, DisplayOption flag)
{
    return (set & flag) == flag;
}

struct DisplayFormat
{
    DisplayOption options = DisplayOption::None;

    bool has(DisplayOption opt) const { return has_option(options, opt); }

    static DisplayFormat short_form();
    static DisplayFormat long_form();
};

std::string format_type(Fern::TypeSymbol* type, const DisplayFormat& fmt);
std::string format_method(Fern::MethodSymbol* method, const DisplayFormat& fmt);
std::string format_field(Fern::FieldSymbol* field, const DisplayFormat& fmt);
std::string format_local(Fern::LocalSymbol* local, const DisplayFormat& fmt);
std::string format_parameter(Fern::ParameterSymbol* param, const DisplayFormat& fmt);
std::string format_namespace(Fern::NamespaceSymbol* ns, const DisplayFormat& fmt);

}
