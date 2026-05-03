#include "display.hpp"

#include <semantic/builtin_aliases.hpp>
#include <semantic/symbol/symbol.hpp>

#include <format>
#include <string_view>

using namespace Fern;

namespace FernDisplay
{

namespace
{

std::string symbol_name(Symbol* sym, const DisplayFormat& fmt)
{
    if (!sym) return "?";
    return fmt.has(DisplayOption::QualifyTypes) ? sym->qualified_name() : sym->name;
}

bool is_core_array(NamedTypeSymbol* named)
{
    if (!named) return false;
    NamedTypeSymbol* origin = named->genericOrigin ? named->genericOrigin : named;
    if (!origin) return false;
    if (origin->name != "Array") return false;
    return origin->parent && origin->parent->name == "Core";
}

}

DisplayFormat DisplayFormat::short_form()
{
    return DisplayFormat{
        .options = DisplayOption::UseBuiltinAliases
                 | DisplayOption::IncludeTypeOnLocals
                 | DisplayOption::IncludeTypeOnParameters
                 | DisplayOption::IncludeTypeOnFields,
    };
}

DisplayFormat DisplayFormat::long_form()
{
    return DisplayFormat{
        .options = DisplayOption::QualifyTypes
                 | DisplayOption::UseBuiltinAliases
                 | DisplayOption::IncludeKindKeyword
                 | DisplayOption::IncludeContainingType
                 | DisplayOption::IncludeParameterNames
                 | DisplayOption::IncludeReturnType
                 | DisplayOption::IncludeTypeOnLocals
                 | DisplayOption::IncludeTypeOnParameters
                 | DisplayOption::IncludeTypeOnFields,
    };
}

#pragma region Type

std::string format_type(TypeSymbol* type, const DisplayFormat& fmt)
{
    if (!type) return "?";

    auto* named = type->as<NamedTypeSymbol>();
    if (fmt.has(DisplayOption::UseBuiltinAliases) && named && named->parent)
    {
        auto alias = BuiltinAliases::find_alias_for(named->parent->name, named->name);
        if (!alias.empty()) return std::string(alias);
    }

    if (fmt.has(DisplayOption::UseBuiltinAliases) && is_core_array(named))
    {
        if (!named->typeArguments.empty())
        {
            return format_type(named->typeArguments[0], fmt) + "[]";
        }
        if (!named->typeParams.empty())
        {
            return std::string(named->typeParams[0]) + "[]";
        }
    }

    if (named && !named->typeArguments.empty())
    {
        std::string result = std::format("{}<", symbol_name(named, fmt));
        for (size_t i = 0; i < named->typeArguments.size(); ++i)
        {
            if (i > 0) result += ", ";
            result += format_type(named->typeArguments[i], fmt);
        }
        result += ">";
        return result;
    }
    if (named && !named->typeParams.empty())
    {
        std::string result = std::format("{}<", symbol_name(named, fmt));
        for (size_t i = 0; i < named->typeParams.size(); ++i)
        {
            if (i > 0) result += ", ";
            result += named->typeParams[i];
        }
        result += ">";
        return result;
    }
    return symbol_name(type, fmt);
}

#pragma region Local

std::string format_local(LocalSymbol* local, const DisplayFormat& fmt)
{
    if (!local) return "?";

    std::string result;
    if (fmt.has(DisplayOption::IncludeKindKeyword)) result = "var ";
    result += local->name;
    if (fmt.has(DisplayOption::IncludeTypeOnLocals))
    {
        result += ": ";
        result += format_type(local->type, fmt);
    }
    return result;
}

#pragma region Parameter

std::string format_parameter(ParameterSymbol* param, const DisplayFormat& fmt)
{
    if (!param) return "?";

    std::string result;
    if (fmt.has(DisplayOption::IncludeKindKeyword)) result = "param ";
    result += param->name;
    if (fmt.has(DisplayOption::IncludeTypeOnParameters))
    {
        result += ": ";
        result += format_type(param->type, fmt);
    }
    return result;
}

#pragma region Field

std::string format_field(FieldSymbol* field, const DisplayFormat& fmt)
{
    if (!field) return "?";

    std::string result;
    if (fmt.has(DisplayOption::IncludeKindKeyword)) result = "field ";

    if (fmt.has(DisplayOption::IncludeContainingType) && field->parent)
    {
        result += symbol_name(field->parent, fmt);
        result += ".";
    }
    result += field->name;
    if (fmt.has(DisplayOption::IncludeTypeOnFields))
    {
        result += ": ";
        result += format_type(field->type, fmt);
    }
    return result;
}

#pragma region Namespace

std::string format_namespace(NamespaceSymbol* ns, const DisplayFormat& fmt)
{
    if (!ns) return "?";

    std::string result;
    if (fmt.has(DisplayOption::IncludeKindKeyword)) result = "namespace ";
    result += symbol_name(ns, fmt);
    return result;
}

#pragma region Method

namespace
{

std::string format_method_kind_and_name(MethodSymbol* method, const DisplayFormat& fmt)
{
    std::string result;
    Symbol* parent = method->parent;
    bool parentIsType = parent && parent->kind == SymbolKind::NamedType;
    bool parentIsNamespace = parent && parent->kind == SymbolKind::Namespace;

    switch (method->callableKind)
    {
        case CallableKind::Function:
        {
            if (fmt.has(DisplayOption::IncludeKindKeyword)) result += "fn ";
            if (fmt.has(DisplayOption::IncludeContainingType) && parentIsType)
            {
                result += symbol_name(parent, fmt);
                result += ".";
            }
            else if (fmt.has(DisplayOption::QualifyTypes) && parentIsNamespace && parent->parent)
            {
                result += parent->qualified_name();
                result += ".";
            }
            result += method->name;
            break;
        }

        case CallableKind::Constructor:
        {
            if (fmt.has(DisplayOption::IncludeKindKeyword)) result += "init ";
            if (parent) result += symbol_name(parent, fmt);
            break;
        }

        case CallableKind::Operator:
        {
            if (fmt.has(DisplayOption::IncludeKindKeyword)) result += "op ";
            if (fmt.has(DisplayOption::IncludeContainingType) && parentIsType)
            {
                result += symbol_name(parent, fmt);
                result += ".";
            }
            result += Fern::format(method->operatorKind);
            break;
        }

        case CallableKind::Literal:
        {
            if (fmt.has(DisplayOption::IncludeKindKeyword)) result += "literal ";
            if (fmt.has(DisplayOption::IncludeContainingType) && parentIsType)
            {
                result += symbol_name(parent, fmt);
                result += ".";
            }
            result += method->name;
            break;
        }

        case CallableKind::Cast:
        {
            if (fmt.has(DisplayOption::IncludeKindKeyword)) result += "cast ";
            if (fmt.has(DisplayOption::IncludeContainingType) && parentIsType)
            {
                result += symbol_name(parent, fmt);
            }
            break;
        }
    }
    return result;
}

}

std::string format_method(MethodSymbol* method, const DisplayFormat& fmt)
{
    if (!method) return "?";

    std::string result = format_method_kind_and_name(method, fmt);

    result += "(";
    for (size_t i = 0; i < method->parameters.size(); ++i)
    {
        if (i > 0) result += ", ";
        ParameterSymbol* p = method->parameters[i];
        if (fmt.has(DisplayOption::IncludeParameterNames) && p && !p->name.empty())
        {
            result += p->name;
            if (fmt.has(DisplayOption::IncludeTypeOnParameters))
            {
                result += ": ";
                result += format_type(p ? p->type : nullptr, fmt);
            }
        }
        else
        {
            result += format_type(p ? p->type : nullptr, fmt);
        }
    }
    result += ")";

    if (fmt.has(DisplayOption::IncludeReturnType))
    {
        TypeSymbol* ret = method->get_return_type();
        result += " -> ";
        result += ret ? format_type(ret, fmt) : std::string("void");
    }

    return result;
}

}
