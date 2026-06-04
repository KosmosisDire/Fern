#include "fmt.hpp"

#include <ast/ast.hpp>
#include <semantic/builtin_aliases.hpp>
#include <semantic/symbol/symbol.hpp>

#include <algorithm>
#include <format>
#include <sstream>
#include <string>
#include <string_view>

namespace Fern
{

#pragma region Presets

SymbolFormat SymbolFormat::diagnostic()
{
    return SymbolFormat{
        .options = SymbolFormatOption::UseBuiltinAliases,
    };
}

SymbolFormat SymbolFormat::signature()
{
    return SymbolFormat{
        .options = SymbolFormatOption::UseBuiltinAliases
                 | SymbolFormatOption::IncludeKindKeyword
                 | SymbolFormatOption::IncludeContainingType
                 | SymbolFormatOption::IncludeParameterNames
                 | SymbolFormatOption::IncludeTypeOnParameters,
    };
}

SymbolFormat SymbolFormat::hover_short()
{
    return SymbolFormat{
        .options = SymbolFormatOption::UseBuiltinAliases
                 | SymbolFormatOption::IncludeTypeOnLocals
                 | SymbolFormatOption::IncludeTypeOnParameters
                 | SymbolFormatOption::IncludeTypeOnFields,
    };
}

SymbolFormat SymbolFormat::hover_long()
{
    return SymbolFormat{
        .options = SymbolFormatOption::QualifyTypes
                 | SymbolFormatOption::UseBuiltinAliases
                 | SymbolFormatOption::IncludeKindKeyword
                 | SymbolFormatOption::IncludeContainingType
                 | SymbolFormatOption::IncludeParameterNames
                 | SymbolFormatOption::IncludeReturnType
                 | SymbolFormatOption::IncludeTypeOnLocals
                 | SymbolFormatOption::IncludeTypeOnParameters
                 | SymbolFormatOption::IncludeTypeOnFields,
    };
}

SymbolFormat SymbolFormat::tree_dump()
{
    return SymbolFormat{
        .options = SymbolFormatOption::IncludeKindKeyword
                 | SymbolFormatOption::IncludeParameterNames
                 | SymbolFormatOption::IncludeReturnType
                 | SymbolFormatOption::IncludeTypeOnLocals
                 | SymbolFormatOption::IncludeTypeOnParameters
                 | SymbolFormatOption::IncludeTypeOnFields
                 | SymbolFormatOption::IncludeAttributes
                 | SymbolFormatOption::IncludeModifiers
                 | SymbolFormatOption::IncludeMembers
                 | SymbolFormatOption::IncludeInstantiations,
    };
}

#pragma region Helpers

namespace
{

std::string symbol_name(Symbol* sym, const SymbolFormat& fmt)
{
    if (!sym) return "?";
    return fmt.has(SymbolFormatOption::QualifyTypes) ? sym->qualified_name() : sym->name;
}

bool is_core_array(NamedTypeSymbol* named)
{
    if (!named) return false;
    NamedTypeSymbol* origin = named->genericOrigin ? named->genericOrigin : named;
    if (!origin) return false;
    if (origin->name != "Array") return false;
    return origin->parent && origin->parent->name == "Core";
}

void format_attributes(std::ostringstream& ss, const std::vector<ResolvedAttribute>& attrs, std::string_view pad)
{
    for (const auto& attr : attrs)
    {
        if (attr.type)
        {
            ss << pad << "@" << attr.type->qualified_name() << "\n";
        }
    }
}

}

#pragma region Kind

std::string_view kind_noun(SymbolKind kind)
{
    switch (kind)
    {
        case SymbolKind::Namespace: return "namespace";
        case SymbolKind::NamedType: return "type";
        case SymbolKind::TypeParam: return "type parameter";
        case SymbolKind::Field:     return "field";
        case SymbolKind::Method:    return "method";
        case SymbolKind::Parameter: return "parameter";
        case SymbolKind::Local:     return "local";
    }
    return "unknown";
}

#pragma region Type

std::string format_type(TypeSymbol* type, const SymbolFormat& fmt)
{
    if (!type) return "?";

    auto* named = type->as<NamedTypeSymbol>();
    bool wantBody = named && fmt.has(SymbolFormatOption::IncludeMembers);
    bool wantInstantiations = named && fmt.has(SymbolFormatOption::IncludeInstantiations);

    std::string prefix;
    if (fmt.has(SymbolFormatOption::IncludeKindKeyword))
    {
        prefix = "type ";
    }

    SymbolFormat inner = fmt.without_def_only();

    std::string name;
    if (fmt.has(SymbolFormatOption::UseBuiltinAliases) && named && named->parent)
    {
        auto alias = BuiltinAliases::find_alias_for(named->parent->name, named->name);
        if (!alias.empty()) name = std::string(alias);
    }

    if (name.empty() && fmt.has(SymbolFormatOption::UseBuiltinAliases) && is_core_array(named))
    {
        if (!named->typeArguments.empty())
        {
            name = format_type(named->typeArguments[0], inner) + "[]";
        }
        else if (!named->typeParams.empty())
        {
            name = std::string(named->typeParams[0]) + "[]";
        }
    }

    if (name.empty() && named && !named->typeArguments.empty())
    {
        name = std::format("{}<", symbol_name(named, fmt));
        for (size_t i = 0; i < named->typeArguments.size(); ++i)
        {
            if (i > 0) name += ", ";
            name += format_type(named->typeArguments[i], inner);
        }
        name += ">";
    }
    else if (name.empty() && named && !named->typeParams.empty())
    {
        name = std::format("{}<", symbol_name(named, fmt));
        for (size_t i = 0; i < named->typeParams.size(); ++i)
        {
            if (i > 0) name += ", ";
            name += named->typeParams[i];
        }
        name += ">";
    }
    else if (name.empty())
    {
        name = symbol_name(type, fmt);
    }

    if (!wantBody && !wantInstantiations)
    {
        return prefix + name;
    }

    std::ostringstream ss;
    std::string pad(fmt.indent, ' ');

    if (fmt.has(SymbolFormatOption::IncludeAttributes))
    {
        format_attributes(ss, named->resolvedAttributes, pad);
    }
    ss << pad;
    if (fmt.has(SymbolFormatOption::IncludeModifiers) && named->modifiers != Modifier::None)
    {
        ss << Fern::format(named->modifiers) << " ";
    }
    ss << prefix << name;

    if (wantBody && (!named->fields.empty() || !named->methods.empty() || !named->nestedTypes.empty()))
    {
        ss << "\n" << pad << "{\n";
        SymbolFormat innerBody = fmt;
        innerBody.indent = fmt.indent + 4;
        for (auto* field : named->fields)
        {
            ss << format_field(field, innerBody) << "\n";
        }
        for (auto* method : named->methods)
        {
            ss << format_method(method, innerBody) << "\n";
        }
        for (auto* nested : named->nestedTypes)
        {
            ss << format_type(nested, innerBody) << "\n";
        }
        ss << pad << "}";
    }

    if (wantInstantiations)
    {
        for (auto* inst : named->instantiations)
        {
            ss << "\n" << format_type(inst, fmt);
        }
    }

    return ss.str();
}

#pragma region Local

std::string format_local(LocalSymbol* local, const SymbolFormat& fmt)
{
    if (!local) return "?";

    std::string result;
    if (fmt.has(SymbolFormatOption::IncludeKindKeyword)) result = "var ";
    result += local->name;
    if (fmt.has(SymbolFormatOption::IncludeTypeOnLocals))
    {
        result += ": ";
        result += format_type(local->type, fmt.without_def_only());
    }
    return result;
}

#pragma region Parameter

std::string format_parameter(ParameterSymbol* param, const SymbolFormat& fmt)
{
    if (!param) return "?";

    std::string result;
    if (fmt.has(SymbolFormatOption::IncludeKindKeyword)) result = "param ";
    result += param->name;
    if (fmt.has(SymbolFormatOption::IncludeTypeOnParameters))
    {
        result += ": ";
        result += format_type(param->type, fmt.without_def_only());
    }
    return result;
}

std::string format_parameter_list(MethodSymbol* method, const SymbolFormat& fmt)
{
    if (!method) return "";
    SymbolFormat inner = fmt.without_def_only();
    std::string result;
    for (size_t i = 0; i < method->parameters.size(); ++i)
    {
        if (i > 0) result += ", ";
        ParameterSymbol* p = method->parameters[i];
        if (!p)
        {
            result += "?";
            continue;
        }
        result += p->name;
        result += ": ";
        result += format_type(p->type, inner);
    }
    return result;
}

#pragma region Field

std::string format_field(FieldSymbol* field, const SymbolFormat& fmt)
{
    if (!field) return "?";

    std::ostringstream ss;
    std::string pad(fmt.indent, ' ');

    if (fmt.has(SymbolFormatOption::IncludeAttributes))
    {
        format_attributes(ss, field->resolvedAttributes, pad);
    }
    ss << pad;
    if (fmt.has(SymbolFormatOption::IncludeKindKeyword)) ss << "field ";

    if (fmt.has(SymbolFormatOption::IncludeContainingType) && field->parent)
    {
        ss << symbol_name(field->parent, fmt) << ".";
    }
    ss << field->name;
    if (fmt.has(SymbolFormatOption::IncludeTypeOnFields))
    {
        ss << ": " << format_type(field->type, fmt.without_def_only());
    }
    return ss.str();
}

#pragma region Namespace

namespace
{

// With a dump whitelist active, a symbol is hidden when its declaration file is
// not on the list. An empty whitelist shows everything.
bool file_hidden(std::span<const uint32_t> dumpFiles, Symbol* sym)
{
    if (dumpFiles.empty()) return false;
    if (!sym || !sym->syntax) return true;
    uint32_t fileId = sym->syntax->span.fileId;
    return std::find(dumpFiles.begin(), dumpFiles.end(), fileId) == dumpFiles.end();
}

// True when the namespace or any descendant holds a type that is not hidden.
bool namespace_has_visible(NamespaceSymbol* ns, std::span<const uint32_t> dumpFiles)
{
    if (!ns) return false;
    for (auto* type : ns->types)
        if (!file_hidden(dumpFiles, type)) return true;
    for (const auto& [name, sub] : ns->namespaces)
        if (namespace_has_visible(sub, dumpFiles)) return true;
    return false;
}

}

std::string format_namespace(NamespaceSymbol* ns, const SymbolFormat& fmt, std::span<const uint32_t> dumpFiles)
{
    if (!ns) return "?";

    std::ostringstream ss;
    std::string pad(fmt.indent, ' ');
    ss << pad;
    if (fmt.has(SymbolFormatOption::IncludeKindKeyword)) ss << "namespace ";
    ss << symbol_name(ns, fmt);

    if (fmt.has(SymbolFormatOption::IncludeMembers) && (!ns->namespaces.empty() || !ns->types.empty()))
    {
        ss << "\n" << pad << "{\n";
        SymbolFormat inner = fmt;
        inner.indent = fmt.indent + 4;
        for (const auto& [name, sub] : ns->namespaces)
        {
            if (!dumpFiles.empty() && !namespace_has_visible(sub, dumpFiles)) continue;
            ss << format_namespace(sub, inner, dumpFiles) << "\n";
        }
        for (auto* type : ns->types)
        {
            if (file_hidden(dumpFiles, type)) continue;
            ss << format_type(type, inner) << "\n";
        }
        ss << pad << "}";
    }
    return ss.str();
}

#pragma region Method

namespace
{

std::string format_method_kind_and_name(MethodSymbol* method, const SymbolFormat& fmt)
{
    std::string result;
    Symbol* parent = method->parent;
    bool parentIsType = parent && parent->kind == SymbolKind::NamedType;
    bool parentIsNamespace = parent && parent->kind == SymbolKind::Namespace;

    switch (method->callableKind)
    {
        case CallableKind::Function:
        {
            if (fmt.has(SymbolFormatOption::IncludeKindKeyword)) result += "fn ";
            if (fmt.has(SymbolFormatOption::IncludeContainingType) && parentIsType)
            {
                result += symbol_name(parent, fmt);
                result += ".";
            }
            else if (fmt.has(SymbolFormatOption::QualifyTypes) && parentIsNamespace && parent->parent)
            {
                result += parent->qualified_name();
                result += ".";
            }
            result += method->name;
            break;
        }

        case CallableKind::Constructor:
        {
            if (fmt.has(SymbolFormatOption::IncludeKindKeyword)) result += "init ";
            if (parent) result += symbol_name(parent, fmt);
            break;
        }

        case CallableKind::Operator:
        {
            if (fmt.has(SymbolFormatOption::IncludeKindKeyword)) result += "op ";
            if (fmt.has(SymbolFormatOption::IncludeContainingType) && parentIsType)
            {
                result += symbol_name(parent, fmt);
                result += ".";
            }
            result += Fern::format(method->operatorKind);
            break;
        }

        case CallableKind::Literal:
        {
            if (fmt.has(SymbolFormatOption::IncludeKindKeyword)) result += "literal ";
            if (fmt.has(SymbolFormatOption::IncludeContainingType) && parentIsType)
            {
                result += symbol_name(parent, fmt);
                result += ".";
            }
            result += method->name;
            break;
        }

        case CallableKind::Cast:
        {
            if (fmt.has(SymbolFormatOption::IncludeKindKeyword)) result += "cast ";
            if (fmt.has(SymbolFormatOption::IncludeContainingType) && parentIsType)
            {
                result += symbol_name(parent, fmt);
            }
            break;
        }
    }
    return result;
}

}

std::string format_method(MethodSymbol* method, const SymbolFormat& fmt)
{
    if (!method) return "?";

    std::ostringstream ss;
    std::string pad(fmt.indent, ' ');

    if (fmt.has(SymbolFormatOption::IncludeAttributes))
    {
        format_attributes(ss, method->resolvedAttributes, pad);
    }
    ss << pad;
    if (fmt.has(SymbolFormatOption::IncludeModifiers) && method->modifiers != Modifier::None)
    {
        ss << Fern::format(method->modifiers) << " ";
    }

    ss << format_method_kind_and_name(method, fmt);

    SymbolFormat innerType = fmt.without_def_only();
    ss << "(";
    for (size_t i = 0; i < method->parameters.size(); ++i)
    {
        if (i > 0) ss << ", ";
        ParameterSymbol* p = method->parameters[i];
        if (fmt.has(SymbolFormatOption::IncludeParameterNames) && p && !p->name.empty())
        {
            ss << p->name;
            if (fmt.has(SymbolFormatOption::IncludeTypeOnParameters))
            {
                ss << ": " << format_type(p->type, innerType);
            }
        }
        else
        {
            ss << format_type(p ? p->type : nullptr, innerType);
        }
    }
    ss << ")";

    if (fmt.has(SymbolFormatOption::IncludeReturnType))
    {
        TypeSymbol* ret = method->get_return_type();
        ss << " -> " << (ret ? format_type(ret, innerType) : std::string("void"));
    }

    return ss.str();
}

#pragma region Dispatch

std::string format_symbol(Symbol* sym, const SymbolFormat& fmt)
{
    if (!sym) return "?";
    switch (sym->kind)
    {
        case SymbolKind::Namespace: return format_namespace(sym->as<NamespaceSymbol>(), fmt);
        case SymbolKind::NamedType: return format_type(sym->as<NamedTypeSymbol>(), fmt);
        case SymbolKind::TypeParam: return format_type(sym->as<TypeSymbol>(), fmt);
        case SymbolKind::Field:     return format_field(sym->as<FieldSymbol>(), fmt);
        case SymbolKind::Method:    return format_method(sym->as<MethodSymbol>(), fmt);
        case SymbolKind::Parameter: return format_parameter(sym->as<ParameterSymbol>(), fmt);
        case SymbolKind::Local:     return format_local(sym->as<LocalSymbol>(), fmt);
    }
    return sym->name;
}

}
