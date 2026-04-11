#include "symbol.hpp"

namespace Fern
{

static void format_attributes(std::ostringstream& ss, const std::vector<ResolvedAttribute>& attrs, std::string_view pad)
{
    for (const auto& attr : attrs)
    {
        if (attr.type)
        {
            ss << pad << "@" << attr.type->qualified_name() << "\n";
        }
    }
}

std::string format_type_name(TypeSymbol* type)
{
    if (!type) return "?";
    auto* named = type->as<NamedTypeSymbol>();
    if (named && !named->typeArguments.empty())
    {
        std::string result = named->name + "<";
        for (size_t i = 0; i < named->typeArguments.size(); ++i)
        {
            if (i > 0) result += ", ";
            result += format_type_name(named->typeArguments[i]);
        }
        result += ">";
        return result;
    }
    if (named && !named->typeParams.empty())
    {
        std::string result = named->name + "<";
        for (size_t i = 0; i < named->typeParams.size(); ++i)
        {
            if (i > 0) result += ", ";
            result += named->typeParams[i];
        }
        result += ">";
        return result;
    }
    return type->name;
}

std::string MethodSymbol::format_parameters() const
{
    std::string result;
    for (size_t i = 0; i < parameters.size(); ++i)
    {
        if (i > 0) result += ", ";
        result += parameters[i]->name + ": " + format_type_name(parameters[i]->type);
    }
    return result;
}

std::string NamespaceSymbol::format(int indent) const
{
    std::ostringstream ss;
    std::string pad(indent, ' ');
    ss << pad << "namespace " << name;
    if (!namespaces.empty() || !types.empty())
    {
        ss << "\n" << pad << "{\n";
        for (const auto& [name, ns] : namespaces)
        {
            ss << ns->format(indent + 4) << "\n";
        }
        for (const auto* type : types)
        {
            ss << type->format(indent + 4) << "\n";
        }
        ss << pad << "}";
    }
    return ss.str();
}

std::string FieldSymbol::format(int indent) const
{
    std::ostringstream ss;
    std::string pad(indent, ' ');
    format_attributes(ss, resolvedAttributes, pad);
    ss << pad << name << ": " << format_type_name(type);
    return ss.str();
}

std::string NamedTypeSymbol::format(int indent) const
{
    std::ostringstream ss;
    std::string pad(indent, ' ');
    format_attributes(ss, resolvedAttributes, pad);
    ss << pad << Fern::format(modifiers);
    if (modifiers != Modifier::None)
    {
        ss << " ";
    }
    ss << "type " << name;
    if (!typeParams.empty())
    {
        ss << "<";
        for (size_t i = 0; i < typeParams.size(); ++i)
        {
            if (i > 0) ss << ", ";
            ss << typeParams[i];
        }
        ss << ">";
    }
    else if (!typeArguments.empty())
    {
        ss << "<";
        for (size_t i = 0; i < typeArguments.size(); ++i)
        {
            if (i > 0) ss << ", ";
            ss << format_type_name(typeArguments[i]);
        }
        ss << ">";
    }
    if (!fields.empty() || !methods.empty() || !nestedTypes.empty())
    {
        ss << "\n" << pad << "{\n";
        for (const auto& field : fields)
        {
            ss << field->format(indent + 4) << "\n";
        }
        for (const auto& method : methods)
        {
            ss << method->format(indent + 4) << "\n";
        }
        for (const auto& nested : nestedTypes)
        {
            ss << nested->format(indent + 4) << "\n";
        }
        ss << pad << "}";
    }
    for (size_t i = 0; i < instantiations.size(); ++i)
    {
        ss << "\n" << instantiations[i]->format(indent);
    }
    return ss.str();
}

std::string MethodSymbol::format(int indent) const
{
    std::ostringstream ss;
    std::string pad(indent, ' ');
    std::string retName = get_return_type() ? format_type_name(get_return_type()) : "void";

    format_attributes(ss, resolvedAttributes, pad);
    ss << pad << Fern::format(modifiers);
    if (modifiers != Modifier::None)
    {
        ss << " ";
    }

    switch (callableKind)
    {
        case CallableKind::Constructor:
            ss << "init";
            break;
        case CallableKind::Operator:
            ss << "op " << Fern::format(operatorKind);
            break;
        case CallableKind::Function:
            ss << "fn " << name;
            break;
        case CallableKind::Literal:
            ss << "literal " << name;
            break;
        case CallableKind::Cast:
            ss << "cast";
            break;
    }

    ss << "(";
    for (size_t i = 0; i < parameters.size(); ++i)
    {
        if (i > 0) ss << ", ";
        ss << parameters[i]->name << ": " << format_type_name(parameters[i]->type);
    }
    ss << ") -> " << retName;

    return ss.str();
}

}
