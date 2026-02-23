#include "symbol.hpp"

namespace Fern
{

FieldSymbol* NamedTypeSymbol::find_field(std::string_view name)
{
    for (auto* field : fields)
    {
        if (field->name == name)
        {
            return field;
        }
    }
    return nullptr;
}

MethodSymbol* NamedTypeSymbol::find_method(std::string_view name)
{
    for (auto* method : methods)
    {
        if (method->name == name)
        {
            return method;
        }
    }
    return nullptr;
}

NamedTypeSymbol* NamedTypeSymbol::find_nested_type(std::string_view name)
{
    for (auto* nested : nestedTypes)
    {
        if (nested->name == name)
        {
            return nested;
        }
    }
    return nullptr;
}

static bool match_parameters(const std::vector<ParameterSymbol*>& params, const std::vector<TypeSymbol*>& argTypes)
{
    if (params.size() != argTypes.size())
    {
        return false;
    }
    for (size_t i = 0; i < argTypes.size(); ++i)
    {
        if (argTypes[i] && params[i]->type && argTypes[i] != params[i]->type)
        {
            return false;
        }
    }
    return true;
}

MethodSymbol* NamedTypeSymbol::resolve_method(std::string_view name, const std::vector<TypeSymbol*>& argTypes)
{
    for (auto* method : methods)
    {
        if (method->name == name && match_parameters(method->parameters, argTypes))
        {
            return method;
        }
    }
    return nullptr;
}

MethodSymbol* NamedTypeSymbol::resolve_constructor(const std::vector<TypeSymbol*>& argTypes)
{
    for (auto* method : methods)
    {
        if (method->isConstructor && match_parameters(method->parameters, argTypes))
        {
            return method;
        }
    }
    return nullptr;
}

MethodSymbol* NamedTypeSymbol::find_binary_operator(TokenKind opKind, TypeSymbol* leftType, TypeSymbol* rightType)
{
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != opKind || method->parameters.size() != 2)
        {
            continue;
        }
        if ((!leftType || method->parameters[0]->type == leftType) &&
            (!rightType || method->parameters[1]->type == rightType))
        {
            return method;
        }
    }
    return nullptr;
}

MethodSymbol* NamedTypeSymbol::find_unary_operator(TokenKind opKind, TypeSymbol* operandType)
{
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != opKind || method->parameters.size() != 1)
        {
            continue;
        }
        if (!operandType || method->parameters[0]->type == operandType)
        {
            return method;
        }
    }
    return nullptr;
}

bool NamedTypeSymbol::has_constructor_with_count(size_t count) const
{
    for (auto* method : methods)
    {
        if (method->isConstructor && method->parameters.size() == count)
        {
            return true;
        }
    }
    return false;
}

bool NamedTypeSymbol::has_method_with_count(std::string_view name, size_t count) const
{
    for (auto* method : methods)
    {
        if (method->name == name && method->parameters.size() == count)
        {
            return true;
        }
    }
    return false;
}

NamespaceSymbol* Symbol::find_enclosing_namespace()
{
    for (auto* s = this; s != nullptr; s = s->parent)
    {
        if (auto* ns = s->as<NamespaceSymbol>())
        {
            return ns;
        }
    }
    return nullptr;
}

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

std::string FieldSymbol::format(int indent) const
{
    std::ostringstream ss;
    std::string pad(indent, ' ');
    format_attributes(ss, resolvedAttributes, pad);
    std::string typeName = type ? type->name : "?";
    ss << pad << name << ": " << typeName;
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
    return ss.str();
}

std::string MethodSymbol::format(int indent) const
{
    std::ostringstream ss;
    std::string pad(indent, ' ');
    std::string retName = returnType ? returnType->name : "void";

    format_attributes(ss, resolvedAttributes, pad);
    ss << pad << Fern::format(modifiers);
    if (modifiers != Modifier::None)
    {
        ss << " ";
    }

    if (isConstructor)
    {
        ss << "init";
    }
    else if (is_operator())
    {
        ss << "op " << Fern::format(operatorKind);
    }
    else
    {
        ss << "fn " << name;
    }

    ss << "(";
    for (size_t i = 0; i < parameters.size(); ++i)
    {
        if (i > 0) ss << ", ";
        std::string typeName = parameters[i]->type ? parameters[i]->type->name : "?";
        ss << parameters[i]->name << ": " << typeName;
    }
    ss << ") -> " << retName;

    return ss.str();
}

}
