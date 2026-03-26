#include "symbol.hpp"
#include "table.hpp"

namespace Fern
{

NamedTypeSymbol* NamedTypeSymbol::find_instantiation(const std::vector<TypeSymbol*>& args) const
{
    for (auto* inst : instantiations)
    {
        if (inst->typeArguments == args) return inst;
    }
    return nullptr;
}

TypeSymbol* MethodSymbol::get_return_type() const
{
    return returnType;
}

TypeSymbol* SubstitutedMethodSymbol::get_return_type() const
{
    if (!returnTypeResolved)
    {
        returnTypeResolved = true;
        if (!is_constructor() && originalMethod && parent && table)
        {
            auto* inst = parent->as<NamedTypeSymbol>();
            if (inst && inst->genericOrigin)
            {
                auto* templ = inst->genericOrigin;
                TypeSymbol* origReturn = originalMethod->get_return_type();
                if (origReturn == templ)
                {
                    returnType = inst;
                }
                else
                {
                    returnType = table->substitute_type(origReturn, templ, inst->typeArguments);
                }
            }
        }
    }
    return returnType;
}

bool NamedTypeSymbol::is_concrete_instantiation() const
{
    if (!genericOrigin) return false;
    for (auto* arg : typeArguments)
    {
        if (arg->is<TypeParamSymbol>()) return false;
        if (auto* named = arg->as<NamedTypeSymbol>())
        {
            if (named->is_generic_definition()) return false;
            if (named->is_generic_instantiation() && !named->is_concrete_instantiation()) return false;
        }
    }
    return true;
}

bool NamedTypeSymbol::is_builtin() const
{
    for (const auto& attr : resolvedAttributes)
    {
        if (attr.type && attr.type->qualified_name() == "Core.BuiltinType")
            return true;
    }
    if (genericOrigin)
    {
        return genericOrigin->is_builtin();
    }
    return false;
}

FieldSymbol* NamedTypeSymbol::find_field(std::string_view name)
{
    if (table) table->ensure_members_populated(this);
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
    if (table) table->ensure_members_populated(this);
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
    if (table) table->ensure_members_populated(this);
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
    if (table) table->ensure_members_populated(this);
    for (auto* method : methods)
    {
        if (method->is_constructor() && match_parameters(method->parameters, argTypes))
        {
            return method;
        }
    }
    return nullptr;
}

MethodSymbol* NamedTypeSymbol::find_binary_operator(TokenKind opKind, TypeSymbol* leftType, TypeSymbol* rightType)
{
    if (table) table->ensure_members_populated(this);
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
    if (table) table->ensure_members_populated(this);
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
        if (method->is_constructor() && method->parameters.size() == count)
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

NamedTypeSymbol* NamespaceSymbol::find_type(std::string_view name, size_t arity)
{
    for (auto* type : types)
    {
        if (type->name == name && type->typeParams.size() == arity)
        {
            return type;
        }
    }
    return nullptr;
}

Symbol* NamespaceSymbol::find_member(std::string_view name)
{
    if (auto* ns = find_namespace(name))
    {
        return ns;
    }
    return find_type(name);
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
