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

std::string NamedTypeSymbol::format(int indent) const
{
    std::ostringstream ss;
    std::string pad(indent, ' ');
    ss << pad << Fern::format(modifiers) << "type " << name;
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

    ss << pad << Fern::format(modifiers);

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
