#include "symbol.hpp"
#include "table.hpp"

namespace Fern
{

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

bool NamedTypeSymbol::is_integer() const
{
    for (const auto& attr : resolvedAttributes)
    {
        if (attr.type && attr.type->qualified_name() == "Core.NumericInt")
            return true;
    }
    if (genericOrigin) return genericOrigin->is_integer();
    return false;
}

bool NamedTypeSymbol::is_float() const
{
    for (const auto& attr : resolvedAttributes)
    {
        if (attr.type && attr.type->qualified_name() == "Core.NumericFloat")
            return true;
    }
    if (genericOrigin) return genericOrigin->is_float();
    return false;
}

bool NamedTypeSymbol::is_numeric() const
{
    return is_integer() || is_float();
}

bool NamedTypeSymbol::allows_custom_literals() const
{
    for (const auto& attr : resolvedAttributes)
    {
        if (attr.type && attr.type->qualified_name() == "Core.AllowCustomLiterals")
            return true;
    }
    if (genericOrigin)
    {
        return genericOrigin->allows_custom_literals();
    }
    return false;
}

}
