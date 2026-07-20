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
    // TODO: Is there a way to simplify this or move the binding to a different time?
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

bool MethodSymbol::is_intrinsic() const
{
    for (const auto& attr : resolvedAttributes)
    {
        if (attr.type && attr.type->qualified_name() == "Core.Intrinsic")
            return true;
    }
    return false;
}

// TODO: This is kinda a workaround since we have not implemented compile time function execution
// Once we do implement that, we can actually run the attribute constructor
// and get the real struct value.
IntrinsicKind MethodSymbol::intrinsic() const
{
    for (const auto& attr : resolvedAttributes)
    {
        if (attr.type && attr.type->qualified_name() == "Core.Intrinsic" &&
            !attr.arguments.empty() && attr.arguments[0].kind == ConstantValue::Kind::String)
        {
            return intrinsic_from_name(attr.arguments[0].stringValue);
        }
    }
    return IntrinsicKind::None;
}

bool SubstitutedMethodSymbol::is_intrinsic() const
{
    return originalMethod && originalMethod->is_intrinsic();
}

IntrinsicKind SubstitutedMethodSymbol::intrinsic() const
{
    return originalMethod ? originalMethod->intrinsic() : IntrinsicKind::None;
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
