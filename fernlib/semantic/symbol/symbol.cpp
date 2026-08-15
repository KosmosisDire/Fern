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

bool NamedTypeSymbol::is_ref() const
{
    if (has_modifier(modifiers, Modifier::Ref)) return true;
    if (genericOrigin)
    {
        return genericOrigin->is_ref();
    }
    return false;
}

// Cycles in value types are illegal but not rejected until the layout pass, so guard here
static bool has_default_impl(const NamedTypeSymbol* type, std::vector<const NamedTypeSymbol*>& visiting)
{
    if (type->is_ref()) return false;
    if (type->is_builtin()) return true;
    for (const auto* seen : visiting)
    {
        if (seen == type) return false;
    }
    visiting.push_back(type);
    bool result = true;
    for (const auto* field : type->fields)
    {
        if (!field->type) continue;
        const auto* fieldType = field->type->as<NamedTypeSymbol>();
        if (!fieldType || !has_default_impl(fieldType, visiting))
        {
            result = false;
            break;
        }
    }
    visiting.pop_back();
    return result;
}

bool NamedTypeSymbol::has_default() const
{
    std::vector<const NamedTypeSymbol*> visiting;
    return has_default_impl(this, visiting);
}

bool NamedTypeSymbol::is_integer() const
{
    for (const auto& attr : resolvedAttributes)
    {
        if (attr.type && (attr.type->qualified_name() == "Core.SignedInt"
            || attr.type->qualified_name() == "Core.UnsignedInt"))
            return true;
    }
    if (genericOrigin) return genericOrigin->is_integer();
    return false;
}

bool NamedTypeSymbol::is_unsigned() const
{
    for (const auto& attr : resolvedAttributes)
    {
        if (attr.type && attr.type->qualified_name() == "Core.UnsignedInt")
            return true;
    }
    if (genericOrigin) return genericOrigin->is_unsigned();
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

// A size argument on @BuiltinType marks a scalar. Absent means a handle sized value type like String.
std::optional<int> NamedTypeSymbol::builtin_scalar_size() const
{
    for (const auto& attr : resolvedAttributes)
    {
        if (attr.type && attr.type->qualified_name() == "Core.BuiltinType"
            && !attr.arguments.empty() && attr.arguments[0].kind == ConstantValue::Kind::Int)
        {
            return static_cast<int>(attr.arguments[0].intValue);
        }
    }
    return std::nullopt;
}

std::optional<IntRange> NamedTypeSymbol::integer_range() const
{
    if (!is_integer()) return std::nullopt;

    std::optional<int> size = builtin_scalar_size();
    if (!size) return std::nullopt;

    int bits = *size * 8;
    if (is_unsigned())
    {
        // Constants are held as a signed 64 bit value so a 64 bit unsigned max has no representation
        if (bits >= 64) return std::nullopt;
        return IntRange{ 0, (1LL << bits) - 1 };
    }

    // Shifting into the sign bit is undefined so the widest case takes the limit directly
    int64_t max = bits == 64 ? INT64_MAX : (1LL << (bits - 1)) - 1;
    return IntRange{ -max - 1, max };
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
