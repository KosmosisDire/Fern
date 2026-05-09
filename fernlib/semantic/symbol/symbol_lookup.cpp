#include "symbol.hpp"
#include "table.hpp"

#include <semantic/fhir/fhir.hpp>

namespace Fern
{

#pragma region Namespace Lookup

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

#pragma region Member Lookup

NamedTypeSymbol* NamedTypeSymbol::find_instantiation(const std::vector<TypeSymbol*>& args) const
{
    for (auto* inst : instantiations)
    {
        if (inst->typeArguments == args) return inst;
    }
    return nullptr;
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

std::vector<MethodSymbol*> NamedTypeSymbol::collect_methods(std::string_view name)
{
    if (table) table->ensure_members_populated(this);
    std::vector<MethodSymbol*> result;
    for (auto* method : methods)
    {
        if (method->callableKind == CallableKind::Function && method->name == name)
        {
            result.push_back(method);
        }
    }
    return result;
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

Symbol* NamedTypeSymbol::find_non_method_member(std::string_view name)
{
    if (auto* nested = find_nested_type(name)) return nested;
    if (auto* field = find_field(name)) return field;
    return nullptr;
}

#pragma region Overloaded Lookup

OverloadResult NamedTypeSymbol::find_method(std::string_view name, const std::vector<OverloadArg>& args)
{
    if (table) table->ensure_members_populated(this);

    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (method->callableKind != CallableKind::Function || method->name != name)
            continue;
        if (method->parameters.size() != args.size())
            continue;
        candidates.push_back(Overload::grade(method, args));
    }

    return Overload::resolve(candidates);
}

OverloadResult NamedTypeSymbol::find_constructor(const std::vector<OverloadArg>& args)
{
    if (table) table->ensure_members_populated(this);

    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (!method->is_constructor())
            continue;
        if (method->parameters.size() != args.size())
            continue;
        candidates.push_back(Overload::grade(method, args));
    }

    return Overload::resolve(candidates);
}

OverloadResult NamedTypeSymbol::find_binary_operator(TokenKind opKind, const OverloadArg& left, const OverloadArg& right)
{
    if (table) table->ensure_members_populated(this);

    std::vector<OverloadArg> args = {left, right};
    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != opKind || method->parameters.size() != 2)
            continue;
        candidates.push_back(Overload::grade(method, args));
    }

    return Overload::resolve(candidates);
}

OverloadResult NamedTypeSymbol::find_unary_operator(TokenKind opKind, const OverloadArg& operand)
{
    if (table) table->ensure_members_populated(this);

    std::vector<OverloadArg> args = {operand};
    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != opKind || method->parameters.size() != 1)
            continue;
        candidates.push_back(Overload::grade(method, args));
    }

    return Overload::resolve(candidates);
}

OverloadResult NamedTypeSymbol::find_index_getter(const OverloadArg& receiver, const OverloadArg& index)
{
    if (table) table->ensure_members_populated(this);

    std::vector<OverloadArg> args = {receiver, index};
    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != TokenKind::IndexOp || method->parameters.size() != 2)
            continue;
        candidates.push_back(Overload::grade(method, args));
    }

    return Overload::resolve(candidates);
}

OverloadResult NamedTypeSymbol::find_index_setter(const OverloadArg& receiver, const OverloadArg& index, const OverloadArg& value)
{
    if (table) table->ensure_members_populated(this);

    std::vector<OverloadArg> args = {receiver, index, value};
    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != TokenKind::IndexSetOp || method->parameters.size() != 3)
            continue;
        candidates.push_back(Overload::grade(method, args));
    }

    return Overload::resolve(candidates);
}

// TODO: returns the first matching setter's value type. 
// We should support overloads at some point.
TypeSymbol* NamedTypeSymbol::expected_index_value_type(TypeSymbol* indexType)
{
    if (table) table->ensure_members_populated(this);

    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != TokenKind::IndexSetOp || method->parameters.size() != 3)
            continue;
        auto conv = get_conversion(indexType, method->parameters[1]->type);
        if (conv.level == Convertibility::Exact || conv.level == Convertibility::Implicit)
            return method->parameters[2]->type;
    }
    return nullptr;
}

#pragma region Conversion Lookup

MethodSymbol* NamedTypeSymbol::find_implicit_cast(TypeSymbol* fromType, TypeSymbol* toType)
{
    if (table) table->ensure_members_populated(this);
    for (auto* method : methods)
    {
        if (method->callableKind != CallableKind::Cast) continue;
        if (!has_modifier(method->modifiers, Modifier::Implicit)) continue;
        if (method->parameters.size() != 1) continue;
        if (method->parameters[0]->type == fromType && method->get_return_type() == toType)
            return method;
    }
    return nullptr;
}

MethodSymbol* NamedTypeSymbol::find_explicit_cast(TypeSymbol* fromType, TypeSymbol* toType)
{
    if (table) table->ensure_members_populated(this);
    for (auto* method : methods)
    {
        if (method->callableKind != CallableKind::Cast) continue;
        if (!has_modifier(method->modifiers, Modifier::Explicit)) continue;
        if (method->parameters.size() != 1) continue;
        if (method->parameters[0]->type == fromType && method->get_return_type() == toType)
            return method;
    }
    return nullptr;
}

Conversion NamedTypeSymbol::get_conversion(TypeSymbol* from, TypeSymbol* to)
{
    if (!from || !to) return {};
    if (from == to) return {Convertibility::Exact, nullptr};

    auto* sourceNamed = from->as<NamedTypeSymbol>();
    auto* targetNamed = to->as<NamedTypeSymbol>();

    MethodSymbol* method = nullptr;
    if (targetNamed) method = targetNamed->find_implicit_cast(from, to);
    if (!method && sourceNamed) method = sourceNamed->find_implicit_cast(from, to);
    if (method) return {Convertibility::Implicit, method};

    if (targetNamed) method = targetNamed->find_explicit_cast(from, to);
    if (!method && sourceNamed) method = sourceNamed->find_explicit_cast(from, to);
    if (method) return {Convertibility::Explicit, method};

    return {};
}

Conversion NamedTypeSymbol::get_conversion(const OverloadArg& arg, TypeSymbol* to)
{
    Conversion conv = get_conversion(arg.type, to);
    if (conv.level != Convertibility::Explicit) return conv;
    if (!arg.constant || arg.constant->kind != ConstantValue::Kind::Int) return conv;

    auto* sourceNamed = arg.type ? arg.type->as<NamedTypeSymbol>() : nullptr;
    auto* targetNamed = to ? to->as<NamedTypeSymbol>() : nullptr;
    if (!sourceNamed || !sourceNamed->is_integer()) return conv;
    if (!targetNamed || !targetNamed->is_integer()) return conv;

    if (arg.constant->range_fits(to))
        conv.level = Convertibility::Implicit;
    return conv;
}

}
