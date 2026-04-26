#include "symbol.hpp"
#include "table.hpp"

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

MethodSymbol* NamedTypeSymbol::find_method(std::string_view name)
{
    if (table) table->ensure_members_populated(this);
    for (auto* method : methods)
    {
        if (method->callableKind == CallableKind::Function && method->name == name)
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

Symbol* NamedTypeSymbol::find_member(std::string_view name)
{
    if (auto* nested = find_nested_type(name)) return nested;
    if (auto* field = find_field(name)) return field;
    if (auto* method = find_method(name)) return method;
    return nullptr;
}

#pragma region Overloaded Lookup

OverloadResult NamedTypeSymbol::find_method(std::string_view name, const std::vector<TypeSymbol*>& argTypes)
{
    if (table) table->ensure_members_populated(this);

    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (method->callableKind != CallableKind::Function || method->name != name)
            continue;
        if (method->parameters.size() != argTypes.size())
            continue;
        candidates.push_back(Overload::grade(method, argTypes));
    }

    return Overload::resolve(candidates);
}

OverloadResult NamedTypeSymbol::find_constructor(const std::vector<TypeSymbol*>& argTypes)
{
    if (table) table->ensure_members_populated(this);

    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (!method->is_constructor())
            continue;
        if (method->parameters.size() != argTypes.size())
            continue;
        candidates.push_back(Overload::grade(method, argTypes));
    }

    return Overload::resolve(candidates);
}

OverloadResult NamedTypeSymbol::find_binary_operator(TokenKind opKind, TypeSymbol* leftType, TypeSymbol* rightType)
{
    if (table) table->ensure_members_populated(this);

    std::vector<TypeSymbol*> argTypes = {leftType, rightType};
    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != opKind || method->parameters.size() != 2)
            continue;
        candidates.push_back(Overload::grade(method, argTypes));
    }

    return Overload::resolve(candidates);
}

OverloadResult NamedTypeSymbol::find_unary_operator(TokenKind opKind, TypeSymbol* operandType)
{
    if (table) table->ensure_members_populated(this);

    std::vector<TypeSymbol*> argTypes = {operandType};
    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != opKind || method->parameters.size() != 1)
            continue;
        candidates.push_back(Overload::grade(method, argTypes));
    }

    return Overload::resolve(candidates);
}

OverloadResult NamedTypeSymbol::find_index_getter(TypeSymbol* indexType)
{
    if (table) table->ensure_members_populated(this);

    std::vector<TypeSymbol*> argTypes = {this, indexType};
    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != TokenKind::IndexOp || method->parameters.size() != 2)
            continue;
        candidates.push_back(Overload::grade(method, argTypes));
    }

    return Overload::resolve(candidates);
}

OverloadResult NamedTypeSymbol::find_index_setter(TypeSymbol* indexType, TypeSymbol* valueType)
{
    if (table) table->ensure_members_populated(this);

    std::vector<TypeSymbol*> argTypes = {this, indexType, valueType};
    std::vector<OverloadMatch> candidates;
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != TokenKind::IndexSetOp || method->parameters.size() != 3)
            continue;
        candidates.push_back(Overload::grade(method, argTypes));
    }

    return Overload::resolve(candidates);
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

}
