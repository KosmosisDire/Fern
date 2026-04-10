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

static MatchQuality grade_arg(TypeSymbol* argType, TypeSymbol* paramType)
{
    auto conv = NamedTypeSymbol::get_convertibility(argType, paramType);
    switch (conv)
    {
        case Convertibility::Exact:    return MatchQuality::Exact;
        case Convertibility::Implicit: return MatchQuality::ImplicitCast;
        default:                       return MatchQuality::None;
    }
}

static OverloadMatch grade_method(MethodSymbol* method, const std::vector<TypeSymbol*>& argTypes)
{
    OverloadMatch match;
    match.method = method;

    if (method->parameters.size() != argTypes.size())
    {
        match.failCount = static_cast<int>(argTypes.size());
        return match;
    }

    for (size_t i = 0; i < argTypes.size(); ++i)
    {
        switch (grade_arg(argTypes[i], method->parameters[i]->type))
        {
            case MatchQuality::Exact:        ++match.exactCount; break;
            case MatchQuality::ImplicitCast:  ++match.implicitCount; break;
            case MatchQuality::None:          ++match.failCount; break;
        }
    }

    return match;
}

static bool is_better_match(const OverloadMatch& a, const OverloadMatch& b)
{
    if (a.failCount != b.failCount) return a.failCount < b.failCount;
    return a.exactCount > b.exactCount;
}

static OverloadResult resolve_overloads(const std::vector<OverloadMatch>& candidates)
{
    if (candidates.empty()) return {};

    std::vector<OverloadMatch> applicable;
    for (const auto& c : candidates)
    {
        if (c.is_callable())
            applicable.push_back(c);
    }

    if (applicable.size() == 1)
        return {applicable[0]};

    if (applicable.size() > 1)
    {
        OverloadMatch best = applicable[0];
        bool tied = false;
        std::vector<MethodSymbol*> tiedMethods;

        for (size_t i = 1; i < applicable.size(); ++i)
        {
            if (is_better_match(applicable[i], best))
            {
                best = applicable[i];
                tied = false;
                tiedMethods.clear();
            }
            else if (!is_better_match(best, applicable[i]))
            {
                if (!tied)
                    tiedMethods.push_back(best.method);
                tiedMethods.push_back(applicable[i].method);
                tied = true;
            }
        }

        if (tied)
            return {{}, tiedMethods, true};

        return {best};
    }

    OverloadMatch best = candidates[0];
    for (size_t i = 1; i < candidates.size(); ++i)
    {
        if (is_better_match(candidates[i], best))
            best = candidates[i];
    }
    return {best};
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
        candidates.push_back(grade_method(method, argTypes));
    }

    return resolve_overloads(candidates);
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
        candidates.push_back(grade_method(method, argTypes));
    }

    return resolve_overloads(candidates);
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

MethodSymbol* NamedTypeSymbol::find_index_getter(TypeSymbol* indexType)
{
    if (table) table->ensure_members_populated(this);
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != TokenKind::IndexOp || method->parameters.size() != 2)
        {
            continue;
        }
        if (!indexType || method->parameters[1]->type == indexType)
        {
            return method;
        }
    }
    return nullptr;
}

MethodSymbol* NamedTypeSymbol::find_index_setter(TypeSymbol* indexType, TypeSymbol* valueType)
{
    if (table) table->ensure_members_populated(this);
    for (auto* method : methods)
    {
        if (!method->is_operator() || method->operatorKind != TokenKind::IndexSetOp || method->parameters.size() != 3)
        {
            continue;
        }
        if ((!indexType || method->parameters[1]->type == indexType) &&
            (!valueType || method->parameters[2]->type == valueType))
        {
            return method;
        }
    }
    return nullptr;
}

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

Convertibility NamedTypeSymbol::get_convertibility(TypeSymbol* from, TypeSymbol* to)
{
    if (!from || !to) return Convertibility::None;
    if (from == to) return Convertibility::Exact;

    auto* sourceNamed = from->as<NamedTypeSymbol>();
    auto* targetNamed = to->as<NamedTypeSymbol>();

    if (targetNamed && targetNamed->find_implicit_cast(from, to))
        return Convertibility::Implicit;
    if (sourceNamed && sourceNamed->find_implicit_cast(from, to))
        return Convertibility::Implicit;

    if (targetNamed && targetNamed->find_explicit_cast(from, to))
        return Convertibility::Explicit;
    if (sourceNamed && sourceNamed->find_explicit_cast(from, to))
        return Convertibility::Explicit;

    return Convertibility::None;
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
