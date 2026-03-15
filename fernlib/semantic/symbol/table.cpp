#include <symbol/table.hpp>

namespace Fern
{

SymbolTable::SymbolTable()
{
    auto globalPtr = std::make_unique<NamespaceSymbol>();
    globalPtr->name = "<global>";
    globalNamespace = globalPtr.get();
    own(std::move(globalPtr));
}

NamespaceSymbol* SymbolTable::get_or_create_namespace(NamespaceSymbol* parent, std::string_view name)
{
    if (!parent)
    {
        return nullptr;
    }

    if (auto* existing = parent->find_namespace(name))
    {
        return existing;
    }

    auto nsPtr = std::make_unique<NamespaceSymbol>();
    nsPtr->name = std::string(name);
    nsPtr->parent = parent;
    auto* ns = own(std::move(nsPtr));
    parent->add_namespace(ns);
    return ns;
}

#pragma region Generic Instantiation

NamedTypeSymbol* SymbolTable::get_or_create_instantiation(NamedTypeSymbol* templ, const std::vector<TypeSymbol*>& typeArgs)
{
    if (auto* existing = templ->find_instantiation(typeArgs))
    {
        return existing;
    }

    auto typePtr = std::make_unique<NamedTypeSymbol>();
    typePtr->name = templ->name;
    typePtr->parent = templ->parent;
    typePtr->modifiers = templ->modifiers;
    typePtr->genericOrigin = templ;
    typePtr->typeArguments = typeArgs;
    auto* inst = own(std::move(typePtr));
    templ->instantiations.push_back(inst);
    return inst;
}

TypeSymbol* SymbolTable::substitute_type(TypeSymbol* type, NamedTypeSymbol* origin, const std::vector<TypeSymbol*>& typeArgs)
{
    if (!type) return nullptr;

    if (type == origin)
    {
        return get_or_create_instantiation(origin, typeArgs);
    }

    if (auto* param = type->as<TypeParamSymbol>())
    {
        if (param->owningType == origin &&
            param->index >= 0 &&
            static_cast<size_t>(param->index) < typeArgs.size())
        {
            return typeArgs[param->index];
        }
    }

    if (auto* named = type->as<NamedTypeSymbol>();
        named && named->is_generic_instantiation() && named->genericOrigin)
    {
        std::vector<TypeSymbol*> newArgs;
        bool changed = false;
        for (auto* arg : named->typeArguments)
        {
            auto* sub = substitute_type(arg, origin, typeArgs);
            newArgs.push_back(sub);
            if (sub != arg) changed = true;
        }
        if (changed)
        {
            return get_or_create_instantiation(named->genericOrigin, newArgs);
        }
    }

    return type;
}

void SymbolTable::ensure_members_populated(NamedTypeSymbol* type)
{
    if (type && type->is_generic_instantiation() && !type->membersPopulated)
    {
        populate_instantiation_members(type);
    }
}

void SymbolTable::populate_instantiation_members(NamedTypeSymbol* inst)
{
    if (inst->membersPopulated) return;
    inst->membersPopulated = true;

    auto* templ = inst->genericOrigin;
    if (!templ) return;

    auto subst = [&](TypeSymbol* type) -> TypeSymbol*
    {
        if (type == templ) return inst;
        return substitute_type(type, templ, inst->typeArguments);
    };

    for (auto* templateField : templ->fields)
    {
        auto fieldPtr = std::make_unique<SubstitutedFieldSymbol>();
        fieldPtr->originalField = templateField;
        fieldPtr->name = templateField->name;
        fieldPtr->parent = inst;
        fieldPtr->syntax = templateField->syntax;
        fieldPtr->modifiers = templateField->modifiers;
        fieldPtr->index = templateField->index;
        fieldPtr->type = subst(templateField->type);
        inst->fields.push_back(own(std::move(fieldPtr)));
    }

    for (auto* templateMethod : templ->methods)
    {
        auto methodPtr = std::make_unique<SubstitutedMethodSymbol>();
        methodPtr->originalMethod = templateMethod;
        methodPtr->name = templateMethod->name;
        methodPtr->syntax = templateMethod->syntax;
        methodPtr->parent = inst;
        methodPtr->modifiers = templateMethod->modifiers;
        methodPtr->callableKind = templateMethod->callableKind;
        methodPtr->operatorKind = templateMethod->operatorKind;

        if (templateMethod->is_constructor())
        {
            methodPtr->set_return_type(inst);
            methodPtr->returnTypeResolved = true;
        }

        auto* method = own(std::move(methodPtr));

        for (auto* templateParam : templateMethod->parameters)
        {
            auto paramPtr = std::make_unique<SubstitutedParameterSymbol>();
            paramPtr->originalParameter = templateParam;
            paramPtr->name = templateParam->name;
            paramPtr->parent = method;
            paramPtr->index = templateParam->index;
            paramPtr->type = subst(templateParam->type);
            method->parameters.push_back(own(std::move(paramPtr)));
        }

        inst->methods.push_back(method);
    }
}

#pragma region Lookup

Symbol* SymbolTable::lookup(std::span<const std::string_view> path)
{
    if (path.empty())
    {
        return nullptr;
    }

    Symbol* current = globalNamespace;

    for (size_t i = 0; i < path.size(); ++i)
    {
        if (auto* ns = current->as<NamespaceSymbol>())
        {
            current = ns->find_member(path[i]);
            if (!current)
            {
                return nullptr;
            }
        }
        else if (auto* type = current->as<NamedTypeSymbol>())
        {
            Symbol* found = nullptr;
            for (auto* nested : type->nestedTypes)
            {
                if (nested->name == path[i])
                {
                    found = nested;
                    break;
                }
            }
            if (!found)
            {
                return nullptr;
            }
            current = found;
        }
        else
        {
            return nullptr;
        }
    }

    return current;
}

Symbol* SymbolTable::lookup(std::initializer_list<std::string_view> path)
{
    return lookup(std::span<const std::string_view>(path.begin(), path.size()));
}

Symbol* SymbolTable::lookup_from(Symbol* start, std::span<const std::string_view> path)
{
    if (path.empty())
    {
        return nullptr;
    }

    for (auto* scope = start; scope != nullptr; scope = scope->parent)
    {
        Symbol* current = nullptr;

        if (auto* ns = scope->as<NamespaceSymbol>())
        {
            current = ns->find_member(path[0]);
        }
        else if (auto* type = scope->as<NamedTypeSymbol>())
        {
            for (auto* nested : type->nestedTypes)
            {
                if (nested->name == path[0])
                {
                    current = nested;
                    break;
                }
            }
        }

        if (!current)
        {
            continue;
        }

        bool resolved = true;
        for (size_t i = 1; i < path.size(); ++i)
        {
            if (auto* innerNs = current->as<NamespaceSymbol>())
            {
                current = innerNs->find_member(path[i]);
                if (!current)
                {
                    resolved = false;
                    break;
                }
            }
            else if (auto* innerType = current->as<NamedTypeSymbol>())
            {
                Symbol* found = nullptr;
                for (auto* nested : innerType->nestedTypes)
                {
                    if (nested->name == path[i])
                    {
                        found = nested;
                        break;
                    }
                }
                if (!found)
                {
                    resolved = false;
                    break;
                }
                current = found;
            }
            else
            {
                resolved = false;
                break;
            }
        }

        if (resolved)
        {
            return current;
        }
    }

    return nullptr;
}

#pragma region Format

std::string SymbolTable::format() const
{
    std::string result;
    result += "---- Symbols ----\n";

    if (globalNamespace)
    {
        result += globalNamespace->format();
        result += "\n";
    }

    return result;
}

}
