#include <symbol/table.hpp>

#include <ast/ast.hpp>

namespace Fern
{

SymbolTable::SymbolTable()
{
    auto globalPtr = std::make_unique<NamespaceSymbol>();
    globalPtr->name = "<global>";
    globalNamespace = globalPtr.get();
    own(std::move(globalPtr));
}

NamespaceSymbol* SymbolTable::get_or_declare_namespace(NamespaceSymbol* parent, std::string_view name)
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

NamespaceSymbol* SymbolTable::get_or_declare_namespace(NamespaceSymbol* parent, NamespaceDeclSyntax* syntax)
{
    auto* ns = get_or_declare_namespace(parent, syntax->name.lexeme);
    if (ns && !ns->syntax)
    {
        ns->syntax = syntax;
    }
    return ns;
}

#pragma region Declare Symbols

NamedTypeSymbol* SymbolTable::declare_type(Symbol* parent, std::string_view name, Modifier modifiers)
{
    auto typePtr = std::make_unique<NamedTypeSymbol>();
    typePtr->name = std::string(name);
    typePtr->parent = parent;
    typePtr->modifiers = modifiers;
    auto* type = own(std::move(typePtr));

    if (auto* ns = parent->as<NamespaceSymbol>())
    {
        ns->add_type(type);
    }
    else if (auto* parentType = parent->as<NamedTypeSymbol>())
    {
        parentType->nestedTypes.push_back(type);
    }

    allTypes.push_back(type);
    return type;
}

NamedTypeSymbol* SymbolTable::declare_type(Symbol* parent, TypeDeclSyntax* syntax)
{
    auto* type = declare_type(parent, syntax->name.lexeme, syntax->modifiers);
    type->syntax = syntax;
    return type;
}

TypeParamSymbol* SymbolTable::declare_type_param(NamedTypeSymbol* owner, int index, std::string_view name)
{
    auto paramPtr = std::make_unique<TypeParamSymbol>();
    paramPtr->name = std::string(name);
    paramPtr->index = index;
    paramPtr->owningType = owner;
    paramPtr->parent = owner;
    auto* param = own(std::move(paramPtr));
    owner->typeParamSymbols.push_back(param);
    return param;
}

FieldSymbol* SymbolTable::declare_field(NamedTypeSymbol* parent, std::string_view name, Modifier modifiers, int index)
{
    auto fieldPtr = std::make_unique<FieldSymbol>();
    fieldPtr->name = std::string(name);
    fieldPtr->parent = parent;
    fieldPtr->modifiers = modifiers;
    fieldPtr->index = index;
    auto* field = own(std::move(fieldPtr));
    parent->fields.push_back(field);
    return field;
}

FieldSymbol* SymbolTable::declare_field(NamedTypeSymbol* parent, FieldDeclSyntax* syntax, int index)
{
    auto* field = declare_field(parent, syntax->name.lexeme, syntax->modifiers, index);
    field->syntax = syntax;
    return field;
}

MethodSymbol* SymbolTable::declare_method(NamedTypeSymbol* parent, std::string_view name, Modifier modifiers, CallableKind kind, TokenKind operatorKind)
{
    auto methodPtr = std::make_unique<MethodSymbol>();
    methodPtr->name = std::string(name);
    methodPtr->parent = parent;
    methodPtr->modifiers = modifiers;
    methodPtr->callableKind = kind;
    methodPtr->operatorKind = operatorKind;
    auto* method = own(std::move(methodPtr));
    parent->methods.push_back(method);
    allMethods.push_back(method);
    if (method->is_literal())
    {
        literalSuffixMap[method->name].push_back(method);
    }
    return method;
}

MethodSymbol* SymbolTable::declare_method(NamedTypeSymbol* parent, CallableDeclSyntax* syntax)
{
    std::string_view name;
    Modifier modifiers = {};
    TokenKind operatorKind = {};

    switch (syntax->callableKind)
    {
        case CallableKind::Function:
            name = syntax->name.lexeme;
            modifiers = syntax->modifiers;
            break;
        case CallableKind::Constructor:
            name = "init";
            modifiers = syntax->modifiers;
            break;
        case CallableKind::Operator:
            name = syntax->name.lexeme;
            operatorKind = syntax->name.kind;
            modifiers = Modifier::Public | Modifier::Static;
            break;
        case CallableKind::Literal:
            name = syntax->name.lexeme;
            modifiers = Modifier::Public | Modifier::Static;
            break;
        case CallableKind::Cast:
            name = "cast";
            modifiers = syntax->modifiers | Modifier::Static;
            break;
    }

    auto* method = declare_method(parent, name, modifiers, syntax->callableKind, operatorKind);
    method->syntax = syntax;
    return method;
}

ParameterSymbol* SymbolTable::declare_parameter(MethodSymbol* parent, std::string_view name, int index)
{
    auto paramPtr = std::make_unique<ParameterSymbol>();
    paramPtr->name = std::string(name);
    paramPtr->parent = parent;
    paramPtr->index = index;
    auto* param = own(std::move(paramPtr));
    parent->parameters.push_back(param);
    return param;
}

ParameterSymbol* SymbolTable::declare_parameter(MethodSymbol* parent, ParameterDeclSyntax* syntax, int index)
{
    auto* param = declare_parameter(parent, syntax->name.lexeme, index);
    param->syntax = syntax;
    return param;
}

#pragma region Generic Instantiation

NamedTypeSymbol* SymbolTable::get_or_declare_generic_instance(NamedTypeSymbol* templ, const std::vector<TypeSymbol*>& typeArgs)
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

NamedTypeSymbol* SymbolTable::get_or_declare_array_type(TypeSymbol* elementType)
{
    if (!elementType) return nullptr;
    auto* coreNs = globalNamespace->find_namespace("Core");
    auto* arrayTemplate = coreNs ? coreNs->find_type("Array", 1) : nullptr;
    if (!arrayTemplate) return nullptr;
    return get_or_declare_generic_instance(arrayTemplate, {elementType});
}

TypeSymbol* SymbolTable::substitute_type(TypeSymbol* type, NamedTypeSymbol* origin, const std::vector<TypeSymbol*>& typeArgs)
{
    if (!type) return nullptr;

    if (type == origin)
    {
        return get_or_declare_generic_instance(origin, typeArgs);
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
            return get_or_declare_generic_instance(named->genericOrigin, newArgs);
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
