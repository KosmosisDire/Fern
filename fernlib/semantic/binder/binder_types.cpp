#include "binder.hpp"

#include <span>

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

static std::string join_path(const std::vector<std::string_view>& path)
{
    std::string result;
    for (size_t i = 0; i < path.size(); ++i)
    {
        if (i > 0) result += ".";
        result += path[i];
    }
    return result;
}

static bool extract_type_path(BaseExprSyntax* expr, std::vector<std::string_view>& path)
{
    if (auto* typeExpr = expr->as<TypeExprSyntax>())
    {
        path.push_back(typeExpr->name.lexeme);
        return true;
    }
    if (auto* idExpr = expr->as<IdentifierExprSyntax>())
    {
        path.push_back(idExpr->name.lexeme);
        return true;
    }
    if (auto* memberExpr = expr->as<MemberAccessExprSyntax>())
    {
        if (!extract_type_path(memberExpr->left, path)) return false;
        path.push_back(memberExpr->right.lexeme);
        return true;
    }
    return false;
}

#pragma region Type Resolution

TypeSymbol* Binder::resolve_type_expr(BaseExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* typeExpr = expr->as<TypeExprSyntax>())
    {
        auto it = typeParamSubstitutions.find(std::string(typeExpr->name.lexeme));
        if (it != typeParamSubstitutions.end()) return it->second;

        TypeSymbol* builtin = context.resolve_type_name(typeExpr->name.lexeme);
        if (builtin) return builtin;

        Symbol* sym = resolve_name(typeExpr->name.lexeme);
        if (sym) return sym->as<TypeSymbol>();

        error("undefined type '" + std::string(typeExpr->name.lexeme) + "'", expr->span);
        return nullptr;
    }

    if (auto* genericExpr = expr->as<GenericTypeExprSyntax>())
    {
        return resolve_generic_type(genericExpr);
    }

    if (auto* arrayExpr = expr->as<ArrayTypeExprSyntax>())
    {
        TypeSymbol* elementType = resolve_type_expr(arrayExpr->elementType);
        if (!elementType) return nullptr;

        auto* coreNs = context.symbols.globalNamespace->find_namespace("Core");
        auto* arrayTemplate = coreNs ? coreNs->find_type("Array", 1) : nullptr;
        if (!arrayTemplate)
        {
            error("Core.Array type not found", expr->span);
            return nullptr;
        }

        return context.symbols.get_or_create_instantiation(arrayTemplate, {elementType});
    }

    if (auto* memberExpr = expr->as<MemberAccessExprSyntax>())
    {
        std::vector<std::string_view> path;
        if (extract_type_path(memberExpr, path))
        {
            auto* startNs = currentNamespace ? currentNamespace : context.symbols.globalNamespace;
            Symbol* sym = context.symbols.lookup_from(startNs, path);
            if (sym) return sym->as<TypeSymbol>();
        }

        error("undefined type '" + join_path(path) + "'", expr->span);
        return nullptr;
    }

    return nullptr;
}

TypeSymbol* Binder::resolve_generic_type(GenericTypeExprSyntax* expr)
{
    std::vector<std::string_view> path;
    if (!extract_type_path(expr->base, path))
    {
        error("invalid type in generic expression", expr->base->span);
        return nullptr;
    }

    size_t arity = expr->typeArgs.size();
    NamedTypeSymbol* templ = nullptr;

    if (path.size() == 1)
    {
        Symbol* startNs = currentNamespace ? currentNamespace : context.symbols.globalNamespace;
        for (auto* scope = startNs; scope != nullptr; scope = scope->parent)
        {
            if (auto* ns = scope->as<NamespaceSymbol>())
            {
                templ = ns->find_type(path[0], arity);
                if (templ) break;
            }
        }
    }
    else
    {
        std::span<const std::string_view> parentPath(path.data(), path.size() - 1);
        auto* startNs = currentNamespace ? currentNamespace : context.symbols.globalNamespace;
        Symbol* parentSym = context.symbols.lookup_from(startNs, parentPath);
        if (auto* ns = parentSym ? parentSym->as<NamespaceSymbol>() : nullptr)
        {
            templ = ns->find_type(path.back(), arity);
        }
    }

    if (!templ)
    {
        error("undefined type '" + join_path(path) + "'", expr->base->span);
        return nullptr;
    }

    if (!templ->is_generic_definition())
    {
        error("type '" + format_type_name(templ) + "' is not generic", expr->span);
        return nullptr;
    }

    std::vector<TypeSymbol*> typeArgs;
    for (auto* arg : expr->typeArgs)
    {
        TypeSymbol* resolved = resolve_type_expr(arg);
        typeArgs.push_back(resolved);
    }

    if (typeArgs.size() != templ->typeParams.size())
    {
        error("type '" + format_type_name(templ) + "' expects " +
              std::to_string(templ->typeParams.size()) + " type argument(s), got " +
              std::to_string(typeArgs.size()), expr->span);
        return nullptr;
    }

    for (auto* arg : typeArgs)
    {
        if (!arg) return nullptr;
    }

    // Self-reference detection: if all type args are the template's own
    // type params in order (e.g. Box<T> inside Box<T>), return the template itself
    if (typeArgs.size() == templ->typeParamSymbols.size())
    {
        bool isSelfRef = true;
        for (size_t i = 0; i < typeArgs.size(); ++i)
        {
            if (typeArgs[i] != templ->typeParamSymbols[i])
            {
                isSelfRef = false;
                break;
            }
        }
        if (isSelfRef) return templ;
    }

    return context.symbols.get_or_create_instantiation(templ, typeArgs);
}

#pragma region Name Resolution

void Binder::push_scope()
{
    scopes.emplace_back();
}

void Binder::pop_scope()
{
    scopes.pop_back();
}

Scope& Binder::current_scope()
{
    return scopes.back();
}

Symbol* Binder::resolve_name(std::string_view name)
{
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it)
    {
        if (Symbol* sym = it->find(name))
        {
            return sym;
        }
    }

    if (currentType)
    {
        if (auto* field = currentType->find_field(name))
        {
            return field;
        }
        if (auto* method = currentType->find_method(name))
        {
            return method;
        }
    }

    Symbol* start = context.symbols.globalNamespace;
    if (currentType) start = currentType;
    else if (currentNamespace) start = currentNamespace;

    std::string_view path[] = {name};
    if (Symbol* sym = context.symbols.lookup_from(start, path))
    {
        return sym;
    }

    if (TypeSymbol* aliased = context.resolve_type_name(name))
    {
        return aliased;
    }

    return nullptr;
}

Symbol* Binder::resolve_expr_symbol(BaseExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* id = expr->as<IdentifierExprSyntax>())
    {
        return resolve_name(id->name.lexeme);
    }

    if (auto* member = expr->as<MemberAccessExprSyntax>())
    {
        std::vector<std::string_view> path;
        if (extract_type_path(member, path))
        {
            auto* startNs = currentNamespace ? currentNamespace : context.symbols.globalNamespace;
            Symbol* sym = context.symbols.lookup_from(startNs, path);
            if (sym) return sym;
        }

        Symbol* leftSym = resolve_expr_symbol(member->left);
        if (!leftSym) return nullptr;

        if (auto* ns = leftSym->as<NamespaceSymbol>())
        {
            return ns->find_member(member->right.lexeme);
        }
        if (auto* typeRef = leftSym->as<NamedTypeSymbol>())
        {
            if (auto* nested = typeRef->find_nested_type(member->right.lexeme))
                return nested;
            if (auto* field = typeRef->find_field(member->right.lexeme))
                return field;
            if (auto* method = typeRef->find_method(member->right.lexeme))
                return method;
        }

        return nullptr;
    }

    if (auto* generic = expr->as<GenericTypeExprSyntax>())
    {
        return resolve_generic_type(generic);
    }

    return nullptr;
}

}
