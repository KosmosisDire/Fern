#include "binder.hpp"

#include <ast/ast.hpp>
#include <semantic/context.hpp>

namespace Fern
{

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
