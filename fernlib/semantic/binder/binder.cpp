#include "binder.hpp"

#include <ast/ast.hpp>
#include <semantic/context.hpp>

namespace Fern
{

Binder::Binder(SemanticContext& context, AllocArena& arena)
    : DiagnosticSystem("Binder")
    , context(context)
    , arena(arena)
    , fhir(arena)
{
}

#pragma region Symbol Creation

void Binder::bind_ast(RootSyntax* ast)
{
    if (!ast)
    {
        return;
    }

    auto* globalNs = context.symbols.globalNamespace;

    for (auto* decl : ast->declarations)
    {
        if (auto* nsDecl = decl->as<NamespaceDeclSyntax>())
        {
            process_namespace(nsDecl, globalNs);
        }
        else if (auto* typeDecl = decl->as<TypeDeclSyntax>())
        {
            create_type_symbol(typeDecl, globalNs);
        }
    }
}

void Binder::process_namespace(NamespaceDeclSyntax* nsDecl, NamespaceSymbol* parentNs)
{
    if (!nsDecl || !parentNs)
    {
        return;
    }

    auto* ns = context.symbols.get_or_create_namespace(parentNs, nsDecl->name.lexeme);

    for (auto* decl : nsDecl->declarations)
    {
        if (auto* nestedNs = decl->as<NamespaceDeclSyntax>())
        {
            process_namespace(nestedNs, ns);
        }
        else if (auto* typeDecl = decl->as<TypeDeclSyntax>())
        {
            create_type_symbol(typeDecl, ns);
        }
    }
}

}
