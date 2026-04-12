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
            declare_namespace(nsDecl, globalNs);
        }
        else if (auto* typeDecl = decl->as<TypeDeclSyntax>())
        {
            declare_type(typeDecl, globalNs);
        }
    }
}

}
