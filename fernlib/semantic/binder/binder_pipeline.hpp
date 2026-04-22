#pragma once

namespace Fern
{

struct SemanticContext;
struct RootSyntax;
struct NamespaceDeclSyntax;
struct TypeDeclSyntax;
struct NamespaceSymbol;
struct NamedTypeSymbol;
struct Symbol;

// Phase driver for semantic analysis. Walks the symbol table / AST across the binder phases and produces bound FHIR. 
// Caches and per-scope binders live on SemanticContext.
class BinderPipeline
{
public:
    explicit BinderPipeline(SemanticContext& context) : context(context) {}

    void declare_symbols(RootSyntax* ast);
    void resolve_signatures();
    void resolve_attributes();
    void bind_methods();
    void validate_signatures();

private:
    SemanticContext& context;

    void define_namespace(NamespaceDeclSyntax* nsDecl, NamespaceSymbol* parentNs);
    NamedTypeSymbol* define_type(TypeDeclSyntax* typeDecl, Symbol* parent);
    void check_duplicate_methods(NamedTypeSymbol* type);
};

}
