#pragma once

#include <vector>

#include "scope.hpp"

namespace Fern
{

struct SemanticContext;
struct RootSyntax;
struct NamespaceDeclSyntax;
struct TypeDeclSyntax;
struct NamespaceSymbol;
struct MethodSymbol;
struct NamedTypeSymbol;
struct TypeSymbol;
struct BaseExprSyntax;
struct BaseStmtSyntax;
struct IdentifierExprSyntax;
struct LiteralExprSyntax;
struct BinaryExprSyntax;
struct AssignmentExprSyntax;
struct CallExprSyntax;
struct MemberAccessExprSyntax;
struct ParenExprSyntax;
struct BlockExprSyntax;
struct ReturnStmtSyntax;
struct VariableDeclSyntax;
struct TypeExprSyntax;

class Binder
{
public:
    Binder(SemanticContext& context);

    void bind_ast(RootSyntax* ast);
    void resolve_all_types();
    void bind_all_methods();

private:
    SemanticContext& context;

    MethodSymbol* currentMethod = nullptr;
    NamedTypeSymbol* currentType = nullptr;
    NamespaceSymbol* currentNamespace = nullptr;
    std::vector<Scope> scopes;
    std::vector<NamedTypeSymbol*> allTypes;
    std::vector<MethodSymbol*> allMethods;

#pragma region Symbol Creation

    void process_namespace(NamespaceDeclSyntax* nsDecl, NamespaceSymbol* parentNs);
    NamedTypeSymbol* create_type_symbol(TypeDeclSyntax* typeDecl, Symbol* parent);

#pragma region Expression Binding

    TypeSymbol* bind_expr(BaseExprSyntax* expr);
    TypeSymbol* bind_identifier(IdentifierExprSyntax* expr);
    TypeSymbol* bind_literal(LiteralExprSyntax* expr);
    TypeSymbol* bind_binary(BinaryExprSyntax* expr);
    TypeSymbol* bind_assignment(AssignmentExprSyntax* expr);
    TypeSymbol* bind_call(CallExprSyntax* expr);
    TypeSymbol* bind_member_access(MemberAccessExprSyntax* expr);
    TypeSymbol* bind_paren(ParenExprSyntax* expr);
    TypeSymbol* bind_block(BlockExprSyntax* expr);

#pragma region Statement Binding

    void bind_stmt(BaseStmtSyntax* stmt);
    void bind_return(ReturnStmtSyntax* stmt);
    void bind_var_decl(VariableDeclSyntax* decl);

#pragma region Type Resolution

    TypeSymbol* resolve_type_expr(BaseExprSyntax* expr);

#pragma region Helpers

    void push_scope();
    void pop_scope();
    Scope& current_scope();

    Symbol* resolve_name(std::string_view name);

    void store_type(BaseExprSyntax* expr, TypeSymbol* type);
    void store_symbol(BaseExprSyntax* expr, Symbol* symbol);
};

}
