#pragma once

#include <unordered_set>
#include <vector>

#include "scope.hpp"
#include <common/diagnostic.hpp>

namespace Fern
{

struct SemanticContext;
struct RootSyntax;
struct NamespaceDeclSyntax;
struct TypeDeclSyntax;
struct BaseDeclSyntax;
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
struct UnaryExprSyntax;
struct InitializerExprSyntax;
struct OperatorDeclSyntax;
struct ParameterDeclSyntax;
struct ThisExprSyntax;
struct TypeExprSyntax;
struct IfStmtSyntax;
struct WhileStmtSyntax;
struct ResolvedAttribute;

class Binder : public DiagnosticSystem
{
public:
    Binder(SemanticContext& context);

    void bind_ast(RootSyntax* ast);
    void resolve_all_types();
    void resolve_all_attributes();
    void bind_all_methods();

private:
    SemanticContext& context;

    MethodSymbol* currentMethod = nullptr;
    NamedTypeSymbol* currentType = nullptr;
    NamespaceSymbol* currentNamespace = nullptr;
    std::vector<Scope> scopes;
    std::vector<NamedTypeSymbol*> allTypes;
    std::vector<MethodSymbol*> allMethods;
    std::unordered_set<MethodSymbol*> boundMethods;
    std::unordered_set<MethodSymbol*> bindingMethods;

#pragma region Symbol Creation

    void process_namespace(NamespaceDeclSyntax* nsDecl, NamespaceSymbol* parentNs);
    NamedTypeSymbol* create_type_symbol(TypeDeclSyntax* typeDecl, Symbol* parent);

#pragma region Method Binding

    void bind_method(MethodSymbol* method);
    TypeSymbol* get_return_type(MethodSymbol* method);

#pragma region Expression Binding

    TypeSymbol* bind_expr(BaseExprSyntax* expr);
    TypeSymbol* bind_identifier(IdentifierExprSyntax* expr);
    TypeSymbol* bind_literal(LiteralExprSyntax* expr);
    TypeSymbol* bind_binary(BinaryExprSyntax* expr);
    TypeSymbol* bind_unary(UnaryExprSyntax* expr);
    TypeSymbol* bind_assignment(AssignmentExprSyntax* expr);
    TypeSymbol* bind_call(CallExprSyntax* expr);
    TypeSymbol* bind_member_access(MemberAccessExprSyntax* expr);
    TypeSymbol* bind_initializer(InitializerExprSyntax* expr);
    TypeSymbol* bind_field_init_target(BaseExprSyntax* target, NamedTypeSymbol* type);
    TypeSymbol* bind_this(ThisExprSyntax* expr);
    TypeSymbol* bind_paren(ParenExprSyntax* expr);
    TypeSymbol* bind_block(BlockExprSyntax* expr);

#pragma region Statement Binding

    void bind_stmt(BaseStmtSyntax* stmt);
    void bind_return(ReturnStmtSyntax* stmt);
    void bind_var_decl(VariableDeclSyntax* decl);
    void bind_if(IfStmtSyntax* stmt);
    void bind_while(WhileStmtSyntax* stmt);

#pragma region Attribute Resolution

    void resolve_attributes(BaseDeclSyntax* decl, std::vector<ResolvedAttribute>& out);

#pragma region Type Resolution

    TypeSymbol* resolve_type_expr(BaseExprSyntax* expr);

#pragma region Helpers

    void push_scope();
    void pop_scope();
    Scope& current_scope();

    Symbol* resolve_name(std::string_view name);

    void create_parameters(MethodSymbol* method, const std::vector<ParameterDeclSyntax*>& params);
    void store_type(BaseExprSyntax* expr, TypeSymbol* type);
    void store_symbol(BaseExprSyntax* expr, Symbol* symbol);
};

}
