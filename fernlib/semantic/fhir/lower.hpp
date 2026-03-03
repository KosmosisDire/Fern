#pragma once

#include "fhir.hpp"
#include <vector>
#include <ast/ast.hpp>

namespace Fern
{

class AllocArena;
struct SemanticContext;
struct NamespaceSymbol;
struct NamedTypeSymbol;

class FhirLowerer
{
public:
    FhirLowerer(SemanticContext& ctx, AllocArena& arena);
    std::vector<FhirMethod*> lower_all();

private:
    SemanticContext& context;
    AllocArena& arena;
    std::vector<FhirMethod*> result;
    int tempCounter = 0;
    std::vector<FhirStmt*>* pendingStmts = nullptr;

    template<typename T>
    T* make();

#pragma region Top-level

    bool is_builtin_type(NamedTypeSymbol* type);
    void collect_methods(NamespaceSymbol* ns);
    FhirMethod* lower_method(MethodSymbol* method);
    FhirMethod* lower_synthetic_constructor(MethodSymbol* method, NamedTypeSymbol* parentType);

#pragma region Expressions

    FhirExpr* lower_expr(BaseExprSyntax* ast);
    FhirExpr* lower_literal(LiteralExprSyntax* ast);
    FhirExpr* lower_identifier(IdentifierExprSyntax* ast);
    FhirExpr* lower_this(ThisExprSyntax* ast);
    FhirExpr* lower_binary(BinaryExprSyntax* ast);
    FhirExpr* lower_unary(UnaryExprSyntax* ast);
    FhirExpr* lower_assignment(AssignmentExprSyntax* ast);
    FhirExpr* lower_initializer_target(InitializerExprSyntax* ast);
    FhirExpr* lower_initializer_expr(InitializerExprSyntax* ast);
    FhirExpr* build_field_access_chain(FhirExpr* receiver, BaseExprSyntax* target);
    void emit_field_inits(InitializerExprSyntax* ast, FhirExpr* receiver, std::vector<FhirStmt*>& out);
    FhirExpr* lower_call(CallExprSyntax* ast);
    FhirExpr* lower_member_access(MemberAccessExprSyntax* ast);
    FhirBlock* lower_block(BlockSyntax* ast);

#pragma region Statements

    void lower_stmt(BaseStmtSyntax* ast, std::vector<FhirStmt*>& out);
    void lower_var_decl(VariableDeclSyntax* ast, std::vector<FhirStmt*>& out);
    FhirStmt* lower_expr_stmt(ExpressionStmtSyntax* ast);
    FhirStmt* lower_return(ReturnStmtSyntax* ast);
    FhirStmt* lower_if(IfStmtSyntax* ast);
    FhirStmt* lower_while(WhileStmtSyntax* ast);
};

}
