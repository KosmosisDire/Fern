#pragma once

#include <vector>

#include "scope.hpp"
#include "fhir_builder.hpp"
#include <common/diagnostic.hpp>
#include <token/kind.hpp>

namespace Fern
{

struct SemanticContext;
struct ResolvedAttribute;

struct RootSyntax;
struct BaseDeclSyntax;
struct BaseExprSyntax;
struct BaseStmtSyntax;
struct AssignmentExprSyntax;
struct BinaryExprSyntax;
struct BlockSyntax;
struct CallExprSyntax;
struct IdentifierExprSyntax;
struct IfStmtSyntax;
struct InitializerExprSyntax;
struct LiteralExprSyntax;
struct MemberAccessExprSyntax;
struct NamespaceDeclSyntax;
struct CallableDeclSyntax;
struct ParameterDeclSyntax;
struct ParenExprSyntax;
struct ReturnStmtSyntax;
struct ThisExprSyntax;
struct TypeDeclSyntax;
struct TypeExprSyntax;
struct GenericTypeExprSyntax;
struct IndexExprSyntax;
struct ArrayLiteralExprSyntax;
struct UnaryExprSyntax;
struct FieldDeclSyntax;
struct VariableDeclSyntax;
struct WhileStmtSyntax;

struct Symbol;
struct MethodSymbol;
struct NamedTypeSymbol;
struct NamespaceSymbol;
struct TypeSymbol;

struct FhirExpr;
struct FhirBlock;
struct FhirStmt;
struct FhirMethod;

// The binder runs in phases, each must be called in order:
//   1. bind_ast              - Create type/method/field symbols from the AST
//   2. resolve_all_types     - Resolve type references on fields, parameters, return types
//   3. resolve_all_attributes - Resolve attribute annotations to their types
//   4. bind_all_methods      - Produce FHIR for all method bodies
//   5. validate_all_types    - Cross-method checks (duplicate signatures, operator rules)
class Binder : public DiagnosticSystem
{
public:
    Binder(SemanticContext& context, AllocArena& arena);

    void bind_ast(RootSyntax* ast);
    void resolve_all_types();
    void validate_all_types();
    void resolve_all_attributes();
    void bind_all_methods();

private:
    SemanticContext& context;
    AllocArena& arena;
    FhirBuilder fhir;

    MethodSymbol* currentMethod = nullptr;
    NamedTypeSymbol* currentType = nullptr;
    NamespaceSymbol* currentNamespace = nullptr;
    std::vector<Scope> scopes;
    std::vector<NamedTypeSymbol*> allTypes;
    std::vector<MethodSymbol*> allMethods;
    std::unordered_map<std::string, TypeSymbol*> typeParamSubstitutions;
    // Points to the current block's statement list so that initializer lowering
    // can inject setup statements (temp locals, field assignments) before the expression
    std::vector<FhirStmt*>* pendingStmts = nullptr;
    int tempCounter = 0;

#pragma region Symbol Creation

    void process_namespace(NamespaceDeclSyntax* nsDecl, NamespaceSymbol* parentNs);
    NamedTypeSymbol* create_type_symbol(TypeDeclSyntax* typeDecl, Symbol* parent);

#pragma region Method Binding

    void bind_method(MethodSymbol* method);
    void lower_synthetic_constructor(MethodSymbol* method, NamedTypeSymbol* parentType);
    void emit_field_defaults(NamedTypeSymbol* type, std::vector<FhirStmt*>& out);
    void check_duplicate_methods(NamedTypeSymbol* type);

#pragma region Expression Binding

    FhirExpr* bind_expr(BaseExprSyntax* expr);
    FhirExpr* bind_identifier(IdentifierExprSyntax* expr);
    FhirExpr* bind_literal(LiteralExprSyntax* expr);
    FhirExpr* bind_binary(BinaryExprSyntax* expr);
    FhirExpr* bind_binary_op(BinaryOp op, FhirExpr* lhs, FhirExpr* rhs, BaseExprSyntax* syntax);
    FhirExpr* try_synthesize_compound_comparison(BinaryOp op, TokenKind opToken, NamedTypeSymbol* namedType, TypeSymbol* leftType, TypeSymbol* rightType, FhirExpr* lhs, FhirExpr* rhs, BaseExprSyntax* syntax);
    FhirExpr* bind_unary(UnaryExprSyntax* expr);
    FhirExpr* bind_assignment(AssignmentExprSyntax* expr);
    FhirExpr* bind_call(CallExprSyntax* expr);
    FhirExpr* bind_member_access(MemberAccessExprSyntax* expr);
    FhirExpr* bind_initializer(InitializerExprSyntax* expr);
    FhirExpr* bind_initializer_target(InitializerExprSyntax* expr);
    FhirExpr* build_field_access_chain(FhirExpr* receiver, BaseExprSyntax* target);
    void bind_initializer_fields(InitializerExprSyntax* expr, NamedTypeSymbol* namedType, std::vector<FhirStmt*>& out, FhirExpr* receiver);
    TypeSymbol* bind_field_init_target(BaseExprSyntax* target, NamedTypeSymbol* type);
    FhirExpr* bind_this(ThisExprSyntax* expr);
    FhirExpr* bind_paren(ParenExprSyntax* expr);
    FhirExpr* bind_generic_type_expr(GenericTypeExprSyntax* expr);
    FhirExpr* bind_index(IndexExprSyntax* expr);
    FhirExpr* bind_array_literal(ArrayLiteralExprSyntax* expr);

#pragma region Statement Binding

    FhirBlock* bind_block(BlockSyntax* block);

    void bind_stmt(BaseStmtSyntax* stmt, std::vector<FhirStmt*>& out);
    void bind_return(ReturnStmtSyntax* stmt, std::vector<FhirStmt*>& out);
    void bind_var_decl(VariableDeclSyntax* decl, std::vector<FhirStmt*>& out);
    void bind_if(IfStmtSyntax* stmt, std::vector<FhirStmt*>& out);
    void bind_while(WhileStmtSyntax* stmt, std::vector<FhirStmt*>& out);

#pragma region Attribute Resolution

    void resolve_attributes(BaseDeclSyntax* decl, std::vector<ResolvedAttribute>& out);

#pragma region Type Resolution

    TypeSymbol* resolve_type_expr(BaseExprSyntax* expr);
    TypeSymbol* resolve_generic_type(GenericTypeExprSyntax* expr);
#pragma region Helpers

    void push_scope();
    void pop_scope();
    Scope& current_scope();

    Symbol* resolve_name(std::string_view name);
    Symbol* resolve_expr_symbol(BaseExprSyntax* expr);

    void create_parameters(MethodSymbol* method, const std::vector<ParameterDeclSyntax*>& params);
};

}
