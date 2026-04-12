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
struct CastExprSyntax;
struct ReturnStmtSyntax;
struct ThisExprSyntax;
struct TypeDeclSyntax;
struct GenericTypeExprSyntax;
struct ArrayTypeExprSyntax;
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
    void resolve_all_attributes();
    void bind_all_methods();
    void validate_all_types();

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
    std::unordered_map<std::string, std::vector<MethodSymbol*>> literalSuffixMap;
    std::unordered_map<std::string, TypeSymbol*> typeParamSubstitutions;
    // Points to the current block's statement list so that initializer lowering
    // can inject setup statements (temp locals, field assignments) before the expression
    std::vector<FhirStmt*>* pendingStmts = nullptr;
    int tempCounter = 0;

    // Symbol Definition

    void define_namespace(NamespaceDeclSyntax* nsDecl, NamespaceSymbol* parentNs);
    NamedTypeSymbol* define_type(TypeDeclSyntax* typeDecl, Symbol* parent);

    // Type Expression Resolution

    TypeSymbol* resolve_type_expr(BaseExprSyntax* expr);
    TypeSymbol* resolve_generic_type(GenericTypeExprSyntax* expr);

    // Type Validation

    void check_duplicate_methods(NamedTypeSymbol* type);

    // Attribute Resolution

    void resolve_attributes(BaseDeclSyntax* decl, std::vector<ResolvedAttribute>& out);
    
    // Method Body Binding

    void bind_method(MethodSymbol* method);
    void lower_synthetic_constructor(MethodSymbol* method, NamedTypeSymbol* parentType);
    void emit_field_defaults(NamedTypeSymbol* type, std::vector<FhirStmt*>& out);

    // Statement Binding

    FhirBlock* bind_block(BlockSyntax* block);
    void bind_stmt(BaseStmtSyntax* stmt, std::vector<FhirStmt*>& out);
    void bind_return(ReturnStmtSyntax* stmt, std::vector<FhirStmt*>& out);
    void bind_var_decl(VariableDeclSyntax* decl, std::vector<FhirStmt*>& out);
    void bind_if(IfStmtSyntax* stmt, std::vector<FhirStmt*>& out);
    void bind_while(WhileStmtSyntax* stmt, std::vector<FhirStmt*>& out);

    // Expression Binding

    FhirExpr* bind_expr(BaseExprSyntax* expr, TypeSymbol* expected = nullptr);
    FhirExpr* bind_value_expr(BaseExprSyntax* expr, TypeSymbol* expected = nullptr);
    FhirCastExpr* try_implicit_cast(FhirExpr* expr, TypeSymbol* targetType, const Span& span);
    FhirExpr* coerce_to_param(FhirExpr* arg, TypeSymbol* paramType);
    FhirExpr* bind_identifier(IdentifierExprSyntax* expr);
    FhirExpr* bind_this(ThisExprSyntax* expr);
    FhirExpr* bind_paren(ParenExprSyntax* expr, TypeSymbol* expected = nullptr);
    FhirExpr* bind_cast(CastExprSyntax* expr);
    FhirExpr* bind_generic_type_expr(GenericTypeExprSyntax* expr);
    FhirExpr* bind_member_access(MemberAccessExprSyntax* expr);
    FhirExpr* bind_unary(UnaryExprSyntax* expr);
    FhirExpr* bind_binary(BinaryExprSyntax* expr);
    FhirExpr* bind_binary_op(BinaryOp op, FhirExpr* lhs, FhirExpr* rhs, BaseExprSyntax* syntax);
    FhirExpr* try_synthesize_compound_comparison(BinaryOp op, TokenKind opToken, NamedTypeSymbol* namedType, TypeSymbol* leftType, TypeSymbol* rightType, FhirExpr* lhs, FhirExpr* rhs, BaseExprSyntax* syntax);
    FhirExpr* bind_assignment(AssignmentExprSyntax* expr);
    FhirExpr* bind_index(IndexExprSyntax* expr);

    // Literal Binding

    FhirExpr* bind_literal(LiteralExprSyntax* expr);
    FhirExpr* bind_suffixed_literal(LiteralSuffixExprSyntax* expr, TypeSymbol* expected = nullptr);
    MethodSymbol* resolve_literal_suffix(std::string_view suffixName, TypeSymbol* argType, TypeSymbol* expected, const Span& span);
    std::string process_escape_sequences(std::string_view raw, const Span& span);
    FhirExpr* bind_array_literal(ArrayLiteralExprSyntax* expr, TypeSymbol* expected = nullptr);

    // Call Binding

    FhirExpr* bind_call(CallExprSyntax* expr);

    // Initializer Binding

    FhirExpr* bind_initializer(InitializerExprSyntax* expr);
    FhirExpr* bind_initializer_target(InitializerExprSyntax* expr);
    FhirExpr* build_field_access_chain(FhirExpr* receiver, BaseExprSyntax* target);
    void bind_initializer_fields(InitializerExprSyntax* expr, NamedTypeSymbol* namedType, std::vector<FhirStmt*>& out, FhirExpr* receiver);
    TypeSymbol* bind_field_init_target(BaseExprSyntax* target, NamedTypeSymbol* type);

    // Scope and Name Resolution

    void push_scope();
    void pop_scope();
    Scope& current_scope();
    Symbol* resolve_name(std::string_view name);
    Symbol* resolve_expr_symbol(BaseExprSyntax* expr);

    static bool extract_type_path(BaseExprSyntax* expr, std::vector<std::string_view>& path);
};

}
