#pragma once

#include <vector>

#include "fhir_builder.hpp"
#include <common/diagnostic.hpp>
#include <token/kind.hpp>

namespace Fern
{

struct SemanticContext;
struct ResolvedAttribute;
struct Scope;

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
struct CallableDeclSyntax;
struct ParenExprSyntax;
struct CastExprSyntax;
struct ReturnStmtSyntax;
struct ThisExprSyntax;
struct GenericTypeExprSyntax;
struct ArrayTypeExprSyntax;
struct IndexExprSyntax;
struct ArrayLiteralExprSyntax;
struct UnaryExprSyntax;
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

// Base class for every binder in the chain. Holds all scope-agnostic binding
// logic and reaches scope-specific state through virtual accessors. Concrete
// subclasses contribute their scope via lookup_in_single_binder and optional
// accessor overrides. Chain terminator is RootBinder.
class Binder
{
public:
    Binder(SemanticContext& context, AllocArena& arena);
    virtual ~Binder() = default;

    Symbol* lookup(std::string_view name);
    Symbol* lookup(BaseExprSyntax* expr);
    FhirBlock* bind_block(BlockSyntax* block);
    void bind_stmt(BaseStmtSyntax* stmt, std::vector<FhirStmt*>& out);
    void resolve_attributes(BaseDeclSyntax* decl, std::vector<ResolvedAttribute>& out);
    void emit_field_defaults(NamedTypeSymbol* type, std::vector<FhirStmt*>& out);
    TypeSymbol* resolve_type_expr(BaseExprSyntax* expr);
    TypeSymbol* resolve_generic_type(GenericTypeExprSyntax* expr);

protected:
    explicit Binder(Binder& parent);

    Binder* next = nullptr;

    SemanticContext& context;
    AllocArena& arena;
    Diagnostics& diag;
    FhirBuilder fhir;

    virtual MethodSymbol* containing_method() { return next ? next->containing_method() : nullptr; }
    virtual NamedTypeSymbol* containing_type() { return next ? next->containing_type() : nullptr; }
    virtual NamespaceSymbol* containing_namespace() { return next ? next->containing_namespace() : nullptr; }
    virtual const std::unordered_map<std::string, TypeSymbol*>* type_param_substitutions() { return next ? next->type_param_substitutions() : nullptr; }
    virtual std::vector<FhirStmt*>* pending_statements() { return next ? next->pending_statements() : nullptr; }
    virtual int* temp_counter() { return next ? next->temp_counter() : nullptr; }
    virtual Scope* current_block_scope() { return next ? next->current_block_scope() : nullptr; }

    virtual Symbol* lookup_in_single_binder(std::string_view name) = 0;

    static bool extract_type_path(BaseExprSyntax* expr, std::vector<std::string_view>& path);

#pragma region Statement Binding

    void bind_var_decl(VariableDeclSyntax* decl, std::vector<FhirStmt*>& out);
    void bind_return(ReturnStmtSyntax* stmt, std::vector<FhirStmt*>& out);
    void bind_if(IfStmtSyntax* stmt, std::vector<FhirStmt*>& out);
    void bind_while(WhileStmtSyntax* stmt, std::vector<FhirStmt*>& out);

#pragma region Expression Binding

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

#pragma region Literal Binding

    FhirExpr* bind_literal(LiteralExprSyntax* expr);
    FhirExpr* bind_suffixed_literal(LiteralSuffixExprSyntax* expr, TypeSymbol* expected = nullptr);
    MethodSymbol* resolve_literal_suffix(std::string_view suffixName, TypeSymbol* argType, TypeSymbol* expected, const Span& span);
    std::string process_escape_sequences(std::string_view raw, const Span& span);
    FhirExpr* bind_array_literal(ArrayLiteralExprSyntax* expr, TypeSymbol* expected = nullptr);

#pragma region Call Binding

    FhirExpr* bind_call(CallExprSyntax* expr);

#pragma region Initializer Binding

    FhirExpr* bind_initializer(InitializerExprSyntax* expr);
    FhirExpr* bind_initializer_target(InitializerExprSyntax* expr);
    FhirExpr* build_field_access_chain(FhirExpr* receiver, BaseExprSyntax* target);
    void bind_initializer_fields(InitializerExprSyntax* expr, NamedTypeSymbol* namedType, std::vector<FhirStmt*>& out, FhirExpr* receiver);
    TypeSymbol* bind_field_init_target(BaseExprSyntax* target, NamedTypeSymbol* type);

};

}
