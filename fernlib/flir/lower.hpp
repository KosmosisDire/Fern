#pragma once

#include <vector>

#include <semantic/fhir/fhir.hpp>
#include <flir/builder.hpp>
#include <flir/context.hpp>
#include <flir/flir.hpp>

namespace Fern
{

struct SemanticContext;

// Lowers FHIR to FLIR. Resolves ref nodes, makes implicit casts explicit,
// normalizes control flow, and splits construction into alloc plus ctor call.
// All entry points are stubs at the moment.
class FlirLowerer
{
public:
    FlirLowerer(SemanticContext& semantic, FlirContext& flir);

    FlirMethod* lower_method(FhirMethod* method);

private:
    SemanticContext& semantic;
    FlirContext& flir;
    FlirBuilder builder;

    FlirMethod* currentMethod = nullptr;

    FlirBlock* lower_block(FhirBlock* block);
    void lower_stmt(FhirStmt* stmt, std::vector<FlirStmt*>& out);
    FlirExpr* lower_expr(FhirExpr* expr);

    FlirExpr* lower_literal(FhirLiteralExpr* expr);
    FlirExpr* lower_local_ref(FhirLocalRefExpr* expr);
    FlirExpr* lower_param_ref(FhirParamRefExpr* expr);
    FlirExpr* lower_field_ref(FhirFieldRefExpr* expr);
    FlirExpr* lower_this(FhirThisExpr* expr);
    FlirExpr* lower_op(FhirOpExpr* expr);
    FlirExpr* lower_short_circuit(FhirOpExpr* expr);
    FlirExpr* lower_call(FhirCallExpr* expr);
    FlirExpr* lower_construction(FhirConstructionExpr* expr);
    FlirExpr* lower_assign(FhirAssignExpr* expr);
    FlirExpr* lower_compound_assign(FhirCompoundAssignExpr* expr);
    FlirExpr* lower_cast(FhirCastExpr* expr);
    FlirExpr* lower_index(FhirIndexExpr* expr);
    FlirExpr* lower_object_builder(FhirObjectBuilderExpr* expr);
    FlirExpr* lower_array_literal(FhirArrayLiteralExpr* expr);

    void lower_store(FhirExpr* target, FlirExpr* value, BaseSyntax* syntax, std::vector<FlirStmt*>& out);

    FlirExpr* address_load(BaseSyntax* syntax, FlirExpr* address, TypeSymbol* type);
    FlirExpr* read_slot(BaseSyntax* syntax, FlirLocal* slot);
    void emit_assign(BaseSyntax* syntax, FlirExpr* destAddr, FlirExpr* value, TypeSymbol* type, std::vector<FlirStmt*>& out);
    FlirExpr* build_call(BaseSyntax* syntax, TypeSymbol* retType, MethodSymbol* method, FlirExpr* thisArg, std::vector<FlirExpr*> args);
    FlirExpr* apply_bin(BaseSyntax* syntax, TypeSymbol* type, FhirOpExpr* binaryOp, FlirExpr* lhs, FlirExpr* rhs);
    void caller_copy_args(BaseSyntax* syntax, std::vector<FlirExpr*>& args, std::vector<FlirStmt*>& out);

    //TODO: Is there a better way to pass statements that an out, like with a sequence expr?
    void lower_var_decl(FhirVarDeclStmt* stmt, std::vector<FlirStmt*>& out);
    void lower_expr_stmt(FhirExprStmt* stmt, std::vector<FlirStmt*>& out);
    void lower_assign_stmt(FhirAssignExpr* assign, std::vector<FlirStmt*>& out);
    void lower_return(FhirReturnStmt* stmt, std::vector<FlirStmt*>& out);
    void lower_if(FhirIfStmt* stmt, std::vector<FlirStmt*>& out);
    void lower_while(FhirWhileStmt* stmt, std::vector<FlirStmt*>& out);

};

}
