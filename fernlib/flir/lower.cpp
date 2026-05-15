#include "lower.hpp"

#include <semantic/context.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

#pragma region Construction

FlirLowerer::FlirLowerer(SemanticContext& semantic, FlirContext& flir)
    : semantic(semantic)
    , flir(flir)
    , builder(flir.arena)
{
}

#pragma region Method Entry

FlirMethod* FlirLowerer::lower_method(FhirMethod* method)
{
    if (!method) return nullptr;

    currentMethod = builder.method(method->symbol, nullptr);

    if (method->symbol)
    {
        auto* parentType = method->symbol->parent ? method->symbol->parent->as<NamedTypeSymbol>() : nullptr;
        bool isStatic = has_modifier(method->symbol->modifiers, Modifier::Static);
        if (parentType && (method->symbol->is_constructor() || !isStatic))
        {
            builder.param(currentMethod, "this", parentType);
        }

        for (auto* param : method->symbol->parameters)
        {
            auto* slot = builder.param(currentMethod, param->name, param->type);
            flir.slots[param] = slot;
        }
    }

    currentMethod->body = lower_block(method->body);

    FlirMethod* result = currentMethod;
    currentMethod = nullptr;
    return result;
}

#pragma region Blocks and Statements

FlirBlock* FlirLowerer::lower_block(FhirBlock* block)
{
    if (!block) return nullptr;

    auto* result = builder.block(block->syntax);
    for (auto* stmt : block->statements)
        lower_stmt(stmt, result->statements);
    return result;
}

void FlirLowerer::lower_stmt(FhirStmt* stmt, std::vector<FlirStmt*>& out)
{
    if (!stmt) return;

    if (auto* s = stmt->as<FhirVarDeclStmt>())    return lower_var_decl(s, out);
    if (auto* s = stmt->as<FhirExprStmt>())       return lower_expr_stmt(s, out);
    if (auto* s = stmt->as<FhirReturnStmt>())     return lower_return(s, out);
    if (auto* s = stmt->as<FhirIfStmt>())         return lower_if(s, out);
    if (auto* s = stmt->as<FhirWhileStmt>())      return lower_while(s, out);
}

void FlirLowerer::lower_var_decl(FhirVarDeclStmt* stmt, std::vector<FlirStmt*>& out)
{
    if (!stmt->local) return;
    auto* slot = builder.local(currentMethod, stmt->local->name, stmt->local->type);
    flir.slots[stmt->local] = slot;
    if (stmt->initializer)
    {
        auto* target = builder.load_local(stmt->syntax, slot);
        auto* value = lower_expr(stmt->initializer);
        out.push_back(builder.assign(stmt->syntax, target, value));
    }
}

void FlirLowerer::lower_expr_stmt(FhirExprStmt* stmt, std::vector<FlirStmt*>& out)
{
    auto* assign = stmt->expression ? stmt->expression->as<FhirAssignExpr>() : nullptr;
    if (assign)
    {
        auto* target = lower_expr(assign->target);
        auto* value = lower_expr(assign->value);
        out.push_back(builder.assign(assign->syntax, target, value));
        return;
    }
    out.push_back(builder.expr_stmt(stmt->syntax, lower_expr(stmt->expression)));
}

void FlirLowerer::lower_return(FhirReturnStmt* stmt, std::vector<FlirStmt*>& out)
{
    out.push_back(builder.return_stmt(stmt->syntax, lower_expr(stmt->value)));
}

void FlirLowerer::lower_if(FhirIfStmt* stmt, std::vector<FlirStmt*>& out)
{
    auto* condition = lower_expr(stmt->condition);
    auto* thenBlock = lower_block(stmt->thenBlock);

    FlirBlock* elseBlock = nullptr;
    if (stmt->elseIf)
    {
        elseBlock = builder.block(stmt->elseIf->syntax);
        lower_if(stmt->elseIf, elseBlock->statements);
    }
    else if (stmt->elseBlock)
    {
        elseBlock = lower_block(stmt->elseBlock);
    }

    out.push_back(builder.if_stmt(stmt->syntax, condition, thenBlock, elseBlock));
}

void FlirLowerer::lower_while(FhirWhileStmt* stmt, std::vector<FlirStmt*>& out)
{
    auto* cond = lower_expr(stmt->condition);
    auto* condType = cond ? cond->type : nullptr;
    auto* notCond = builder.intrinsic(stmt->syntax, condType, IntrinsicOp::Not, { cond });

    auto* breakBlock = builder.block(stmt->syntax);
    breakBlock->statements.push_back(builder.break_stmt(stmt->syntax));

    auto* bodySyntax = stmt->body ? stmt->body->syntax : stmt->syntax;
    auto* loopBody = builder.block(bodySyntax);
    loopBody->statements.push_back(builder.if_stmt(stmt->syntax, notCond, breakBlock, nullptr));

    if (stmt->body)
    {
        for (auto* s : stmt->body->statements)
            lower_stmt(s, loopBody->statements);
    }

    out.push_back(builder.loop(stmt->syntax, loopBody));
}

#pragma region Expressions

FlirExpr* FlirLowerer::lower_expr(FhirExpr* expr)
{
    if (!expr) return nullptr;

    if (auto* e = expr->as<FhirLiteralExpr>())      return lower_literal(e);
    if (auto* e = expr->as<FhirLocalRefExpr>())     return lower_local_ref(e);
    if (auto* e = expr->as<FhirParamRefExpr>())     return lower_param_ref(e);
    if (auto* e = expr->as<FhirFieldRefExpr>())     return lower_field_ref(e);
    if (auto* e = expr->as<FhirThisExpr>())         return lower_this(e);
    if (auto* e = expr->as<FhirOpExpr>())           return lower_op(e);
    if (auto* e = expr->as<FhirCallExpr>())         return lower_call(e);
    if (auto* e = expr->as<FhirConstructionExpr>()) return lower_construction(e);
    if (auto* e = expr->as<FhirAssignExpr>())       return lower_assign(e);
    if (auto* e = expr->as<FhirCastExpr>())         return lower_cast(e);

    return nullptr;
}

FlirExpr* FlirLowerer::lower_literal(FhirLiteralExpr* expr)
{
    return builder.constant(expr->syntax, expr->type, expr->value);
}

FlirExpr* FlirLowerer::lower_local_ref(FhirLocalRefExpr* expr)
{
    return builder.load_local(expr->syntax, flir.lookup_local_symbol(expr->local));
}

FlirExpr* FlirLowerer::lower_param_ref(FhirParamRefExpr* expr)
{
    return builder.load_local(expr->syntax, flir.lookup_param_symbol(expr->parameter));
}

FlirExpr* FlirLowerer::lower_field_ref(FhirFieldRefExpr* expr)
{
    auto* base = lower_expr(expr->thisRef);
    return builder.load_field(expr->syntax, base, expr->field);
}

FlirExpr* FlirLowerer::lower_this(FhirThisExpr* expr)
{
    if (!currentMethod || currentMethod->parameters.empty()) return nullptr;
    return builder.load_local(expr->syntax, currentMethod->parameters[0]);
}

FlirExpr* FlirLowerer::lower_op(FhirOpExpr* expr)
{
    std::vector<FlirExpr*> args;
    args.reserve(expr->args.size());
    for (auto* a : expr->args)
        args.push_back(lower_expr(a));

    if (expr->method && !expr->method->is_intrinsic())
    {
        return builder.call(expr->syntax, expr->type, expr->method, nullptr, std::move(args));
    }
    return builder.intrinsic(expr->syntax, expr->type, expr->op, std::move(args));
}

FlirExpr* FlirLowerer::lower_call(FhirCallExpr* expr)
{
    auto* callee = expr->callee;
    MethodSymbol* method = callee ? callee->method : nullptr;
    FlirExpr* thisArg = callee ? lower_expr(callee->thisRef) : nullptr;

    std::vector<FlirExpr*> args;
    args.reserve(expr->arguments.size());
    for (auto* a : expr->arguments)
        args.push_back(lower_expr(a));

    return builder.call(expr->syntax, expr->type, method, thisArg, std::move(args));
}

FlirExpr* FlirLowerer::lower_construction(FhirConstructionExpr* expr)
{
    if (!expr->call || !expr->call->callee) return nullptr;

    auto* ctor = expr->call->callee->method;
    auto* type = expr->type;

    auto* alloc = builder.alloc_expr(expr->syntax, type);

    std::vector<FlirExpr*> args;
    args.reserve(expr->call->arguments.size());
    for (auto* a : expr->call->arguments)
        args.push_back(lower_expr(a));

    return builder.call(expr->syntax, type, ctor, alloc, std::move(args));
}

// FhirAssignExpr yields the assigned value; FlirAssign is a statement with no
// value. The common case (assign in statement position) is handled directly in
// lower_expr_stmt. Assign as a value expression needs a stmt out-stream on the
// lowerer to emit the side-effect.
FlirExpr* FlirLowerer::lower_assign(FhirAssignExpr*) { return nullptr; }

FlirExpr* FlirLowerer::lower_cast(FhirCastExpr* expr)
{
    auto* operand = lower_expr(expr->operand);
    if (expr->method && !expr->method->is_intrinsic())
    {
        return builder.call(expr->syntax, expr->type, expr->method, nullptr, { operand });
    }
    return builder.cast(expr->syntax, expr->type, operand);
}

#pragma region Local Helpers

}
