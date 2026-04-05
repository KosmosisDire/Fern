#include "fold.hpp"
#include <arena.hpp>

namespace Fern
{

FhirConstantFolder::FhirConstantFolder(AllocArena& arena) : arena(arena) {}

template<typename T>
T* FhirConstantFolder::make()
{
    return arena.alloc<T>();
}

#pragma region Fold Helpers

bool FhirConstantFolder::try_fold_int_arithmetic(IntrinsicOp op, int64_t lhs, int64_t rhs, int64_t& out)
{
    switch (op)
    {
        case IntrinsicOp::Add: out = lhs + rhs; return true;
        case IntrinsicOp::Sub: out = lhs - rhs; return true;
        case IntrinsicOp::Mul: out = lhs * rhs; return true;
        case IntrinsicOp::Div:
            if (rhs == 0) return false;
            out = lhs / rhs;
            return true;
        default: return false;
    }
}

bool FhirConstantFolder::try_fold_int_comparison(IntrinsicOp op, int64_t lhs, int64_t rhs, bool& out)
{
    switch (op)
    {
        case IntrinsicOp::Greater:      out = lhs > rhs; return true;
        case IntrinsicOp::Less:         out = lhs < rhs; return true;
        case IntrinsicOp::GreaterEqual: out = lhs >= rhs; return true;
        case IntrinsicOp::LessEqual:    out = lhs <= rhs; return true;
        case IntrinsicOp::Equal:        out = lhs == rhs; return true;
        case IntrinsicOp::NotEqual:     out = lhs != rhs; return true;
        default: return false;
    }
}

bool FhirConstantFolder::try_fold_bool_logic(IntrinsicOp op, bool lhs, bool rhs, bool& out)
{
    switch (op)
    {
        case IntrinsicOp::And:      out = lhs && rhs; return true;
        case IntrinsicOp::Or:       out = lhs || rhs; return true;
        case IntrinsicOp::Equal:    out = lhs == rhs; return true;
        case IntrinsicOp::NotEqual: out = lhs != rhs; return true;
        default: return false;
    }
}

FhirExpr* FhirConstantFolder::try_fold_intrinsic(FhirIntrinsicExpr* node)
{
    if (node->args.size() == 2)
    {
        auto* lhs = node->args[0] ? node->args[0]->as<FhirLiteralExpr>() : nullptr;
        auto* rhs = node->args[1] ? node->args[1]->as<FhirLiteralExpr>() : nullptr;
        if (lhs && rhs &&
            lhs->value.kind == LiteralValue::Kind::Int &&
            rhs->value.kind == LiteralValue::Kind::Int)
        {
            int64_t intResult;
            if (try_fold_int_arithmetic(node->op, lhs->value.intValue, rhs->value.intValue, intResult))
            {
                auto* lit = make<FhirLiteralExpr>();
                lit->span = node->span;
                lit->type = node->type;
                lit->value = LiteralValue::make_int(intResult);
                return lit;
            }

            bool boolResult;
            if (try_fold_int_comparison(node->op, lhs->value.intValue, rhs->value.intValue, boolResult))
            {
                auto* lit = make<FhirLiteralExpr>();
                lit->span = node->span;
                lit->type = node->type;
                lit->value = LiteralValue::make_bool(boolResult);
                return lit;
            }
        }

        if (lhs && rhs &&
            lhs->value.kind == LiteralValue::Kind::Bool &&
            rhs->value.kind == LiteralValue::Kind::Bool)
        {
            bool boolResult;
            if (try_fold_bool_logic(node->op, lhs->value.boolValue, rhs->value.boolValue, boolResult))
            {
                auto* lit = make<FhirLiteralExpr>();
                lit->span = node->span;
                lit->type = node->type;
                lit->value = LiteralValue::make_bool(boolResult);
                return lit;
            }
        }
    }

    if (node->args.size() == 1)
    {
        auto* operand = node->args[0] ? node->args[0]->as<FhirLiteralExpr>() : nullptr;
        if (operand && operand->value.kind == LiteralValue::Kind::Int)
        {
            if (node->op == IntrinsicOp::Negative)
            {
                auto* lit = make<FhirLiteralExpr>();
                lit->span = node->span;
                lit->type = node->type;
                lit->value = LiteralValue::make_int(-operand->value.intValue);
                return lit;
            }

            if (node->op == IntrinsicOp::Positive)
                return operand;
        }
        else if (operand && operand->value.kind == LiteralValue::Kind::Bool)
        {
            if (node->op == IntrinsicOp::Not)
            {
                auto* lit = make<FhirLiteralExpr>();
                lit->span = node->span;
                lit->type = node->type;
                lit->value = LiteralValue::make_bool(!operand->value.boolValue);
                return lit;
            }
        }
    }

    return nullptr;
}

#pragma region Expression Folding

FhirExpr* FhirConstantFolder::fold_expr(FhirExpr* expr)
{
    if (!expr) return nullptr;

    if (auto* node = expr->as<FhirIntrinsicExpr>())
    {
        for (auto*& arg : node->args)
            arg = fold_expr(arg);

        if (auto* folded = try_fold_intrinsic(node))
            return folded;

        return node;
    }

    if (auto* node = expr->as<FhirFieldAccessExpr>())
    {
        node->object = fold_expr(node->object);
        return node;
    }

    if (auto* node = expr->as<FhirCallExpr>())
    {
        for (auto*& arg : node->arguments)
            arg = fold_expr(arg);
        return node;
    }

    if (auto* node = expr->as<FhirMethodCallExpr>())
    {
        node->receiver = fold_expr(node->receiver);
        for (auto*& arg : node->arguments)
            arg = fold_expr(arg);
        return node;
    }

    if (auto* node = expr->as<FhirObjectCreateExpr>())
    {
        for (auto*& arg : node->arguments)
            arg = fold_expr(arg);
        return node;
    }

    if (auto* node = expr->as<FhirAssignExpr>())
    {
        node->target = fold_expr(node->target);
        node->value = fold_expr(node->value);
        return node;
    }

    return expr;
}

#pragma region Statement Folding

void FhirConstantFolder::fold_stmt(FhirStmt* stmt)
{
    if (!stmt) return;

    if (auto* node = stmt->as<FhirVarDeclStmt>())
    {
        node->initializer = fold_expr(node->initializer);
    }
    else if (auto* node = stmt->as<FhirExprStmt>())
    {
        node->expression = fold_expr(node->expression);
    }
    else if (auto* node = stmt->as<FhirReturnStmt>())
    {
        node->value = fold_expr(node->value);
    }
    else if (auto* node = stmt->as<FhirIfStmt>())
    {
        node->condition = fold_expr(node->condition);
        if (node->thenBlock) fold_block(node->thenBlock);
        if (node->elseIf) fold_stmt(node->elseIf);
        if (node->elseBlock) fold_block(node->elseBlock);
    }
    else if (auto* node = stmt->as<FhirWhileStmt>())
    {
        node->condition = fold_expr(node->condition);
        if (node->body) fold_block(node->body);
    }
}

void FhirConstantFolder::fold_block(FhirBlock* block)
{
    if (!block) return;
    for (auto* stmt : block->statements)
        fold_stmt(stmt);
}

#pragma region Public

void FhirConstantFolder::fold(FhirMethod* method, AllocArena& arena)
{
    if (!method || !method->body) return;
    FhirConstantFolder folder(arena);
    folder.fold_block(method->body);
}

}
