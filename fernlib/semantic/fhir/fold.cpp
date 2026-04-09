#include "fold.hpp"
#include <arena.hpp>
#include <symbol/symbol.hpp>

namespace Fern
{

FhirConstantFolder::FhirConstantFolder(AllocArena& arena) : arena(arena) {}

template<typename T>
T* FhirConstantFolder::make()
{
    return arena.alloc<T>();
}

#pragma region Expression Folding

FhirExpr* FhirConstantFolder::fold_expr(FhirExpr* expr)
{
    if (!expr) return nullptr;

    if (auto* node = expr->as<FhirLocalRefExpr>())
    {
        auto it = constants.find(node->local);
        if (it != constants.end())
        {
            auto* lit = make<FhirLiteralExpr>();
            lit->span = node->span;
            lit->type = node->type;
            lit->value = it->second;
            return lit;
        }
        return node;
    }

    if (auto* node = expr->as<FhirIntrinsicExpr>())
    {
        for (auto*& arg : node->args)
            arg = fold_expr(arg);

        if (auto result = try_evaluate_constant_int(node))
        {
            auto* lit = make<FhirLiteralExpr>();
            lit->span = node->span;
            lit->type = node->type;
            lit->value = LiteralValue::make_int(*result);
            return lit;
        }
        if (auto result = try_evaluate_constant_float(node))
        {
            auto* lit = make<FhirLiteralExpr>();
            lit->span = node->span;
            lit->type = node->type;
            lit->value = LiteralValue::make_float(*result);
            return lit;
        }
        if (auto result = try_evaluate_constant_bool(node))
        {
            auto* lit = make<FhirLiteralExpr>();
            lit->span = node->span;
            lit->type = node->type;
            lit->value = LiteralValue::make_bool(*result);
            return lit;
        }

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
        node->value = fold_expr(node->value);
        return node;
    }

    return expr;
}

#pragma region If/While Folding

void FhirConstantFolder::fold_if(FhirIfStmt* stmt)
{
    stmt->condition = fold_expr(stmt->condition);

    bool condTrue = false;
    bool condFalse = false;
    if (auto* lit = stmt->condition->as<FhirLiteralExpr>())
    {
        if (lit->value.kind == LiteralValue::Kind::Bool)
        {
            condTrue = lit->value.boolValue;
            condFalse = !lit->value.boolValue;
        }
    }

    if (condTrue)
    {
        fold_block(stmt->thenBlock);
        auto afterThen = constants;

        constants.clear();
        if (stmt->elseIf) fold_if(stmt->elseIf);
        if (stmt->elseBlock) fold_block(stmt->elseBlock);

        constants = afterThen;
    }
    else if (condFalse)
    {
        auto saved = constants;

        constants.clear();
        fold_block(stmt->thenBlock);

        constants = saved;
        if (stmt->elseIf) fold_if(stmt->elseIf);
        else if (stmt->elseBlock) fold_block(stmt->elseBlock);
    }
    else
    {
        constants.clear();
        fold_block(stmt->thenBlock);

        constants.clear();
        if (stmt->elseIf) fold_if(stmt->elseIf);
        if (stmt->elseBlock) fold_block(stmt->elseBlock);

        constants.clear();
    }
}

void FhirConstantFolder::fold_while(FhirWhileStmt* stmt)
{
    stmt->condition = fold_expr(stmt->condition);

    constants.clear();
    fold_block(stmt->body);
    constants.clear();
}

#pragma region Statement Folding

void FhirConstantFolder::fold_stmt(FhirStmt* stmt)
{
    if (!stmt) return;

    if (auto* node = stmt->as<FhirVarDeclStmt>())
    {
        node->initializer = fold_expr(node->initializer);
        if (node->local && node->initializer)
        {
            if (auto* lit = node->initializer->as<FhirLiteralExpr>())
            {
                constants[node->local] = lit->value;
            }
        }
    }
    else if (auto* node = stmt->as<FhirExprStmt>())
    {
        node->expression = fold_expr(node->expression);
        if (auto* assign = node->expression ? node->expression->as<FhirAssignExpr>() : nullptr)
        {
            if (auto* localRef = assign->target->as<FhirLocalRefExpr>())
            {
                if (auto* lit = assign->value->as<FhirLiteralExpr>())
                {
                    constants[localRef->local] = lit->value;
                }
                else
                {
                    constants.erase(localRef->local);
                }
            }
        }
    }
    else if (auto* node = stmt->as<FhirReturnStmt>())
    {
        node->value = fold_expr(node->value);
    }
    else if (auto* node = stmt->as<FhirIfStmt>())
    {
        fold_if(node);
    }
    else if (auto* node = stmt->as<FhirWhileStmt>())
    {
        fold_while(node);
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

#pragma region Static Evaluators

std::optional<int64_t> FhirConstantFolder::try_evaluate_constant_int(FhirExpr* expr)
{
    if (!expr) return std::nullopt;

    if (auto* lit = expr->as<FhirLiteralExpr>())
    {
        if (lit->value.kind == LiteralValue::Kind::Int)
            return lit->value.intValue;
        if (lit->value.kind == LiteralValue::Kind::UInt)
            return static_cast<int64_t>(lit->value.uintValue);
    }

    if (auto* intrinsic = expr->as<FhirIntrinsicExpr>())
    {
        if (intrinsic->args.size() == 1 && intrinsic->op == IntrinsicOp::Negative)
        {
            if (auto inner = try_evaluate_constant_int(intrinsic->args[0]))
                return -*inner;
        }
        if (intrinsic->args.size() == 1 && intrinsic->op == IntrinsicOp::Positive)
        {
            return try_evaluate_constant_int(intrinsic->args[0]);
        }
        if (intrinsic->args.size() == 2)
        {
            auto lhs = try_evaluate_constant_int(intrinsic->args[0]);
            auto rhs = try_evaluate_constant_int(intrinsic->args[1]);
            if (lhs && rhs)
            {
                switch (intrinsic->op)
                {
                    case IntrinsicOp::Add: return *lhs + *rhs;
                    case IntrinsicOp::Sub: return *lhs - *rhs;
                    case IntrinsicOp::Mul: return *lhs * *rhs;
                    case IntrinsicOp::Div: return *rhs != 0 ? std::optional(*lhs / *rhs) : std::nullopt;
                    default: break;
                }
            }
        }
    }

    return std::nullopt;
}

std::optional<double> FhirConstantFolder::try_evaluate_constant_float(FhirExpr* expr)
{
    if (!expr) return std::nullopt;

    if (auto* lit = expr->as<FhirLiteralExpr>())
    {
        if (lit->value.kind == LiteralValue::Kind::Float)
            return lit->value.floatValue;
    }

    if (auto* intrinsic = expr->as<FhirIntrinsicExpr>())
    {
        if (intrinsic->args.size() == 1 && intrinsic->op == IntrinsicOp::Negative)
        {
            if (auto inner = try_evaluate_constant_float(intrinsic->args[0]))
                return -*inner;
        }
        if (intrinsic->args.size() == 1 && intrinsic->op == IntrinsicOp::Positive)
        {
            return try_evaluate_constant_float(intrinsic->args[0]);
        }
        if (intrinsic->args.size() == 2)
        {
            auto lhs = try_evaluate_constant_float(intrinsic->args[0]);
            auto rhs = try_evaluate_constant_float(intrinsic->args[1]);
            if (lhs && rhs)
            {
                switch (intrinsic->op)
                {
                    case IntrinsicOp::Add: return *lhs + *rhs;
                    case IntrinsicOp::Sub: return *lhs - *rhs;
                    case IntrinsicOp::Mul: return *lhs * *rhs;
                    case IntrinsicOp::Div: return *rhs != 0.0 ? std::optional(*lhs / *rhs) : std::nullopt;
                    default: break;
                }
            }
        }
    }

    return std::nullopt;
}

std::optional<bool> FhirConstantFolder::try_evaluate_constant_bool(FhirExpr* expr)
{
    if (!expr) return std::nullopt;

    if (auto* lit = expr->as<FhirLiteralExpr>())
    {
        if (lit->value.kind == LiteralValue::Kind::Bool)
            return lit->value.boolValue;
    }

    if (auto* intrinsic = expr->as<FhirIntrinsicExpr>())
    {
        if (intrinsic->args.size() == 1 && intrinsic->op == IntrinsicOp::Not)
        {
            if (auto inner = try_evaluate_constant_bool(intrinsic->args[0]))
                return !*inner;
        }
        if (intrinsic->args.size() == 2)
        {
            if (auto lhsInt = try_evaluate_constant_int(intrinsic->args[0]))
            {
                if (auto rhsInt = try_evaluate_constant_int(intrinsic->args[1]))
                {
                    switch (intrinsic->op)
                    {
                        case IntrinsicOp::Greater:      return *lhsInt > *rhsInt;
                        case IntrinsicOp::Less:         return *lhsInt < *rhsInt;
                        case IntrinsicOp::GreaterEqual:  return *lhsInt >= *rhsInt;
                        case IntrinsicOp::LessEqual:    return *lhsInt <= *rhsInt;
                        case IntrinsicOp::Equal:        return *lhsInt == *rhsInt;
                        case IntrinsicOp::NotEqual:     return *lhsInt != *rhsInt;
                        default: break;
                    }
                }
            }

            if (auto lhsFloat = try_evaluate_constant_float(intrinsic->args[0]))
            {
                if (auto rhsFloat = try_evaluate_constant_float(intrinsic->args[1]))
                {
                    switch (intrinsic->op)
                    {
                        case IntrinsicOp::Greater:      return *lhsFloat > *rhsFloat;
                        case IntrinsicOp::Less:         return *lhsFloat < *rhsFloat;
                        case IntrinsicOp::GreaterEqual:  return *lhsFloat >= *rhsFloat;
                        case IntrinsicOp::LessEqual:    return *lhsFloat <= *rhsFloat;
                        case IntrinsicOp::Equal:        return *lhsFloat == *rhsFloat;
                        case IntrinsicOp::NotEqual:     return *lhsFloat != *rhsFloat;
                        default: break;
                    }
                }
            }

            if (auto lhsBool = try_evaluate_constant_bool(intrinsic->args[0]))
            {
                if (auto rhsBool = try_evaluate_constant_bool(intrinsic->args[1]))
                {
                    switch (intrinsic->op)
                    {
                        case IntrinsicOp::And:      return *lhsBool && *rhsBool;
                        case IntrinsicOp::Or:       return *lhsBool || *rhsBool;
                        case IntrinsicOp::Equal:    return *lhsBool == *rhsBool;
                        case IntrinsicOp::NotEqual: return *lhsBool != *rhsBool;
                        default: break;
                    }
                }
            }
        }
    }

    return std::nullopt;
}

}
