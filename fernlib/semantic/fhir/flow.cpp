#include "flow.hpp"
#include <ast/ast.hpp>
#include <symbol/symbol.hpp>

namespace Fern
{

FlowAnalyzer::FlowAnalyzer(Diagnostics& diag) : diag(diag) {}

#pragma region Constant Detection

bool FlowAnalyzer::is_constant_true(FhirExpr* expr)
{
    if (!expr) return false;
    const auto& c = expr->get_constant();
    return c && c->kind == ConstantValue::Kind::Bool && c->boolValue;
}

bool FlowAnalyzer::is_constant_false(FhirExpr* expr)
{
    if (!expr) return false;
    const auto& c = expr->get_constant();
    return c && c->kind == ConstantValue::Kind::Bool && !c->boolValue;
}

#pragma region Return Analysis

bool FlowAnalyzer::check_if(FhirIfStmt* stmt)
{
    if (is_constant_true(stmt->condition))
    {
        return check_block(stmt->thenBlock);
    }

    if (is_constant_false(stmt->condition))
    {
        if (stmt->elseIf) return check_if(stmt->elseIf);
        if (stmt->elseBlock) return check_block(stmt->elseBlock);
        return false;
    }

    bool thenReturns = check_block(stmt->thenBlock);

    if (stmt->elseIf)
    {
        return thenReturns && check_if(stmt->elseIf);
    }
    if (stmt->elseBlock)
    {
        return thenReturns && check_block(stmt->elseBlock);
    }

    return false;
}

bool FlowAnalyzer::check_while(FhirWhileStmt* stmt)
{
    if (is_constant_true(stmt->condition))
    {
        return check_block(stmt->body);
    }
    return false;
}

bool FlowAnalyzer::check_stmt(FhirStmt* stmt)
{
    if (!stmt) return false;

    if (stmt->is<FhirReturnStmt>()) return true;
    if (auto* ifStmt = stmt->as<FhirIfStmt>()) return check_if(ifStmt);
    if (auto* whileStmt = stmt->as<FhirWhileStmt>()) return check_while(whileStmt);

    return false;
}

bool FlowAnalyzer::check_block(FhirBlock* block)
{
    if (!block) return false;

    for (size_t i = 0; i < block->statements.size(); i++)
    {
        if (check_stmt(block->statements[i]))
        {
            if (i + 1 < block->statements.size())
            {
                diag.report(DiagnosticCode::Wrn_UnreachableCode, block->statements[i + 1]->span);
            }
            return true;
        }
    }

    return false;
}

#pragma region Definite Assignment State

FlowAnalyzer::State FlowAnalyzer::join(State&& a, State&& b)
{
    if (!a.reachable) return std::move(b);
    if (!b.reachable) return std::move(a);

    State result;
    for (const Symbol* symbol : a.assigned)
    {
        if (b.assigned.contains(symbol))
        {
            result.assigned.insert(symbol);
        }
    }
    return result;
}

#pragma region Definite Assignment Statements

void FlowAnalyzer::da_block(FhirBlock* block, State& state)
{
    if (!block) return;
    for (auto* stmt : block->statements)
    {
        if (!state.reachable) return;
        da_stmt(stmt, state);
    }
}

void FlowAnalyzer::da_stmt(FhirStmt* stmt, State& state)
{
    if (!stmt) return;

    if (auto* var = stmt->as<FhirVarDeclStmt>())
    {
        if (var->initializer)
        {
            da_expr(var->initializer, state);
            if (var->local)
            {
                state.assigned.insert(var->local);
            }
        }
        return;
    }
    if (auto* exprStmt = stmt->as<FhirExprStmt>())
    {
        da_expr(exprStmt->expression, state);
        return;
    }
    if (auto* ret = stmt->as<FhirReturnStmt>())
    {
        da_expr(ret->value, state);
        check_ctor_complete(state, ret->span);
        state.reachable = false;
        return;
    }
    if (auto* ifStmt = stmt->as<FhirIfStmt>())
    {
        da_if(ifStmt, state);
        return;
    }
    if (auto* whileStmt = stmt->as<FhirWhileStmt>())
    {
        da_expr(whileStmt->condition, state);
        if (is_constant_true(whileStmt->condition))
        {
            // No break statement exists yet so a constant true loop never exits
            da_block(whileStmt->body, state);
            state.reachable = false;
            return;
        }
        // The body may not run so its assignments are not definite afterwards
        State body = state;
        da_block(whileStmt->body, body);
        return;
    }
}

void FlowAnalyzer::da_if(FhirIfStmt* stmt, State& state)
{
    if (!stmt) return;
    da_expr(stmt->condition, state);

    if (is_constant_true(stmt->condition))
    {
        da_block(stmt->thenBlock, state);
        return;
    }
    if (is_constant_false(stmt->condition))
    {
        if (stmt->elseIf) da_if(stmt->elseIf, state);
        else da_block(stmt->elseBlock, state);
        return;
    }

    State thenState = state;
    da_block(stmt->thenBlock, thenState);

    State elseState = std::move(state);
    if (stmt->elseIf) da_if(stmt->elseIf, elseState);
    else da_block(stmt->elseBlock, elseState);

    state = join(std::move(thenState), std::move(elseState));
}

#pragma region Definite Assignment Expressions

void FlowAnalyzer::da_expr(FhirExpr* expr, State& state)
{
    if (!expr) return;

    if (auto* local = expr->as<FhirLocalRefExpr>())
    {
        if (local->symbol && !state.assigned.contains(local->symbol))
        {
            diag.report(DiagnosticCode::Err_UseOfUnassignedLocal, expr->span, local->symbol->name);
            state.assigned.insert(local->symbol);
        }
        return;
    }
    if (auto* field = expr->as<FhirFieldRefExpr>())
    {
        da_field_read(field, state);
        return;
    }
    if (expr->is<FhirThisExpr>())
    {
        check_this_escape(expr, state);
        return;
    }
    if (auto* assign = expr->as<FhirAssignExpr>())
    {
        da_assign(assign->target, assign->value, state);
        return;
    }
    if (auto* compound = expr->as<FhirCompoundAssignExpr>())
    {
        da_expr(compound->target(), state);
        da_expr(compound->value(), state);
        return;
    }
    if (auto* op = expr->as<FhirOpExpr>())
    {
        if ((op->op == IntrinsicKind::BoolAnd || op->op == IntrinsicKind::BoolOr) && op->args.size() == 2)
        {
            // The right side may not evaluate so its assignments are not definite
            da_expr(op->args[0], state);
            State rhs = state;
            da_expr(op->args[1], rhs);
            return;
        }
        for (auto* arg : op->args)
        {
            da_expr(arg, state);
        }
        return;
    }
    if (auto* call = expr->as<FhirCallExpr>())
    {
        da_expr(call->callee, state);
        for (auto* arg : call->arguments)
        {
            da_expr(arg, state);
        }
        return;
    }
    if (auto* methodRef = expr->as<FhirMethodRefExpr>())
    {
        da_expr(methodRef->thisRef, state);
        return;
    }
    if (auto* group = expr->as<FhirMethodGroupRefExpr>())
    {
        da_expr(group->thisRef, state);
        return;
    }
    if (auto* construction = expr->as<FhirConstructionExpr>())
    {
        da_expr(construction->call, state);
        return;
    }
    if (auto* cast = expr->as<FhirCastExpr>())
    {
        da_expr(cast->operand, state);
        return;
    }
    if (auto* index = expr->as<FhirIndexExpr>())
    {
        da_expr(index->object, state);
        da_expr(index->index, state);
        return;
    }
    if (auto* builder = expr->as<FhirObjectBuilderExpr>())
    {
        da_expr(builder->construction, state);
        for (auto& entry : builder->entries)
        {
            da_expr(entry.value, state);
        }
        return;
    }
    if (auto* array = expr->as<FhirArrayLiteralExpr>())
    {
        for (auto* element : array->elements)
        {
            da_expr(element, state);
        }
        return;
    }
    if (auto* error = expr->as<FhirErrorExpr>())
    {
        da_expr(error->inner, state);
        return;
    }
}

void FlowAnalyzer::da_assign(FhirExpr* target, FhirExpr* value, State& state)
{
    if (auto* local = target ? target->as<FhirLocalRefExpr>() : nullptr)
    {
        da_expr(value, state);
        if (local->symbol)
        {
            state.assigned.insert(local->symbol);
        }
        return;
    }
    if (auto* field = target ? target->as<FhirFieldRefExpr>() : nullptr)
    {
        bool onThis = field->thisRef && field->thisRef->is<FhirThisExpr>();
        if (!onThis)
        {
            da_expr(field->thisRef, state);
        }
        da_expr(value, state);
        if (onThis && field->symbol)
        {
            state.assigned.insert(field->symbol);
        }
        return;
    }
    if (auto* index = target ? target->as<FhirIndexExpr>() : nullptr)
    {
        da_expr(index->object, state);
        da_expr(index->index, state);
        da_expr(value, state);
        return;
    }
    da_expr(target, state);
    da_expr(value, state);
}

void FlowAnalyzer::da_field_read(FhirFieldRefExpr* expr, State& state)
{
    bool onThis = expr->thisRef && expr->thisRef->is<FhirThisExpr>();
    if (!onThis)
    {
        da_expr(expr->thisRef, state);
        return;
    }
    if (ctorType && expr->symbol && !state.assigned.contains(expr->symbol))
    {
        diag.report(DiagnosticCode::Err_UseOfUnassignedField, expr->span, expr->symbol->name);
        state.assigned.insert(expr->symbol);
    }
}

#pragma region Constructor Checks

void FlowAnalyzer::check_this_escape(FhirExpr* expr, State& state)
{
    if (!ctorType) return;
    for (auto* field : ctorType->fields)
    {
        if (!state.assigned.contains(field))
        {
            diag.report(DiagnosticCode::Err_ThisBeforeInit, expr->span);
            return;
        }
    }
}

void FlowAnalyzer::check_ctor_complete(State& state, const Span& span)
{
    if (!ctorType) return;
    for (auto* field : ctorType->fields)
    {
        if (!state.assigned.contains(field))
        {
            diag.report(DiagnosticCode::Err_CtorFieldNotAssigned, span, field->name);
        }
    }
}

#pragma region Public

void FlowAnalyzer::analyze(FhirMethod* method, Diagnostics& diag)
{
    if (!method || !method->body) return;
    if (!method->symbol) return;

    FlowAnalyzer analyzer(diag);
    bool definitelyReturns = analyzer.check_block(method->body);

    if (method->symbol->is_constructor())
    {
        analyzer.ctorType = method->symbol->parent ? method->symbol->parent->as<NamedTypeSymbol>() : nullptr;
    }
    else
    {
        TypeSymbol* returnType = method->symbol->get_return_type();
        if (returnType && !definitelyReturns)
        {
            Span loc = method->body->span;
            if (auto* callable = method->symbol->syntax ? method->symbol->syntax->as<CallableDeclSyntax>() : nullptr)
            {
                loc = callable->name.span;
                if (callable->returnType)
                {
                    loc = loc.merge(callable->returnType->span);
                }
            }
            diag.report(DiagnosticCode::Err_NotAllPathsReturn, loc);
        }
    }

    State state;
    if (analyzer.ctorType)
    {
        for (auto* field : analyzer.ctorType->fields)
        {
            // Fields with a missing type already errored so do not require them
            auto* fieldType = field->type ? field->type->as<NamedTypeSymbol>() : nullptr;
            if (!fieldType || fieldType->has_default())
            {
                state.assigned.insert(field);
            }
        }
    }

    analyzer.da_block(method->body, state);

    if (analyzer.ctorType && state.reachable)
    {
        analyzer.check_ctor_complete(state, method->body->span);
    }
}

}
