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

#pragma region Statement Analysis

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

#pragma region Block Analysis

bool FlowAnalyzer::check_block(FhirBlock* block)
{
    if (!block) return false;

    for (size_t i = 0; i < block->statements.size(); i++)
    {
        if (check_stmt(block->statements[i]))
        {
            if (i + 1 < block->statements.size())
            {
                diag.warn("unreachable code", block->statements[i + 1]->span);
            }
            return true;
        }
    }

    return false;
}

#pragma region Public

void FlowAnalyzer::analyze(FhirMethod* method, Diagnostics& diag)
{
    if (!method || !method->body) return;
    if (!method->symbol) return;
    if (method->symbol->is_constructor()) return;

    FlowAnalyzer analyzer(diag);
    bool definitelyReturns = analyzer.check_block(method->body);

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
        diag.error("not all code paths return a value", loc);
    }
}

}
