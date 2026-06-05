#include "binder.hpp"

#include <algorithm>
#include <format>

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>
#include <semantic/symbol/fmt.hpp>

namespace Fern
{

static std::string format_field_path(BaseExprSyntax* expr)
{
    if (!expr) return "";
    if (auto* id = expr->as<IdentifierExprSyntax>())
    {
        return std::string(id->name.lexeme);
    }
    if (auto* member = expr->as<MemberAccessExprSyntax>())
    {
        std::string left = format_field_path(member->left);
        if (left.empty() || !member->right) return "";
        return std::format("{}.{}", left, member->right->name.lexeme);
    }
    return "";
}

FhirExpr* Binder::bind_initializer_target(InitializerExprSyntax* expr)
{
    if (!expr->target) return nullptr;

    if (auto* call = expr->target->as<CallExprSyntax>())
        return bind_call(call);

    FhirExpr* targetExpr = bind_expr(expr->target);
    if (targetExpr && targetExpr->is_error()) return targetExpr;

    if (auto* tref = targetExpr ? targetExpr->as<FhirTypeRef>() : nullptr)
    {
        if (auto* namedType = tref->referenced ? tref->referenced->as<NamedTypeSymbol>() : nullptr)
        {
            auto ctorResult = namedType->find_constructor({});
            if (ctorResult.best.method)
            {
                return fhir.construction(expr->target, namedType, tref, ctorResult.best.method, {});
            }
        }
    }

    return bind_value_expr(expr->target);
}

// Walk a field syntax and collect the chain of FieldSymbols. 
// Returns false and reports diagnostics if any link is not a field on the expected type.
static bool collect_field_path(Binder* binder, BaseExprSyntax* target, NamedTypeSymbol* type,
                               std::vector<FieldSymbol*>& path_out, Diagnostics& diag)
{
    if (auto* id = target->as<IdentifierExprSyntax>())
    {
        FieldSymbol* field = type->find_field(id->name.lexeme);
        if (!field)
        {
            diag.report(DiagnosticCode::Err_NoSuchMember, target->span, format_type(type), id->name.lexeme);
            return false;
        }
        path_out.push_back(field);
        return true;
    }

    if (auto* member = target->as<MemberAccessExprSyntax>())
    {
        if (!collect_field_path(binder, member->left, type, path_out, diag))
            return false;

        TypeSymbol* parentType = path_out.back()->type;
        auto* nestedType = parentType ? parentType->as<NamedTypeSymbol>() : nullptr;
        if (!nestedType)
        {
            diag.report(DiagnosticCode::Err_InitMemberOnNonStruct, member->span);
            return false;
        }

        if (!member->right)
        {
            diag.report(DiagnosticCode::Err_InitTargetBadShape, member->span);
            return false;
        }

        return collect_field_path(binder, member->right, nestedType, path_out, diag);
    }

    diag.report(DiagnosticCode::Err_InitTargetBadShape, target->span);
    return false;
}

// Binds an initializer expression like `Foo { a: 1, b.c: 2 }`
FhirExpr* Binder::bind_initializer(InitializerExprSyntax* expr)
{
    // Expect a type or constructor call before the list
    if (!expr->target)
    {
        diag.report(DiagnosticCode::Err_InitListNoType, expr->span);
        for (auto* member : expr->members)
        {
            if (auto* fieldInit = member->as<FieldInitSyntax>())
            {
                bind_value_expr(fieldInit->value);
            }
            else if (auto* childStmt = member->as<ExpressionStmtSyntax>())
            {
                bind_value_expr(childStmt->expression);
            }
        }
        return fhir.error_expr(expr);
    }

    NamedTypeSymbol* namedType = nullptr;

    // Try as a call, like `Foo(1, 2) { ... }`
    if (auto* callExpr = expr->target->as<CallExprSyntax>())
    {
        FhirExpr* callResult = bind_call(callExpr);
        if (!callResult || callResult->is_error())
        {
            return fhir.error_expr(expr);
        }
        if (!callResult->is<FhirConstructionExpr>())
        {
            diag.report(DiagnosticCode::Err_InitListBadTarget, callExpr->span);
            return fhir.error_expr(expr);
        }
        TypeSymbol* callType = callResult->type;
        namedType = callType ? callType->as<NamedTypeSymbol>() : nullptr;
    }
    else // just bind and see if it is a type reference like `Foo { ... }`
    {
        FhirExpr* targetExpr = bind_expr(expr->target);
        if (!targetExpr || targetExpr->is_error())
        {
            return fhir.error_expr(expr);
        }

        auto* tref = targetExpr->as<FhirTypeRef>();
        if (!tref)
        {
            diag.report(DiagnosticCode::Err_InitListBadTarget, expr->target->span);
            return fhir.error_expr(expr);
        }

        namedType = tref->referenced ? tref->referenced->as<NamedTypeSymbol>() : nullptr;
        if (!namedType)
        {
            diag.report(DiagnosticCode::Err_InitListBadTarget, expr->target->span);
            return fhir.error_expr(expr);
        }

        if (!namedType->find_constructor({}).best.method)
        {
            diag.report(DiagnosticCode::Err_NoDefaultCtor, expr->target->span, format_type(namedType));
        }
    }

    if (!namedType)
    {
        for (auto* member : expr->members)
        {
            if (auto* fieldInit = member->as<FieldInitSyntax>())
            {
                bind_value_expr(fieldInit->value);
            }
            else if (auto* childStmt = member->as<ExpressionStmtSyntax>())
            {
                bind_value_expr(childStmt->expression);
            }
        }
        return fhir.error_expr(expr);
    }

    if (expr->members.empty())
    {
        return bind_initializer_target(expr);
    }

    FhirExpr* construction = bind_initializer_target(expr);

    std::vector<FhirInitializerEntry> entries;
    std::vector<std::string> seenPaths;
    for (auto* member : expr->members)
    {
        auto* fieldInit = member->as<FieldInitSyntax>();
        if (!fieldInit)
        {
            if (auto* childStmt = member->as<ExpressionStmtSyntax>())
            {
                bind_value_expr(childStmt->expression);
            }
            continue;
        }

        std::string pathStr = format_field_path(fieldInit->target);
        if (!pathStr.empty())
        {
            auto it = std::find(seenPaths.begin(), seenPaths.end(), pathStr);
            if (it != seenPaths.end())
            {
                diag.report(DiagnosticCode::Err_DuplicateInitField, fieldInit->target->span, pathStr);
                continue;
            }
            seenPaths.push_back(std::move(pathStr));
        }

        std::vector<FieldSymbol*> path;
        if (!fieldInit->target || !collect_field_path(this, fieldInit->target, namedType, path, diag))
        {
            if (fieldInit->value) bind_value_expr(fieldInit->value);
            continue;
        }

        TypeSymbol* fieldType = path.back()->type;
        FhirExpr* value = fieldInit->value ? bind_value_expr(fieldInit->value, fieldType) : nullptr;
        if (!value) continue;

        FhirInitializerEntry entry;
        entry.path = std::move(path);
        entry.value = value;
        entries.push_back(std::move(entry));
    }

    return fhir.initializer_expr(expr, namedType, construction, std::move(entries));
}

}
