#include "binder.hpp"

#include <algorithm>

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

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
        if (left.empty()) return "";
        return left + "." + std::string(member->right.lexeme);
    }
    return "";
}

FhirExpr* Binder::bind_initializer_target(InitializerExprSyntax* expr)
{
    if (!expr->target) return nullptr;

    if (auto* call = expr->target->as<CallExprSyntax>())
        return bind_call(call);

    Symbol* sym = lookup(expr->target);
    auto* namedType = sym ? sym->as<NamedTypeSymbol>() : nullptr;
    if (namedType)
    {
        auto ctorResult = namedType->find_constructor({});
        if (ctorResult.best.method)
        {
            return fhir.object_create(expr->target, namedType, ctorResult.best.method, {});
        }
    }

    return bind_value_expr(expr->target);
}

FhirExpr* Binder::build_field_access_chain(FhirExpr* receiver, BaseExprSyntax* target)
{
    if (auto* id = target->as<IdentifierExprSyntax>())
    {
        TypeSymbol* recvType = receiver ? receiver->type : nullptr;
        auto* namedRecv = recvType ? recvType->as<NamedTypeSymbol>() : nullptr;
        auto* field = namedRecv ? namedRecv->find_field(id->name.lexeme) : nullptr;
        return fhir.field_access(id, receiver, field);
    }

    if (auto* member = target->as<MemberAccessExprSyntax>())
    {
        FhirExpr* left = build_field_access_chain(receiver, member->left);
        TypeSymbol* leftType = left ? left->type : nullptr;
        auto* namedLeft = leftType ? leftType->as<NamedTypeSymbol>() : nullptr;
        auto* field = namedLeft ? namedLeft->find_field(member->right.lexeme) : nullptr;
        return fhir.field_access(member, left, field);
    }

    return nullptr;
}

FhirExpr* Binder::bind_initializer(InitializerExprSyntax* expr)
{
    if (!expr->target)
    {
        diag.error("cannot infer type for initializer list, use an explicit type name", expr->span);
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

    if (auto* idExpr = expr->target->as<IdentifierExprSyntax>())
    {
        Symbol* sym = lookup(idExpr->name.lexeme);
        if (!sym)
        {
            diag.error("undefined name '" + std::string(idExpr->name.lexeme) + "'", idExpr->span);
            return fhir.error_expr(expr);
        }

        namedType = sym->as<NamedTypeSymbol>();
        if (!namedType)
        {
            diag.error("initializer lists can only be applied to a type or constructor call", idExpr->span);
            return fhir.error_expr(expr);
        }

        if (!namedType->find_constructor({}).best.method)
        {
            diag.error("type '" + format_type_name(namedType) + "' has no default constructor", idExpr->span);
        }
    }
    else if (auto* callExpr = expr->target->as<CallExprSyntax>())
    {
        FhirExpr* callResult = bind_call(callExpr);
        if (callResult && callResult->is_error())
        {
            return fhir.error_expr(expr);
        }
        TypeSymbol* callType = callResult ? callResult->type : nullptr;
        namedType = callType ? callType->as<NamedTypeSymbol>() : nullptr;

        if (namedType)
        {
            Symbol* calleeSym = lookup(callExpr->callee);
            if (calleeSym)
            {
                auto* method = calleeSym->as<MethodSymbol>();
                if (method && !method->is_constructor())
                {
                    diag.error("initializer lists can only be applied to a type or constructor call", callExpr->span);
                    namedType = nullptr;
                }
            }
        }
    }
    else if (auto* memberExpr = expr->target->as<MemberAccessExprSyntax>())
    {
        Symbol* sym = lookup(memberExpr);
        if (!sym)
        {
            diag.error("undefined name '" + std::string(memberExpr->right.lexeme) + "'", memberExpr->span);
            return fhir.error_expr(expr);
        }

        namedType = sym->as<NamedTypeSymbol>();
        if (!namedType)
        {
            diag.error("initializer lists can only be applied to a type or constructor call", memberExpr->span);
            return fhir.error_expr(expr);
        }

        if (!namedType->find_constructor({}).best.method)
        {
            diag.error("type '" + format_type_name(namedType) + "' has no default constructor", memberExpr->span);
        }
    }
    else if (auto* genericExpr = expr->target->as<GenericTypeExprSyntax>())
    {
        TypeSymbol* type = resolve_generic_type(genericExpr);
        namedType = type ? type->as<NamedTypeSymbol>() : nullptr;
        if (!namedType)
        {
            return fhir.error_expr(expr);
        }

        if (!namedType->find_constructor({}).best.method)
        {
            diag.error("type '" + format_type_name(namedType) + "' has no default constructor", genericExpr->span);
        }
    }
    else
    {
        diag.error("initializer lists can only be applied to a type or constructor call", expr->target->span);
        return fhir.error_expr(expr);
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

    // Initializer lists with field assignments are lowered to:
    //   var __init_N = Constructor(...)   
    //   __init_N.field1 = value1          
    //   __init_N.field2 = value2          
    //   ... expression result is __init_N
    auto* pending = pending_statements();
    int* counter = temp_counter();
    if (!pending || !counter)
    {
        diag.error("initializer lists are not yet supported outside of method bodies", expr->span);
        return fhir.error_expr(expr);
    }

    auto tempPtr = std::make_unique<LocalSymbol>();
    tempPtr->name = "__init_" + std::to_string((*counter)++);
    tempPtr->type = namedType;
    auto* tempLocal = context.symbols.own(std::move(tempPtr));

    pending->push_back(fhir.var_decl(expr, tempLocal, bind_initializer_target(expr)));

    bind_initializer_fields(expr, namedType, *pending, fhir.local_ref(expr, tempLocal));

    return fhir.local_ref(expr, tempLocal);
}

void Binder::bind_initializer_fields(InitializerExprSyntax* expr, NamedTypeSymbol* namedType, std::vector<FhirStmt*>& out, FhirExpr* receiver)
{
    std::vector<std::string> boundFieldPaths;
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

        TypeSymbol* fieldType = nullptr;
        if (fieldInit->target &&
            (fieldInit->target->is<IdentifierExprSyntax>() ||
             fieldInit->target->is<MemberAccessExprSyntax>()))
        {
            fieldType = bind_field_init_target(fieldInit->target, namedType);
        }

        if (fieldInit->value)
        {
            if (receiver)
            {
                FhirExpr* fieldTarget = build_field_access_chain(receiver, fieldInit->target);
                out.push_back(fhir.expr_stmt(fieldInit, fhir.assign(fieldInit, fieldTarget, bind_value_expr(fieldInit->value, fieldType))));
            }
            else
            {
                bind_value_expr(fieldInit->value, fieldType);
            }
        }
    }

    for (auto* member : expr->members)
    {
        auto* fieldInit = member->as<FieldInitSyntax>();
        if (!fieldInit) continue;

        std::string path = format_field_path(fieldInit->target);
        if (path.empty()) continue;

        auto it = std::find(boundFieldPaths.begin(), boundFieldPaths.end(), path);
        if (it != boundFieldPaths.end())
        {
            diag.error("duplicate field '" + path + "' in initializer", fieldInit->target->span);
        }
        else
        {
            boundFieldPaths.push_back(std::move(path));
        }
    }
}

TypeSymbol* Binder::bind_field_init_target(BaseExprSyntax* target, NamedTypeSymbol* type)
{
    if (auto* id = target->as<IdentifierExprSyntax>())
    {
        FieldSymbol* field = type->find_field(id->name.lexeme);
        if (!field)
        {
            diag.error("type '" + format_type_name(type) + "' has no field named '" +
                  std::string(id->name.lexeme) + "'", target->span);
            return nullptr;
        }
        return field->type;
    }

    if (auto* member = target->as<MemberAccessExprSyntax>())
    {
        TypeSymbol* leftType = bind_field_init_target(member->left, type);
        if (!leftType)
        {
            return nullptr;
        }

        auto* nestedType = leftType->as<NamedTypeSymbol>();
        if (!nestedType)
        {
            diag.error("cannot access member on non-struct type", member->span);
            return nullptr;
        }

        FieldSymbol* field = nestedType->find_field(member->right.lexeme);
        if (!field)
        {
            diag.error("type '" + format_type_name(nestedType) + "' has no field named '" +
                  std::string(member->right.lexeme) + "'", member->span);
            return nullptr;
        }
        return field->type;
    }

    diag.error("initializer target must be a field name or member access", target->span);
    return nullptr;
}

}
