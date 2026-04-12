#include "binder.hpp"

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

FhirExpr* Binder::bind_call(CallExprSyntax* expr)
{
    Symbol* calleeSym = resolve_expr_symbol(expr->callee);

    std::vector<FhirExpr*> argExprs;
    std::vector<TypeSymbol*> argTypes;
    bool hasErrorArg = false;
    for (auto* arg : expr->arguments)
    {
        FhirExpr* bound = bind_value_expr(arg);
        argExprs.push_back(bound);
        if (bound && bound->is_error())
        {
            hasErrorArg = true;
            argTypes.push_back(nullptr);
        }
        else
        {
            argTypes.push_back(bound ? bound->type : nullptr);
        }
    }

    if (calleeSym && calleeSym->kind == SymbolKind::Type)
    {
        auto* namedType = calleeSym->as<NamedTypeSymbol>();
        if (namedType)
        {
            auto result = namedType->find_constructor(argTypes);
            if (result.ambiguous && !hasErrorArg)
            {
                std::string msg = "call is ambiguous between constructors:";
                for (auto* m : result.ambiguousCandidates)
                    msg += "\n  " + format_type_name(namedType) + "(" + m->format_parameters() + ")";
                error(msg, expr->span);
                return fhir.error_expr(expr);
            }
            if (!result.best.method && !hasErrorArg)
            {
                error("'" + format_type_name(namedType) + "' does not contain a constructor that takes " +
                      std::to_string(argTypes.size()) + (argTypes.size() == 1 ? " argument" : " arguments"), expr->span);
            }
            if (result.best.method && !hasErrorArg)
            {
                for (size_t i = 0; i < expr->arguments.size(); ++i)
                {
                    argExprs[i] = bind_value_expr(expr->arguments[i], result.best.method->parameters[i]->type);
                }
            }

            return fhir.object_create(expr, namedType, result.best.method, std::move(argExprs));
        }
    }

    MethodSymbol* method = nullptr;
    NamedTypeSymbol* targetType = nullptr;
    std::string_view methodName;
    FhirExpr* receiver = nullptr;

    if (auto* idExpr = expr->callee->as<IdentifierExprSyntax>())
    {
        if (calleeSym && calleeSym->kind == SymbolKind::Method)
        {
            targetType = calleeSym->parent ? calleeSym->parent->as<NamedTypeSymbol>() : nullptr;
            methodName = idExpr->name.lexeme;
        }
        else if (calleeSym)
        {
            error("'" + std::string(idExpr->name.lexeme) + "' cannot be called as a function", idExpr->span);
            return fhir.error_expr(expr);
        }
    }
    else if (auto* memberExpr = expr->callee->as<MemberAccessExprSyntax>())
    {
        if (calleeSym && calleeSym->kind == SymbolKind::Method)
        {
            targetType = calleeSym->parent ? calleeSym->parent->as<NamedTypeSymbol>() : nullptr;
            methodName = memberExpr->right.lexeme;
            receiver = bind_expr(memberExpr->left);
        }
        else
        {
            receiver = bind_expr(memberExpr->left);
            if (receiver && receiver->is_error())
            {
                return fhir.error_expr(expr);
            }
            TypeSymbol* leftType = receiver ? receiver->type : nullptr;
            auto* namedLeft = leftType ? leftType->as<NamedTypeSymbol>() : nullptr;
            if (namedLeft)
            {
                targetType = namedLeft;
                methodName = memberExpr->right.lexeme;
            }
            else if (calleeSym)
            {
                error("'" + std::string(memberExpr->right.lexeme) + "' cannot be called as a function", memberExpr->span);
                return fhir.error_expr(expr);
            }
        }
    }

    if (targetType && !methodName.empty())
    {
        auto result = targetType->find_method(methodName, argTypes);
        if (result.ambiguous && !hasErrorArg)
        {
            std::string msg = "call to '" + std::string(methodName) + "' is ambiguous between:";
            for (auto* m : result.ambiguousCandidates)
                msg += "\n  " + format_type_name(targetType) + "." + std::string(methodName) + "(" + m->format_parameters() + ")";
            error(msg, expr->span);
            return fhir.error_expr(expr);
        }
        method = result.best.method;
        if (!method)
        {
            if (!hasErrorArg)
            {
                error("'" + format_type_name(targetType) + "' does not contain a method '" + std::string(methodName) +
                      "' that takes " + std::to_string(argTypes.size()) +
                      (argTypes.size() == 1 ? " argument" : " arguments"), expr->span);
            }
            return fhir.error_expr(expr);
        }
        if (!hasErrorArg)
        {
            for (size_t i = 0; i < expr->arguments.size(); ++i)
            {
                argExprs[i] = bind_value_expr(expr->arguments[i], method->parameters[i]->type);
            }
        }
    }

    if (!method)
    {
        if (!calleeSym)
        {
            FhirExpr* calleeExpr = bind_value_expr(expr->callee);
            if (calleeExpr && !calleeExpr->is_error())
            {
                error("expression cannot be called as a function", expr->callee->span);
            }
        }
        return fhir.error_expr(expr);
    }

    TypeSymbol* returnType = method->get_return_type();

    if (method->is_constructor())
    {
        return fhir.object_create(expr, returnType, method, std::move(argExprs));
    }

    if (receiver && !has_modifier(method->modifiers, Modifier::Static))
    {
        return fhir.method_call(expr, returnType, receiver, method, std::move(argExprs));
    }

    return fhir.call(expr, returnType, method, std::move(argExprs));
}

}
