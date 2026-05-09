#include "binder.hpp"

#include <format>

#include <ast/ast.hpp>
#include <common/cast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>
#include <semantic/symbol/fmt.hpp>

namespace Fern
{

static void report_argument_mismatches(
    Diagnostics& diag,
    MethodSymbol* candidate,
    const std::vector<OverloadArg>& args,
    const std::vector<ExprPtr>& argSyntax)
{
    for (size_t i = 0; i < args.size(); ++i)
    {
        if (i >= candidate->parameters.size()) continue;
        auto* param = candidate->parameters[i];
        if (!param || !param->type) continue;
        auto level = NamedTypeSymbol::get_conversion(args[i], param->type).level;
        if (level == Convertibility::Exact || level == Convertibility::Implicit) continue;

        std::string prefix = std::format("argument '{}': ", param->name);
        DiagnosticCode code = (level == Convertibility::Explicit)
            ? DiagnosticCode::Err_NoImplicitConv
            : DiagnosticCode::Err_TypeMismatch;
        diag.report(code, argSyntax[i]->span,
                    prefix, format_type(args[i].type), format_type(param->type));
    }
}

FhirExpr* Binder::bind_call(CallExprSyntax* expr)
{
    FhirExpr* callee = bind_expr(expr->callee);

    std::vector<FhirExpr*> argExprs;
    std::vector<OverloadArg> args;
    bool hasErrorArg = false;
    for (auto* arg : expr->arguments)
    {
        FhirExpr* bound = bind_value_expr(arg);
        argExprs.push_back(bound);
        if (bound && bound->is_error())
        {
            hasErrorArg = true;
            args.push_back({});
        }
        else
        {
            args.push_back(OverloadArg(bound));
        }
    }

    if (!callee || callee->is_error()) return fhir.error_expr(expr);

    // Construction: callee bound to a type
    if (auto* tref = callee->as<FhirTypeRef>())
    {
        auto* namedType = tref->referenced ? tref->referenced->as<NamedTypeSymbol>() : nullptr;
        if (!namedType)
        {
            diag.report(DiagnosticCode::Err_BadSymbolKind, expr->callee->span,
                format_type(tref->referenced),
                kind_noun(tref->referenced->kind), "method");
            return fhir.error_expr(expr);
        }

        auto result = namedType->find_constructor(args);
        if (result.ambiguous && !hasErrorArg)
        {
            std::string candidates;
            for (auto* m : result.ambiguousCandidates)
                candidates += std::format("\n  {}", format_method(m, SymbolFormat::signature()));
            diag.report(DiagnosticCode::Err_AmbiguousCall, expr->span, candidates);
            return fhir.error_expr(expr);
        }
        if (!result.best.method)
        {
            if (!hasErrorArg)
            {
                if (result.bestFailure.method)
                {
                    report_argument_mismatches(diag, result.bestFailure.method, args, expr->arguments);
                }
                else
                {
                    diag.report(DiagnosticCode::Err_NoMatchingConstructor, expr->span, format_type(namedType), args.size());
                }
            }
            return fhir.error_expr(expr);
        }
        if (!hasErrorArg)
        {
            for (size_t i = 0; i < expr->arguments.size(); ++i)
            {
                argExprs[i] = bind_value_expr(expr->arguments[i], result.best.method->parameters[i]->type);
            }
        }
        return fhir.construction(expr, namedType, tref, result.best.method, std::move(argExprs));
    }

    // Method call: callee bound to a method group
    if (auto* group = callee->as<FhirMethodGroupRefExpr>())
    {
        auto* targetType = group->enclosingScope ? group->enclosingScope->as<NamedTypeSymbol>() : nullptr;
        if (!targetType)
        {
            if (group->enclosingScope)
            {
                diag.report(DiagnosticCode::Err_BadSymbolKind, expr->callee->span,
                    group->enclosingScope->qualified_name(),
                    kind_noun(group->enclosingScope->kind), "type");
            }
            else
            {
                diag.report(DiagnosticCode::Err_NotCallableExpr, expr->callee->span);
            }
            return fhir.error_expr(expr, nullptr, group);
        }

        auto result = targetType->find_method(group->name, args);
        if (result.ambiguous && !hasErrorArg)
        {
            std::string candidates;
            for (auto* m : result.ambiguousCandidates)
                candidates += std::format("\n  {}", format_method(m, SymbolFormat::signature()));
            diag.report(DiagnosticCode::Err_AmbiguousCall, expr->span, candidates);
            return fhir.error_expr(expr);
        }
        MethodSymbol* method = result.best.method;
        if (!method)
        {
            if (!hasErrorArg)
            {
                if (result.bestFailure.method)
                {
                    report_argument_mismatches(diag, result.bestFailure.method, args, expr->arguments);
                }
                else
                {
                    diag.report(DiagnosticCode::Err_NoMatchingMethod, expr->span, group->name, format_type(targetType), args.size());
                }
            }
            return fhir.error_expr(expr);
        }

        // Static and instance compatibility check now that we know the resolved overload.
        bool isStatic = has_modifier(method->modifiers, Modifier::Static);
        if (!isStatic && !group->thisRef)
        {
            diag.report(DiagnosticCode::Err_InstanceMethodNoReceiver, expr->callee->span, group->name);
            return fhir.error_expr(expr);
        }

        if (!hasErrorArg)
        {
            for (size_t i = 0; i < expr->arguments.size(); ++i)
            {
                argExprs[i] = bind_value_expr(expr->arguments[i], method->parameters[i]->type);
            }
        }

        TypeSymbol* returnType = method->get_return_type();

        if (method->is_constructor())
        {
            auto* synthTypeRef = fhir.type_ref(expr->callee, targetType);
            return fhir.construction(expr, returnType, synthTypeRef, method, std::move(argExprs));
        }

        FhirExpr* thisRef = (!isStatic) ? group->thisRef : nullptr;
        return fhir.call(expr, returnType, method, thisRef, std::move(argExprs));
    }

    // Anything else is not callable
    if (auto* nref = callee->as<FhirNamespaceRefExpr>())
    {
        std::string_view name = "?";
        if (nref->namespaceSymbol) name = nref->namespaceSymbol->name;
        diag.report(DiagnosticCode::Err_BadSymbolKind, expr->callee->span, name, "namespace", "method");
        return fhir.error_expr(expr, nullptr, nref);
    }
    if (auto* lref = callee->as<FhirLocalRefExpr>())
    {
        std::string_view name = "?";
        if (lref->local) name = lref->local->name;
        diag.report(DiagnosticCode::Err_BadSymbolKind, expr->callee->span, name, "local", "method");
        return fhir.error_expr(expr, nullptr, lref);
    }
    if (auto* pref = callee->as<FhirParamRefExpr>())
    {
        std::string_view name = "?";
        if (pref->parameter) name = pref->parameter->name;
        diag.report(DiagnosticCode::Err_BadSymbolKind, expr->callee->span, name, "parameter", "method");
        return fhir.error_expr(expr, nullptr, pref);
    }
    if (auto* fref = callee->as<FhirFieldRefExpr>())
    {
        std::string_view name = "?";
        if (fref->field) name = fref->field->name;
        diag.report(DiagnosticCode::Err_BadSymbolKind, expr->callee->span, name, "field", "method");
        return fhir.error_expr(expr, nullptr, fref);
    }

    diag.report(DiagnosticCode::Err_NotCallableExpr, expr->callee->span);
    return fhir.error_expr(expr, nullptr, callee);
}

}
