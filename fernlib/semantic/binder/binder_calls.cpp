#include "binder.hpp"

#include <cassert>

#include <ast/ast.hpp>
#include <common/cast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

FhirExpr* Binder::bind_call(CallExprSyntax* expr)
{
    FhirExpr* callee = bind_expr(expr->callee);

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

    if (!callee || callee->is_error()) return fhir.error_expr(expr);

    // Construction: callee bound to a type
    if (auto* tref = callee->as<FhirTypeRef>())
    {
        auto* namedType = tref->referenced ? tref->referenced->as<NamedTypeSymbol>() : nullptr;
        assert(namedType && "construction callee must not be null");

        auto result = namedType->find_constructor(argTypes);
        if (result.ambiguous && !hasErrorArg)
        {
            std::string msg = "call is ambiguous between constructors:";
            for (auto* m : result.ambiguousCandidates)
                msg += "\n  " + format_type_name(namedType) + "(" + m->format_parameters() + ")";
            diag.error(msg, expr->span);
            return fhir.error_expr(expr);
        }
        if (!result.best.method)
        {
            if (!hasErrorArg)
            {
                diag.error("'" + format_type_name(namedType) + "' does not contain a constructor that takes " +
                      std::to_string(argTypes.size()) + (argTypes.size() == 1 ? " argument" : " arguments"), expr->span);
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
            diag.error("'" + std::string(group->name) + "' cannot be called", expr->callee->span);
            return fhir.error_expr(expr, nullptr, group);
        }

        auto result = targetType->find_method(group->name, argTypes);
        if (result.ambiguous && !hasErrorArg)
        {
            std::string msg = "call to '" + std::string(group->name) + "' is ambiguous between:";
            for (auto* m : result.ambiguousCandidates)
                msg += "\n  " + format_type_name(targetType) + "." + std::string(group->name) + "(" + m->format_parameters() + ")";
            diag.error(msg, expr->span);
            return fhir.error_expr(expr);
        }
        MethodSymbol* method = result.best.method;
        if (!method)
        {
            if (!hasErrorArg)
            {
                diag.error("'" + format_type_name(targetType) + "' does not contain a method '" + std::string(group->name) +
                      "' that takes " + std::to_string(argTypes.size()) +
                      (argTypes.size() == 1 ? " argument" : " arguments"), expr->span);
            }
            return fhir.error_expr(expr);
        }

        // Static and instance compatibility check now that we know the resolved overload.
        bool isStatic = has_modifier(method->modifiers, Modifier::Static);
        if (!isStatic && !group->thisRef)
        {
            diag.error("instance method '" + std::string(group->name) + "' requires a receiver", expr->callee->span);
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

    // Anything else (namespaces, plain values) is not callable
    if (auto* nref = callee->as<FhirNamespaceRefExpr>())
    {
        std::string name = nref->namespaceSymbol ? nref->namespaceSymbol->name : "?";
        diag.error("'" + name + "' is a namespace and cannot be called", expr->callee->span);
        return fhir.error_expr(expr, nullptr, nref);
    }

    diag.error("expression cannot be called as a function", expr->callee->span);
    return fhir.error_expr(expr, nullptr, callee);
}

}
