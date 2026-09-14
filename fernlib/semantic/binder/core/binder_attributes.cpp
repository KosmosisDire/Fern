#include "binder.hpp"

#include <format>

#include <ast/ast.hpp>
#include <common/cast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>
#include <semantic/intrinsics.hpp>
#include <semantic/symbol/fmt.hpp>

namespace Fern
{

// Gets the attribute name / path from an attribute constructor / object builder @Foo(1) -> "Foo", @Test.Foo { ... } -> "Test.Foo"
static BaseExprSyntax* extract_attribute_name(BaseExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* call = expr->as<CallExprSyntax>())
    {
        return extract_attribute_name(call->callee);
    }
    if (auto* builder = expr->as<ObjectBuilderExprSyntax>())
    {
        return extract_attribute_name(builder->target);
    }
    return expr;
}

// Resolves the constructor for an explicit argument list and evaluates every
// argument to a compile time constant.
MethodSymbol* Binder::resolve_attribute_ctor(
    NamedTypeSymbol* attrType,
    const std::vector<ExprPtr>& argSyntax,
    const Span& span,
    std::vector<ConstantValue>& outArgs)
{
    std::vector<OverloadArg> args;
    bool hasErrorArg = false;
    for (auto* arg : argSyntax)
    {
        FhirExpr* bound = bind_value_expr(arg);
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

    auto result = attrType->find_constructor(args);
    if (result.ambiguous)
    {
        if (!hasErrorArg)
        {
            std::string candidates;
            for (auto* m : result.ambiguousCandidates)
                candidates += std::format("\n  {}", format_method(m, SymbolFormat::signature()));
            diag.report(DiagnosticCode::Err_AmbiguousCall, span, candidates);
        }
        return nullptr;
    }
    if (!result.best.method)
    {
        if (!hasErrorArg)
        {
            if (result.bestFailure.method)
            {
                report_argument_mismatches(result.bestFailure.method, args, argSyntax);
            }
            else
            {
                diag.report(DiagnosticCode::Err_NoMatchingConstructor, span, format_type(attrType), args.size());
            }
        }
        return nullptr;
    }

    if (hasErrorArg) return result.best.method;

    // Rebind with the parameter types so implicit conversions fold into the constants
    for (size_t i = 0; i < argSyntax.size(); ++i)
    {
        FhirExpr* coerced = bind_value_expr(argSyntax[i], result.best.method->parameters[i]->type);
        if (coerced && coerced->get_constant())
        {
            outArgs.push_back(*coerced->get_constant());
        }
        else
        {
            diag.report(DiagnosticCode::Err_AttrArgNotConst, argSyntax[i]->span);
            outArgs.push_back({});
        }
    }

    return result.best.method;
}

void Binder::resolve_attributes(BaseDeclSyntax* decl, std::vector<ResolvedAttribute>& out)
{
    if (!decl) return;

    for (auto* attr : decl->attributes)
    {
        if (!attr || !attr->value) continue;

        auto* root = extract_attribute_name(attr->value);
        if (!root) continue;

        FhirExpr* rootExpr = bind_expr(root);
        if (!rootExpr || rootExpr->is_error()) continue;

        auto* tref = rootExpr->as<FhirTypeRef>();
        auto* attrType = tref && tref->referenced ? tref->referenced->as<NamedTypeSymbol>() : nullptr;
        if (!attrType)
        {
            diag.report(DiagnosticCode::Err_AttrMustBeType, attr->span);
            continue;
        }

        if (!has_modifier(attrType->modifiers, Modifier::Attr))
        {
            diag.report(DiagnosticCode::Err_NotAttrType, attr->span, attrType->name);
            continue;
        }

        // Find the explicit argument list: @Foo(args) or @Foo(args) { ... }
        const std::vector<ExprPtr>* argSyntax = nullptr;
        if (auto* callExpr = attr->value->as<CallExprSyntax>())
        {
            argSyntax = &callExpr->arguments;
        }
        else if (auto* builderExpr = attr->value->as<ObjectBuilderExprSyntax>())
        {
            if (!builderExpr->target)
            {
                diag.report(DiagnosticCode::Err_AttrNeedsTypeName, attr->span);
                continue;
            }
            if (auto* innerCall = builderExpr->target->as<CallExprSyntax>())
            {
                argSyntax = &innerCall->arguments;
            }
        }

        MethodSymbol* ctor = nullptr;
        std::vector<ConstantValue> arguments;

        if (argSyntax)
        {
            ctor = resolve_attribute_ctor(attrType, *argSyntax, attr->span, arguments);
        }
        else
        {
            ctor = attrType->find_constructor({}).best.method;
            if (!ctor)
            {
                diag.report(DiagnosticCode::Err_AttrNeedsParameterlessCtor, attr->span, attrType->name);
            }
        }

        // Hardcode intrisic validation
        // there is probably a better way to do this, but fine for now
        if (ctor && attrType->qualified_name() == "Core.Intrinsic" &&
            !arguments.empty() && arguments[0].kind == ConstantValue::Kind::String &&
            intrinsic_from_name(arguments[0].stringValue) == IntrinsicKind::None)
        {
            diag.report(DiagnosticCode::Err_UnknownIntrinsic, attr->span, arguments[0].stringValue);
        }

        out.push_back(ResolvedAttribute{attrType, ctor, std::move(arguments)});
    }
}

}
