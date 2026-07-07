#include "binder.hpp"

#include <format>

#include <ast/ast.hpp>
#include <common/cast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

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

        if (!attrType->is_attribute())
        {
            diag.report(DiagnosticCode::Err_NotAttrType, attr->span, attrType->name);
            continue;
        }

        MethodSymbol* ctor = nullptr;

        if (auto* callExpr = attr->value->as<CallExprSyntax>())
        {
            std::vector<OverloadArg> args;
            for (auto* arg : callExpr->arguments)
            {
                args.push_back(OverloadArg(bind_value_expr(arg)));
            }
            ctor = attrType->find_constructor(args).best.method;
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
                std::vector<OverloadArg> args;
                for (auto* arg : innerCall->arguments)
                {
                    args.push_back(OverloadArg(bind_value_expr(arg)));
                }
                ctor = attrType->find_constructor(args).best.method;
            }
            else
            {
                ctor = attrType->find_constructor({}).best.method;
            }
        }
        else
        {
            ctor = attrType->find_constructor({}).best.method;
            if (!ctor)
            {
                diag.report(DiagnosticCode::Err_AttrNeedsParameterlessCtor, attr->span, attrType->name);
            }
        }

        out.push_back(ResolvedAttribute{attrType, ctor});
    }
}

}
