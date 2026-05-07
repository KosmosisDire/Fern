#include "binder.hpp"

#include <format>

#include <ast/ast.hpp>
#include <common/cast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

// Gets the attribute name / path from an attribute constructor / initializer @Foo(1) -> "Foo", @Test.Foo { ... } -> "Test.Foo"
static BaseExprSyntax* extract_attribute_name(BaseExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* call = expr->as<CallExprSyntax>())
    {
        return extract_attribute_name(call->callee);
    }
    if (auto* init = expr->as<InitializerExprSyntax>())
    {
        return extract_attribute_name(init->target);
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
            std::vector<TypeSymbol*> argTypes;
            for (auto* arg : callExpr->arguments)
            {
                auto* fhir = bind_value_expr(arg);
                argTypes.push_back(fhir ? fhir->type : nullptr);
            }
            ctor = attrType->find_constructor(argTypes).best.method;
        }
        else if (auto* initExpr = attr->value->as<InitializerExprSyntax>())
        {
            if (!initExpr->target)
            {
                diag.report(DiagnosticCode::Err_AttrNeedsTypeName, attr->span);
                continue;
            }

            if (auto* innerCall = initExpr->target->as<CallExprSyntax>())
            {
                std::vector<TypeSymbol*> argTypes;
                for (auto* arg : innerCall->arguments)
                {
                    auto* boundArg = bind_value_expr(arg);
                    argTypes.push_back(boundArg ? boundArg->type : nullptr);
                }
                ctor = attrType->find_constructor(argTypes).best.method;
            }
            else
            {
                std::vector<TypeSymbol*> emptyArgs;
                ctor = attrType->find_constructor(emptyArgs).best.method;
            }
        }
        else
        {
            std::vector<TypeSymbol*> emptyArgs;
            ctor = attrType->find_constructor(emptyArgs).best.method;
            if (!ctor)
            {
                diag.report(DiagnosticCode::Err_AttrNeedsParameterlessCtor, attr->span, attrType->name);
            }
        }

        out.push_back(ResolvedAttribute{attrType, ctor});
    }
}

}
