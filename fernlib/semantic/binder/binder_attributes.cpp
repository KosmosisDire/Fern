#include "binder.hpp"

#include <ast/ast.hpp>
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

        if (!root->is<IdentifierExprSyntax>() && !root->is<MemberAccessExprSyntax>())
        {
            error("attribute must be a type name", attr->span);
            continue;
        }

        Symbol* sym = resolve_expr_symbol(root);
        if (!sym)
        {
            continue;
        }

        auto* attrType = sym->as<NamedTypeSymbol>();
        if (!attrType)
        {
            error("'" + sym->qualified_name() + "' is not a type", attr->span);
            continue;
        }

        if (!attrType->is_attribute())
        {
            error("type '" + attrType->name + "' is not an attribute type (missing 'attr' modifier)", attr->span);
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
                error("expected type name in attribute initializer", attr->span);
                continue;
            }

            if (auto* innerCall = initExpr->target->as<CallExprSyntax>())
            {
                Symbol* calleeSym = resolve_expr_symbol(innerCall->callee);
                if (calleeSym && calleeSym->kind == SymbolKind::Method)
                {
                    ctor = calleeSym->as<MethodSymbol>();
                }
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
                error("'" + attrType->name + "' must contain a parameterless constructor to construct with only an initializer list", attr->span);
            }
        }

        out.push_back(ResolvedAttribute{attrType, ctor});
    }
}

void Binder::resolve_all_attributes()
{
    for (auto* type : context.symbols.allTypes)
    {

        currentType = type;
        currentNamespace = type->find_enclosing_namespace();

        auto* typeDecl = type->syntax ? type->syntax->as<TypeDeclSyntax>() : nullptr;
        if (typeDecl)
        {
            resolve_attributes(typeDecl, type->resolvedAttributes);
        }

        for (auto* field : type->fields)
        {
            auto* fieldDecl = field->syntax ? field->syntax->as<FieldDeclSyntax>() : nullptr;
            if (fieldDecl)
            {
                resolve_attributes(fieldDecl, field->resolvedAttributes);
            }
        }

        for (auto* method : type->methods)
        {
            if (method->is_auto_generated())
            {
                continue;
            }

            auto* callableDecl = method->syntax->as<CallableDeclSyntax>();
            if (callableDecl)
            {
                resolve_attributes(callableDecl, method->resolvedAttributes);
            }
        }
    }

    currentType = nullptr;
    currentNamespace = nullptr;
}

}
