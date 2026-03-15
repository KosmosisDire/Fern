#include "binder.hpp"

#include <algorithm>

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

NamedTypeSymbol* Binder::create_type_symbol(TypeDeclSyntax* typeDecl, Symbol* parent)
{
    if (!typeDecl || !parent)
    {
        return nullptr;
    }

    auto typePtr = std::make_unique<NamedTypeSymbol>();
    typePtr->name = std::string(typeDecl->name.lexeme);
    typePtr->syntax = typeDecl;
    typePtr->parent = parent;
    typePtr->modifiers = typeDecl->modifiers;
    for (auto& param : typeDecl->typeParams)
    {
        typePtr->typeParams.push_back(std::string(param.lexeme));
    }

    auto* type = context.symbols.own(std::move(typePtr));

    for (int i = 0; i < static_cast<int>(type->typeParams.size()); ++i)
    {
        auto paramPtr = std::make_unique<TypeParamSymbol>();
        paramPtr->name = type->typeParams[i];
        paramPtr->index = i;
        paramPtr->owningType = type;
        paramPtr->parent = type;
        type->typeParamSymbols.push_back(context.symbols.own(std::move(paramPtr)));
    }

    if (auto* ns = parent->as<NamespaceSymbol>())
    {
        ns->add_type(type);
    }
    else if (auto* parentType = parent->as<NamedTypeSymbol>())
    {
        parentType->nestedTypes.push_back(type);
    }

    allTypes.push_back(type);

    int fieldIndex = 0;
    for (auto* member : typeDecl->declarations)
    {
        if (auto* fieldAst = member->as<FieldDeclSyntax>())
        {
            auto fieldPtr = std::make_unique<FieldSymbol>();
            fieldPtr->name = std::string(fieldAst->name.lexeme);
            fieldPtr->syntax = fieldAst;
            fieldPtr->parent = type;
            fieldPtr->modifiers = fieldAst->modifiers;
            fieldPtr->index = fieldIndex++;

            type->fields.push_back(context.symbols.own(std::move(fieldPtr)));
        }
        else if (auto* callableAst = member->as<CallableDeclSyntax>())
        {
            auto methodPtr = std::make_unique<MethodSymbol>();
            methodPtr->syntax = callableAst;
            methodPtr->parent = type;
            methodPtr->callableKind = callableAst->callableKind;

            switch (callableAst->callableKind)
            {
                case CallableKind::Function:
                    methodPtr->name = std::string(callableAst->name.lexeme);
                    methodPtr->modifiers = callableAst->modifiers;
                    break;
                case CallableKind::Constructor:
                    methodPtr->name = "init";
                    methodPtr->modifiers = callableAst->modifiers;
                    break;
                case CallableKind::Operator:
                    methodPtr->name = std::string(callableAst->name.lexeme);
                    methodPtr->operatorKind = callableAst->name.kind;
                    methodPtr->modifiers = Modifier::Public | Modifier::Static;
                    break;
            }

            auto* method = context.symbols.own(std::move(methodPtr));
            create_parameters(method, callableAst->parameters);

            if (callableAst->callableKind == CallableKind::Operator &&
                (method->parameters.size() < 1 || method->parameters.size() > 2))
            {
                error("operator '" + std::string(Fern::format(callableAst->name.kind)) +
                      "' must have 1 parameter (unary) or 2 parameters (binary), but has " +
                      std::to_string(method->parameters.size()), callableAst->span);
            }

            type->methods.push_back(method);
            allMethods.push_back(method);
        }
        else if (auto* nestedTypeDecl = member->as<TypeDeclSyntax>())
        {
            create_type_symbol(nestedTypeDecl, type);
        }
    }

    bool hasInit = std::any_of(typeDecl->declarations.begin(), typeDecl->declarations.end(),
        [](auto* member)
        {
            auto* callable = member->template as<CallableDeclSyntax>();
            return callable && callable->callableKind == CallableKind::Constructor;
        });

    if (!hasInit)
    {
        auto methodPtr = std::make_unique<MethodSymbol>();
        methodPtr->name = "init";
        methodPtr->parent = type;
        methodPtr->callableKind = CallableKind::Constructor;
        methodPtr->modifiers = Modifier::Public;

        auto* method = context.symbols.own(std::move(methodPtr));

        int paramIndex = 0;
        for (auto* field : type->fields)
        {
            auto paramPtr = std::make_unique<ParameterSymbol>();
            paramPtr->name = field->name;
            paramPtr->parent = method;
            paramPtr->index = paramIndex++;

            method->parameters.push_back(context.symbols.own(std::move(paramPtr)));
        }

        type->methods.push_back(method);
        allMethods.push_back(method);
    }

    type->membersPopulated = true;
    return type;
}

void Binder::create_parameters(MethodSymbol* method, const std::vector<ParameterDeclSyntax*>& params)
{
    int paramIndex = 0;
    for (auto* paramAst : params)
    {
        auto paramPtr = std::make_unique<ParameterSymbol>();
        paramPtr->name = std::string(paramAst->name.lexeme);
        paramPtr->syntax = paramAst;
        paramPtr->parent = method;
        paramPtr->index = paramIndex++;

        method->parameters.push_back(context.symbols.own(std::move(paramPtr)));
    }
}

#pragma region Type Resolution

void Binder::resolve_all_types()
{
    for (auto* type : allTypes)
    {
        currentType = type;
        currentNamespace = type->find_enclosing_namespace();

        if (type->is_generic_definition())
        {
            for (size_t i = 0; i < type->typeParamSymbols.size(); ++i)
            {
                typeParamSubstitutions[type->typeParams[i]] = type->typeParamSymbols[i];
            }
        }

        for (auto* field : type->fields)
        {
            auto* fieldAst = field->syntax ? field->syntax->as<FieldDeclSyntax>() : nullptr;
            if (fieldAst && fieldAst->type)
            {
                field->type = resolve_type_expr(fieldAst->type);
            }
        }

        for (auto* method : type->methods)
        {
            auto* callable = method->syntax ? method->syntax->as<CallableDeclSyntax>() : nullptr;

            if (callable)
            {
                if (callable->returnType)
                {
                    method->set_return_type(resolve_type_expr(callable->returnType));
                }
                else if (method->is_constructor())
                {
                    method->set_return_type(type);
                }

                for (size_t i = 0; i < method->parameters.size() && i < callable->parameters.size(); ++i)
                {
                    if (callable->parameters[i]->type)
                    {
                        method->parameters[i]->type = resolve_type_expr(callable->parameters[i]->type);
                    }
                }
            }
            else if (method->is_constructor())
            {
                method->set_return_type(type);
                for (size_t i = 0; i < method->parameters.size() && i < type->fields.size(); ++i)
                {
                    method->parameters[i]->type = type->fields[i]->type;
                }
            }
        }

        if (type->is_generic_definition())
        {
            typeParamSubstitutions.clear();
        }
    }

    currentType = nullptr;
    currentNamespace = nullptr;
}

#pragma region Validation

void Binder::check_duplicate_methods(NamedTypeSymbol* type)
{
    for (size_t i = 0; i < type->methods.size(); ++i)
    {
        auto* a = type->methods[i];
        for (size_t j = i + 1; j < type->methods.size(); ++j)
        {
            auto* b = type->methods[j];
            if (a->name != b->name || a->parameters.size() != b->parameters.size())
            {
                continue;
            }

            bool sameSignature = std::equal(
                a->parameters.begin(), a->parameters.end(),
                b->parameters.begin(),
                [](auto* pa, auto* pb) { return pa->type == pb->type; });

            if (sameSignature)
            {
                Span loc = b->syntax ? b->syntax->span : Span{};
                switch (b->callableKind)
                {
                    case CallableKind::Constructor:
                        error("duplicate constructor on type '" + format_type_name(type) + "'", loc);
                        break;
                    case CallableKind::Operator:
                        error("duplicate operator '" + b->name + "' on type '" + format_type_name(type) + "'", loc);
                        break;
                    case CallableKind::Function:
                        error("duplicate method '" + b->name + "' on type '" + format_type_name(type) + "'", loc);
                        break;
                }
            }
        }
    }
}

void Binder::validate_all_types()
{
    for (auto* type : allTypes)
    {
        for (auto* method : type->methods)
        {
            if (method->is_operator())
            {
                bool hasContainingType = std::any_of(method->parameters.begin(), method->parameters.end(),
                    [type](auto* param) { return param->type == type; });

                if (!hasContainingType)
                {
                    Span loc = method->syntax ? method->syntax->span : Span{};
                    error("operator '" + method->name +
                          "' must have at least one parameter of containing type '" +
                          format_type_name(type) + "'", loc);
                }
            }
        }

        check_duplicate_methods(type);
    }
}

#pragma region Attribute Resolution

static BaseExprSyntax* extract_attribute_root(BaseExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* call = expr->as<CallExprSyntax>())
    {
        return extract_attribute_root(call->callee);
    }
    if (auto* init = expr->as<InitializerExprSyntax>())
    {
        return extract_attribute_root(init->target);
    }
    return expr;
}

void Binder::resolve_attributes(BaseDeclSyntax* decl, std::vector<ResolvedAttribute>& out)
{
    if (!decl) return;

    for (auto* attr : decl->attributes)
    {
        if (!attr || !attr->value) continue;

        auto* root = extract_attribute_root(attr->value);
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
                auto* fhir = bind_expr(arg);
                argTypes.push_back(fhir ? fhir->type : nullptr);
            }
            ctor = attrType->resolve_constructor(argTypes);
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
                ctor = attrType->resolve_constructor(emptyArgs);
            }
        }
        else
        {
            std::vector<TypeSymbol*> emptyArgs;
            ctor = attrType->resolve_constructor(emptyArgs);
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
    for (auto* type : allTypes)
    {
        if (type->is_generic_definition()) continue;

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
