#include "binder.hpp"

#include <algorithm>

#include <ast/ast.hpp>
#include <semantic/context.hpp>

namespace Fern
{

void Binder::declare_namespace(NamespaceDeclSyntax* nsDecl, NamespaceSymbol* parentNs)
{
    if (!nsDecl || !parentNs)
    {
        return;
    }

    auto* ns = context.symbols.get_or_create_namespace(parentNs, nsDecl->name.lexeme);
    if (!ns->syntax)
    {
        ns->syntax = nsDecl;
    }

    for (auto* decl : nsDecl->declarations)
    {
        if (auto* nestedNs = decl->as<NamespaceDeclSyntax>())
        {
            declare_namespace(nestedNs, ns);
        }
        else if (auto* typeDecl = decl->as<TypeDeclSyntax>())
        {
            declare_type(typeDecl, ns);
        }
    }
}

NamedTypeSymbol* Binder::declare_type(TypeDeclSyntax* typeDecl, Symbol* parent)
{
    if (!typeDecl || !parent)
    {
        return nullptr;
    }

    auto* type = context.symbols.create_type(parent, typeDecl->name.lexeme, typeDecl, typeDecl->modifiers);

    for (auto& param : typeDecl->typeParams)
    {
        type->typeParams.push_back(std::string(param.lexeme));
    }

    for (int i = 0; i < static_cast<int>(type->typeParams.size()); ++i)
    {
        auto paramPtr = std::make_unique<TypeParamSymbol>();
        paramPtr->name = type->typeParams[i];
        paramPtr->index = i;
        paramPtr->owningType = type;
        paramPtr->parent = type;
        type->typeParamSymbols.push_back(context.symbols.own(std::move(paramPtr)));
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
                case CallableKind::Literal:
                    methodPtr->name = std::string(callableAst->name.lexeme);
                    methodPtr->modifiers = Modifier::Public | Modifier::Static;
                    break;
                case CallableKind::Cast:
                    methodPtr->name = "cast";
                    methodPtr->modifiers = callableAst->modifiers | Modifier::Static;
                    break;
            }

            auto* method = context.symbols.own(std::move(methodPtr));
            create_parameters(method, callableAst->parameters);

            if (callableAst->callableKind == CallableKind::Operator)
            {
                bool isIndexGet = callableAst->name.kind == TokenKind::IndexOp;
                bool isIndexSet = callableAst->name.kind == TokenKind::IndexSetOp;

                if (isIndexGet && method->parameters.size() != 2)
                {
                    error("operator '[]' must have 2 parameters (self, index), but has " +
                          std::to_string(method->parameters.size()), callableAst->span);
                }
                else if (isIndexSet && method->parameters.size() != 3)
                {
                    error("operator '[]=' must have 3 parameters (self, index, value), but has " +
                          std::to_string(method->parameters.size()), callableAst->span);
                }
                else if (!isIndexGet && !isIndexSet &&
                         (method->parameters.size() < 1 || method->parameters.size() > 2))
                {
                    error("operator '" + std::string(Fern::format(callableAst->name.kind)) +
                          "' must have 1 parameter (unary) or 2 parameters (binary), but has " +
                          std::to_string(method->parameters.size()), callableAst->span);
                }
            }

            type->methods.push_back(method);
            allMethods.push_back(method);

            if (method->is_literal())
                literalSuffixMap[method->name].push_back(method);
        }
        else if (auto* nestedTypeDecl = member->as<TypeDeclSyntax>())
        {
            declare_type(nestedTypeDecl, type);
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

}
