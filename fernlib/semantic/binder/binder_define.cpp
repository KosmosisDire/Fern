#include "binder.hpp"

#include <algorithm>

#include <ast/ast.hpp>
#include <semantic/context.hpp>

namespace Fern
{

void Binder::define_namespace(NamespaceDeclSyntax* nsDecl, NamespaceSymbol* parentNs)
{
    if (!nsDecl || !parentNs)
    {
        return;
    }

    auto* ns = context.symbols.get_or_declare_namespace(parentNs, nsDecl);

    for (auto* decl : nsDecl->declarations)
    {
        if (auto* nestedNs = decl->as<NamespaceDeclSyntax>())
        {
            define_namespace(nestedNs, ns);
        }
        else if (auto* typeDecl = decl->as<TypeDeclSyntax>())
        {
            define_type(typeDecl, ns);
        }
    }
}

NamedTypeSymbol* Binder::define_type(TypeDeclSyntax* typeDecl, Symbol* parent)
{
    if (!typeDecl || !parent)
    {
        return nullptr;
    }

    auto* type = context.symbols.declare_type(parent, typeDecl);

    for (auto& param : typeDecl->typeParams)
    {
        type->typeParams.push_back(std::string(param.lexeme));
    }

    for (int i = 0; i < static_cast<int>(type->typeParams.size()); ++i)
    {
        context.symbols.declare_type_param(type, i, type->typeParams[i]);
    }

    int fieldIndex = 0;
    for (auto* member : typeDecl->declarations)
    {
        if (auto* fieldAst = member->as<FieldDeclSyntax>())
        {
            context.symbols.declare_field(type, fieldAst, fieldIndex++);
        }
        else if (auto* callableAst = member->as<CallableDeclSyntax>())
        {
            auto* method = context.symbols.declare_method(type, callableAst);

            for (int i = 0; i < static_cast<int>(callableAst->parameters.size()); ++i)
            {
                context.symbols.declare_parameter(method, callableAst->parameters[i], i);
            }

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
        }
        else if (auto* nestedTypeDecl = member->as<TypeDeclSyntax>())
        {
            define_type(nestedTypeDecl, type);
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
        auto* method = context.symbols.declare_method(type, "init", Modifier::Public, CallableKind::Constructor);

        for (int i = 0; i < static_cast<int>(type->fields.size()); ++i)
        {
            context.symbols.declare_parameter(method, type->fields[i]->name, i);
        }
    }

    type->membersPopulated = true;
    return type;
}

}
