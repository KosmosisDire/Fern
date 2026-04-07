#pragma once

#include <ast/ast.hpp>
#include <common/diagnostic.hpp>
#include <token/kind.hpp>

namespace Fern
{

class AstValidator : public DefaultAstVisitor, public DiagnosticSystem
{
public:
    AstValidator() : DiagnosticSystem("AstValidator") {}

    void validate(RootSyntax* root)
    {
        root->accept(this);
    }

    void visit(RootSyntax* node) override
    {
        Scope prev = scope;
        scope = Scope::Root;
        DefaultAstVisitor::visit(node);
        scope = prev;
    }

    void visit(NamespaceDeclSyntax* node) override
    {
        if (scope != Scope::Root && scope != Scope::Namespace)
        {
            error("namespaces can only be declared at the top level or inside other namespaces", node->span);
        }

        Scope prev = scope;
        scope = Scope::Namespace;
        DefaultAstVisitor::visit(node);
        scope = prev;
    }

    void visit(TypeDeclSyntax* node) override
    {
        if (scope == Scope::Function)
        {
            error("type declarations are not allowed inside function bodies", node->span);
        }

        validate_modifiers(node);

        Scope prev = scope;
        scope = Scope::Type;
        DefaultAstVisitor::visit(node);
        scope = prev;
    }

    void visit(CallableDeclSyntax* node) override
    {
        if (node->callableKind == CallableKind::Constructor && scope != Scope::Type)
        {
            error("constructors can only be declared inside a type", node->span);
        }
        else if (node->callableKind == CallableKind::Operator && scope != Scope::Type)
        {
            error("operators can only be declared inside a type", node->span);
        }
        else if (node->callableKind == CallableKind::Literal && scope != Scope::Type)
        {
            error("literal declarations can only be declared inside a type", node->span);
        }
        else if (node->callableKind == CallableKind::Function && scope == Scope::Function)
        {
            error("nested functions are not yet supported", node->span);
        }

        validate_modifiers(node);

        Scope prev = scope;
        scope = Scope::Function;
        DefaultAstVisitor::visit(node);
        scope = prev;
    }

    void visit(BlockSyntax* node) override
    {
        Scope prev = scope;
        scope = Scope::Function;
        DefaultAstVisitor::visit(node);
        scope = prev;
    }

    void visit(FieldDeclSyntax* node) override
    {
        if (scope != Scope::Type)
        {
            error("fields can only be declared inside a type", node->span);
        }

        validate_modifiers(node);
        DefaultAstVisitor::visit(node);
    }

    void visit(VariableDeclSyntax* node) override
    {
        if (scope == Scope::Function)
        {
            if (!node->attributes.empty())
            {
                error("attributes are not allowed on local variable declarations", node->attributes.front()->span);
            }
            if (node->modifiers != Modifier::None)
            {
                error("modifiers are not allowed on local variable declarations", node->span);
            }
        }

        validate_modifiers(node);
        DefaultAstVisitor::visit(node);
    }

private:
    enum class Scope
    {
        Root,
        Namespace,
        Type,
        Function,
    };

    Scope scope = Scope::Root;

    void validate_modifiers(BaseDeclSyntax* node)
    {
        if (!node->is<TypeDeclSyntax>())
        {
            if (has_modifier(node->modifiers, Modifier::Ref))
            {
                error("'ref' modifier can only be applied to type declarations", node->span);
            }
            if (has_modifier(node->modifiers, Modifier::Attr))
            {
                error("'attr' modifier can only be applied to type declarations", node->span);
            }
        }
    }
};

}
