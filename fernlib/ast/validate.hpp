#pragma once

#include <ast/ast.hpp>
#include <common/diagnostic.hpp>
#include <token/kind.hpp>

namespace Fern
{

class AstValidator : public DefaultAstVisitor
{
public:
    explicit AstValidator(Diagnostics& diag) : diag(diag) {}

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
            diag.error("namespaces can only be declared at the top level or inside other namespaces", node->span);
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
            diag.error("type declarations are not allowed inside function bodies", node->span);
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
            diag.error("constructors can only be declared inside a type", node->span);
        }
        else if (node->callableKind == CallableKind::Operator && scope != Scope::Type)
        {
            diag.error("operators can only be declared inside a type", node->span);
        }
        else if (node->callableKind == CallableKind::Literal && scope != Scope::Type)
        {
            diag.error("literal declarations can only be declared inside a type", node->span);
        }
        else if (node->callableKind == CallableKind::Cast && scope != Scope::Type)
        {
            diag.error("cast declarations can only be declared inside a type", node->span);
        }
        else if (node->callableKind == CallableKind::Function && scope != Scope::Type)
        {
            diag.error("functions can only be declared inside a type", node->span);
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
            diag.error("fields can only be declared inside a type", node->span);
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
                diag.error("attributes are not allowed on local variable declarations", node->attributes.front()->span);
            }
            if (node->modifiers != Modifier::None)
            {
                diag.error("modifiers are not allowed on local variable declarations", node->span);
            }
        }
        else
        {
            diag.error("Local variable declarations are only allowed inside function bodies", node->span);
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

    Diagnostics& diag;
    Scope scope = Scope::Root;

    void validate_modifiers(BaseDeclSyntax* node)
    {
        if (!node->is<TypeDeclSyntax>())
        {
            if (has_modifier(node->modifiers, Modifier::Ref))
            {
                diag.error("'ref' modifier can only be applied to type declarations", node->span);
            }
            if (has_modifier(node->modifiers, Modifier::Attr))
            {
                diag.error("'attr' modifier can only be applied to type declarations", node->span);
            }
        }

        auto* callable = node->as<CallableDeclSyntax>();
        bool isCast = callable && callable->callableKind == CallableKind::Cast;
        bool hasImplicit = has_modifier(node->modifiers, Modifier::Implicit);
        bool hasExplicit = has_modifier(node->modifiers, Modifier::Explicit);

        if ((hasImplicit || hasExplicit) && !isCast)
        {
            diag.error("'implicit' and 'explicit' modifiers can only be applied to cast declarations", node->span);
        }
        if (isCast && !hasImplicit && !hasExplicit)
        {
            diag.error("cast declarations must be marked 'implicit' or 'explicit'", node->span);
        }
        if (hasImplicit && hasExplicit)
        {
            diag.error("cast declarations cannot be both 'implicit' and 'explicit'", node->span);
        }
    }
};

}
