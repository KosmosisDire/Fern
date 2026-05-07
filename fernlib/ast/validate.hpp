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
        if (scope == Scope::Type)
        {
            diag.report(DiagnosticCode::Err_BadTypeContent, node->span);
        }
        else if (scope == Scope::Function)
        {
            diag.report(DiagnosticCode::Err_BadFunctionBodyContent, node->span);
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
            diag.report(DiagnosticCode::Err_BadFunctionBodyContent, node->span);
        }

        validate_modifiers(node);

        Scope prev = scope;
        scope = Scope::Type;
        DefaultAstVisitor::visit(node);
        scope = prev;
    }

    void visit(CallableDeclSyntax* node) override
    {
        if (scope == Scope::Root || scope == Scope::Namespace)
        {
            diag.report(DiagnosticCode::Err_BadNamespaceContent, node->span);
        }
        else if (scope == Scope::Function)
        {
            diag.report(DiagnosticCode::Err_BadFunctionBodyContent, node->span);
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
        if (scope == Scope::Root || scope == Scope::Namespace)
        {
            diag.report(DiagnosticCode::Err_BadNamespaceContent, node->span);
        }
        else if (scope == Scope::Function)
        {
            diag.report(DiagnosticCode::Err_BadFunctionBodyContent, node->span);
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
                diag.report(DiagnosticCode::Err_AttrOnLocal, node->attributes.front()->span);
            }
            if (node->modifiers != Modifier::None)
            {
                diag.report(DiagnosticCode::Err_ModifierOnLocal, node->span);
            }
        }
        else if (scope == Scope::Root || scope == Scope::Namespace)
        {
            diag.report(DiagnosticCode::Err_BadNamespaceContent, node->span);
        }
        else if (scope == Scope::Type)
        {
            diag.report(DiagnosticCode::Err_BadTypeContent, node->span);
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

    // TODO: BaseDeclSyntax needs to track per-modifier source tokens so we can highlight modifier spans
    void validate_modifiers(BaseDeclSyntax* node)
    {
        if (!node->is<TypeDeclSyntax>())
        {
            if (has_modifier(node->modifiers, Modifier::Ref))
            {
                diag.report(DiagnosticCode::Err_BadModifierTarget, node->span, "ref", "type");
            }
            if (has_modifier(node->modifiers, Modifier::Attr))
            {
                diag.report(DiagnosticCode::Err_BadModifierTarget, node->span, "attr", "type");
            }
        }

        auto* callable = node->as<CallableDeclSyntax>();
        bool isCast = callable && callable->callableKind == CallableKind::Cast;
        bool hasImplicit = has_modifier(node->modifiers, Modifier::Implicit);
        bool hasExplicit = has_modifier(node->modifiers, Modifier::Explicit);

        if (!isCast)
        {
            if (hasImplicit)
            {
                diag.report(DiagnosticCode::Err_BadModifierTarget, node->span, "implicit", "cast");
            }
            if (hasExplicit)
            {
                diag.report(DiagnosticCode::Err_BadModifierTarget, node->span, "explicit", "cast");
            }
        }
        if (isCast && !hasImplicit && !hasExplicit)
        {
            diag.report(DiagnosticCode::Err_CastMissingExplicitness, node->span);
        }
        if (hasImplicit && hasExplicit)
        {
            diag.report(DiagnosticCode::Err_CastBothExplicitness, node->span);
        }
    }
};

}
