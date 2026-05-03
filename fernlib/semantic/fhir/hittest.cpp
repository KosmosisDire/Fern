#include "hittest.hpp"

#include <ast/ast.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

namespace
{

bool span_strictly_contains(const Span& outer, const Span& inner)
{
    bool same = outer.startLine == inner.startLine && outer.startColumn == inner.startColumn
             && outer.endLine == inner.endLine && outer.endColumn == inner.endColumn;
    if (same) return false;

    bool startOk = outer.startLine < inner.startLine
                || (outer.startLine == inner.startLine && outer.startColumn <= inner.startColumn);
    bool endOk = outer.endLine > inner.endLine
              || (outer.endLine == inner.endLine && outer.endColumn >= inner.endColumn);
    return startOk && endOk;
}

class FhirHitTestVisitor : public DefaultFhirVisitor
{
public:
    uint32_t line = 0;
    uint32_t col = 0;
    FhirNode* best = nullptr;

    void on_visit(FhirNode* node) override
    {
        if (!node->span.contains(line, col)) return;
        if (!best || span_strictly_contains(best->span, node->span))
        {
            best = node;
        }
    }

    // The outer typeRef of a construction names the type being constructed.
    // We skip recording it so that hits on the type name resolve to the
    // construction itself (which formats as the constructor signature).
    // Nested type args are still visited normally.
    void visit(FhirConstructionExpr* node) override
    {
        on_visit(node);
        if (node->typeRef) node->typeRef->visit_children(this);
        if (node->call) node->call->accept(this);
    }
};

// Walks a (possibly qualified) name expression to find which symbol the cursor
// is on. Returns the leaf symbol if the cursor is on the rightmost name, or
// recurses through the parent chain for namespace or type prefixes on the left.
// QualifiedNameExprSyntax appears in type slots (var x: A.B), MemberAccessExprSyntax
// appears in expression slots (A.B(...)). Returns null if the cursor sits between
// names (e.g. on a dot).
Symbol* resolve_cursor_in_name(BaseSyntax* syntax, Symbol* leafSymbol, uint32_t line, uint32_t col)
{
    if (!syntax || !leafSymbol) return nullptr;

    BaseSyntax* left = nullptr;
    BaseSyntax* right = nullptr;

    if (auto* qual = syntax->as<QualifiedNameExprSyntax>())
    {
        left = qual->left;
        right = qual->right;
    }
    else if (auto* member = syntax->as<MemberAccessExprSyntax>())
    {
        left = member->left;
        right = member->right;
    }

    if (left && right)
    {
        if (right->span.contains(line, col)) return leafSymbol;
        if (left->span.contains(line, col))
        {
            return resolve_cursor_in_name(left, leafSymbol->parent, line, col);
        }
        return nullptr;
    }

    if (syntax->span.contains(line, col)) return leafSymbol;
    return nullptr;
}

}

FhirHitResult FhirHitTest::find(FhirNode* root, uint32_t line, uint32_t col)
{
    if (!root) return {};

    FhirHitTestVisitor visitor;
    visitor.line = line;
    visitor.col = col;
    root->accept(&visitor);

    FhirHitResult result;
    result.node = visitor.best;
    if (!result.node) return result;

    // nameTarget is only set when the cursor is on a non-leaf prefix of a
    // qualified name (e.g. the `Math` in `Math.Vector2`). Hits on the leaf
    // name fall through to the regular formatter on result.node.
    auto setIfPrefix = [&](BaseSyntax* syntax, Symbol* leaf)
    {
        Symbol* sym = resolve_cursor_in_name(syntax, leaf, line, col);
        if (sym && sym != leaf) result.nameTarget = sym;
    };

    if (auto* tref = result.node->as<FhirTypeRef>())
    {
        setIfPrefix(tref->syntax, tref->referenced);
    }
    else if (auto* construct = result.node->as<FhirConstructionExpr>())
    {
        if (construct->typeRef)
        {
            setIfPrefix(construct->typeRef->syntax, construct->typeRef->referenced);
        }
    }
    else if (auto* call = result.node->as<FhirCallExpr>())
    {
        // Call's syntax is the whole CallExprSyntax. The qualified name lives
        // in callee, so navigate one level in.
        if (call->callee && call->callee->method && call->syntax)
        {
            if (auto* callSyntax = call->syntax->as<CallExprSyntax>())
            {
                if (callSyntax->callee)
                {
                    setIfPrefix(callSyntax->callee, call->callee->method);
                }
            }
        }
    }
    else if (auto* fieldRef = result.node->as<FhirFieldRefExpr>())
    {
        setIfPrefix(fieldRef->syntax, fieldRef->field);
    }

    return result;
}

}
