#pragma once

#include "bound_tree.hpp"
#include "binding_arena.hpp"
#include "conversions.hpp"
#include "common/error.hpp"

namespace Fern
{
    // Inserts explicit BoundConversionExpression nodes where implicit conversions occur.
    // Run this after TypeResolver has annotated all types.
    class ConversionInserter : public BoundVisitor, public DiagnosticSystem
    {
    private:
        BindingArena& arena_;

        BoundExpression* wrap_if_needed(BoundExpression* expr, TypePtr targetType);

    public:
        explicit ConversionInserter(BindingArena& arena)
            : DiagnosticSystem("ConversionInserter"), arena_(arena) {}

        void transform(BoundCompilationUnit* unit);

        // Visitor implementations
        void visit(BoundBinaryExpression* node) override;
        void visit(BoundAssignmentExpression* node) override;
        void visit(BoundCallExpression* node) override;
        void visit(BoundNewExpression* node) override;
        void visit(BoundReturnStatement* node) override;
        void visit(BoundVariableDeclaration* node) override;
        void visit(BoundArrayCreationExpression* node) override;

        // Pass-through visitors (just visit children)
        void visit(BoundCompilationUnit* node) override;
        void visit(BoundBlockStatement* node) override;
        void visit(BoundExpressionStatement* node) override;
        void visit(BoundIfStatement* node) override;
        void visit(BoundWhileStatement* node) override;
        void visit(BoundForStatement* node) override;
        void visit(BoundFunctionDeclaration* node) override;
        void visit(BoundTypeDeclaration* node) override;
        void visit(BoundNamespaceDeclaration* node) override;
        void visit(BoundPropertyDeclaration* node) override;

        // Leaf visitors (no children to process)
        void visit(BoundLiteralExpression* node) override {}
        void visit(BoundNameExpression* node) override {}
        void visit(BoundUnaryExpression* node) override;
        void visit(BoundMemberAccessExpression* node) override;
        void visit(BoundIndexExpression* node) override;
        void visit(BoundCastExpression* node) override;
        void visit(BoundThisExpression* node) override {}
        void visit(BoundConversionExpression* node) override;
        void visit(BoundTypeExpression* node) override {}
        void visit(BoundBreakStatement* node) override {}
        void visit(BoundContinueStatement* node) override {}
        void visit(BoundUsingStatement* node) override {}
    };

} // namespace Fern
