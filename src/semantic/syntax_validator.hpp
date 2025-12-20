#pragma once
#include "ast/ast.hpp"
#include "common/error.hpp"

namespace Fern
{
    class SyntaxValidator : public DiagnosticSystem, public DefaultVisitor
    {
    public:
        SyntaxValidator() : DiagnosticSystem("SyntaxValidator") {}

        void validate(CompilationUnitSyntax* unit);

    private:
        int loopDepth = 0;
        bool inFunction = false;

        void visit(FunctionDeclSyntax* node) override;
        void visit(ParameterDeclSyntax* node) override;
        void visit(VariableDeclSyntax* node) override;
        void visit(PropertyDeclSyntax* node) override;
        void visit(ConstructorDeclSyntax* node) override;
        void visit(WhileStmtSyntax* node) override;
        void visit(ForStmtSyntax* node) override;
        void visit(ReturnStmtSyntax* node) override;
        void visit(BreakStmtSyntax* node) override;
        void visit(ContinueStmtSyntax* node) override;
        void visit(LambdaExprSyntax* node) override;
        void visit(PropertyAccessorSyntax* node) override;
        void visit(TypeDeclSyntax* node) override;
        void visit(GenericNameSyntax* node) override;
        void visit(NamespaceDeclSyntax* node) override;
        void visit(ArrayLiteralExprSyntax* node) override;
    };

} // namespace Fern
