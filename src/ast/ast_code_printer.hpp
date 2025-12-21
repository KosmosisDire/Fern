#pragma once

#include "ast/ast.hpp"
#include <sstream>
#include <variant>

namespace Fern
{
    class AstCodePrinter : public Visitor
    {
    public:
        std::string get_string(BaseSyntax *root);

        // Base Node Types
        void visit(BaseSyntax *node) override;
        void visit(BaseExprSyntax *node) override;
        void visit(BaseStmtSyntax *node) override;
        void visit(BaseDeclSyntax *node) override;

        // Building Blocks & Errors
        void visit(BaseNameExprSyntax *node) override;
        void visit(SimpleNameSyntax *node) override;
        void visit(TypedIdentifier *node) override;
        void visit(MissingExprSyntax *node) override;
        void visit(MissingStmtSyntax *node) override;

        // Expressions
        void visit(LiteralExprSyntax *node) override;
        void visit(ArrayLiteralExprSyntax *node) override;
        void visit(UnaryExprSyntax *node) override;
        void visit(BinaryExprSyntax *node) override;
        void visit(AssignmentExprSyntax *node) override;
        void visit(CallExprSyntax *node) override;
        void visit(QualifiedNameSyntax *node) override;
        void visit(IndexerExprSyntax *node) override;
        void visit(CastExprSyntax *node) override;
        void visit(NewExprSyntax *node) override;
        void visit(ThisExprSyntax *node) override;
        void visit(LambdaExprSyntax *node) override;

        // Control Flow
        void visit(BlockSyntax *node) override;
        void visit(IfStmtSyntax *node) override;

        // Statements
        void visit(ExpressionStmtSyntax *node) override;
        void visit(ReturnStmtSyntax *node) override;
        void visit(BreakStmtSyntax *node) override;
        void visit(ContinueStmtSyntax *node) override;
        void visit(WhileStmtSyntax *node) override;
        void visit(ForStmtSyntax *node) override;
        void visit(UsingDirectiveSyntax *node) override;

        // Declarations
        void visit(VariableDeclSyntax *node) override;
        void visit(PropertyDeclSyntax *node) override;
        void visit(ParameterDeclSyntax *node) override;
        void visit(FunctionDeclSyntax *node) override;
        void visit(ConstructorDeclSyntax *node) override;
        void visit(PropertyAccessorSyntax *node) override;
        void visit(EnumCaseDeclSyntax *node) override;
        void visit(TypeDeclSyntax *node) override;
        void visit(NamespaceDeclSyntax *node) override;

        // Type Expressions
        void visit(ArrayTypeSyntax *node) override;
        void visit(PointerTypeSyntax *node) override;

        // Root
        void visit(CompilationUnitSyntax *node) override;

    private:
        std::ostringstream output;
        int indentLevel = 0;

        class IndentGuard
        {
            int &level;
        public:
            IndentGuard(int &level) : level(level) { this->level++; }
            ~IndentGuard() { this->level--; }
        };

        std::string get_indent();
        void emit(const std::string &text);
        void emit_indent();
        void emit_newline();
        void print_modifiers(const ModifierKindFlags &modifiers);
        void print_body(BaseStmtSyntax *body);
    };

#pragma region Implementation

    inline std::string AstCodePrinter::get_string(BaseSyntax *root)
    {
        if (!root)
            return "[Null AST Node]\n";
        output.str("");
        output.clear();
        root->accept(this);
        std::string result = output.str();
        if (!result.empty() && result.back() == '\n')
            result.pop_back();
        return result;
    }

    inline std::string AstCodePrinter::get_indent()
    {
        return std::string(indentLevel * 2, ' ');
    }

    inline void AstCodePrinter::emit(const std::string &text)
    {
        output << text;
    }

    inline void AstCodePrinter::emit_indent()
    {
        output << get_indent();
    }

    inline void AstCodePrinter::emit_newline()
    {
        output << "\n";
    }

    inline void AstCodePrinter::print_modifiers(const ModifierKindFlags &modifiers)
    {
        emit(to_string(modifiers) + " ");
    }

    inline void AstCodePrinter::print_body(BaseStmtSyntax *body)
    {
        if (!body)
        {
            emit(";\n");
            return;
        }

        if (body->is<BlockSyntax>())
        {
            emit(" ");
            body->accept(this);
            emit_newline();
        }
        else
        {
            emit_newline();
            IndentGuard guard(indentLevel);
            body->accept(this);
        }
    }

#pragma region Base Node Types

    inline void AstCodePrinter::visit(BaseSyntax *node)
    {
        emit("[AbstractNode]");
    }

    inline void AstCodePrinter::visit(BaseExprSyntax *node)
    {
        emit("[AbstractExpression]");
    }

    inline void AstCodePrinter::visit(BaseStmtSyntax *node)
    {
        emit_indent();
        emit("[AbstractStatement]");
        emit_newline();
    }

    inline void AstCodePrinter::visit(BaseDeclSyntax *node)
    {
        emit_indent();
        print_modifiers(node->modifiers);
        emit("[AbstractDeclaration]");
        emit_newline();
    }

#pragma region Building Blocks & Errors

    inline void AstCodePrinter::visit(BaseNameExprSyntax *node)
    {
        emit(node->get_name());
    }

    inline void AstCodePrinter::visit(SimpleNameSyntax *node)
    {
        emit(std::string(node->identifier.text));
    }

    inline void AstCodePrinter::visit(TypedIdentifier *node)
    {
        if (node->type)
        {
            node->type->accept(this);
            emit(" ");
        }
        else
        {
            emit("var ");
        }
        if (node->name)
            node->name->accept(this);
    }

    inline void AstCodePrinter::visit(MissingExprSyntax *node)
    {
        emit("[ERROR: " + std::string(node->message) + "]");
    }

    inline void AstCodePrinter::visit(MissingStmtSyntax *node)
    {
        emit_indent();
        emit("[ERROR: " + std::string(node->message) + "]");
        emit_newline();
    }

#pragma region Expressions

    inline void AstCodePrinter::visit(LiteralExprSyntax *node)
    {
        emit(std::string(node->value));
    }

    inline void AstCodePrinter::visit(ArrayLiteralExprSyntax *node)
    {
        emit("[");
        for (size_t i = 0; i < node->elements.size(); i++)
        {
            if (i > 0)
                emit(", ");
            if (node->elements[i])
                node->elements[i]->accept(this);
        }
        emit("]");
    }

    inline void AstCodePrinter::visit(UnaryExprSyntax *node)
    {
        if (node->isPostfix)
        {
            node->operand->accept(this);
            emit(std::string(to_string(node->op)));
        }
        else
        {
            emit(std::string(to_string(node->op)));
            node->operand->accept(this);
        }
    }

    inline void AstCodePrinter::visit(BinaryExprSyntax *node)
    {
        node->left->accept(this);
        emit(" " + std::string(to_string(node->op)) + " ");
        node->right->accept(this);
    }

    inline void AstCodePrinter::visit(AssignmentExprSyntax *node)
    {
        node->target->accept(this);
        emit(" " + std::string(to_string(node->op)) + " ");
        node->value->accept(this);
    }

    inline void AstCodePrinter::visit(CallExprSyntax *node)
    {
        node->callee->accept(this);
        emit("(");
        for (size_t i = 0; i < node->arguments.size(); i++)
        {
            if (i > 0)
                emit(", ");
            if (node->arguments[i])
                node->arguments[i]->accept(this);
        }
        emit(")");
    }

    inline void AstCodePrinter::visit(QualifiedNameSyntax *node)
    {
        if (node->left)
            node->left->accept(this);
        emit(".");
        if (node->right)
            node->right->accept(this);
    }

    inline void AstCodePrinter::visit(IndexerExprSyntax *node)
    {
        node->object->accept(this);
        emit("[");
        node->index->accept(this);
        emit("]");
    }

    inline void AstCodePrinter::visit(CastExprSyntax *node)
    {
        emit("(");
        node->targetType->accept(this);
        emit(")");
        node->expression->accept(this);
    }

    inline void AstCodePrinter::visit(NewExprSyntax *node)
    {
        emit("new ");
        node->type->accept(this);
        emit("(");
        for (size_t i = 0; i < node->arguments.size(); i++)
        {
            if (i > 0)
                emit(", ");
            if (node->arguments[i])
                node->arguments[i]->accept(this);
        }
        emit(")");
    }

    inline void AstCodePrinter::visit(ThisExprSyntax *node)
    {
        emit("this");
    }

    inline void AstCodePrinter::visit(LambdaExprSyntax *node)
    {
        emit("(");
        for (size_t i = 0; i < node->parameters.size(); i++)
        {
            if (i > 0)
                emit(", ");
            if (node->parameters[i])
                node->parameters[i]->accept(this);
        }
        emit(") => ");
        if (node->body)
            node->body->accept(this);
    }

#pragma region Control Flow

    inline void AstCodePrinter::visit(BlockSyntax *node)
    {
        emit("{");
        emit_newline();
        {
            IndentGuard guard(indentLevel);
            for (auto stmt : node->statements)
            {
                if (stmt)
                    stmt->accept(this);
            }
        }
        emit_indent();
        emit("}");
    }

    inline void AstCodePrinter::visit(IfStmtSyntax *node)
    {
        emit("if (");
        node->condition->accept(this);
        emit(")");
        print_body(node->thenBranch);

        if (node->elseBranch)
        {
            output.seekp(-1, std::ios_base::end);
            emit(" else");

            if (node->elseBranch->is<IfStmtSyntax>())
            {
                emit(" ");
                node->elseBranch->accept(this);
            }
            else
            {
                print_body(node->elseBranch);
            }
        }
    }

#pragma region Statements

    inline void AstCodePrinter::visit(ExpressionStmtSyntax *node)
    {
        emit_indent();
        node->expression->accept(this);
        if (!node->expression->is<IfStmtSyntax>() && !node->expression->is<BlockSyntax>())
            emit(";");
        emit_newline();
    }

    inline void AstCodePrinter::visit(ReturnStmtSyntax *node)
    {
        emit_indent();
        emit("return");
        if (node->value)
        {
            emit(" ");
            node->value->accept(this);
        }
        emit(";");
        emit_newline();
    }

    inline void AstCodePrinter::visit(BreakStmtSyntax *node)
    {
        emit_indent();
        emit("break;");
        emit_newline();
    }

    inline void AstCodePrinter::visit(ContinueStmtSyntax *node)
    {
        emit_indent();
        emit("continue;");
        emit_newline();
    }

    inline void AstCodePrinter::visit(WhileStmtSyntax *node)
    {
        emit_indent();
        emit("while (");
        node->condition->accept(this);
        emit(")");
        print_body(node->body);
    }

    inline void AstCodePrinter::visit(ForStmtSyntax *node)
    {
        emit_indent();
        emit("for (");
        if (node->initializer)
        {
            if (auto varDecl = node->initializer->as<VariableDeclSyntax>())
            {
                print_modifiers(varDecl->modifiers);
                varDecl->variable->accept(this);
                if (varDecl->initializer)
                {
                    emit(" = ");
                    varDecl->initializer->accept(this);
                }
            }
            else if (auto exprStmt = node->initializer->as<ExpressionStmtSyntax>())
            {
                exprStmt->expression->accept(this);
            }
        }
        emit("; ");
        if (node->condition)
            node->condition->accept(this);
        emit("; ");
        for (size_t i = 0; i < node->updates.size(); i++)
        {
            if (i > 0)
                emit(", ");
            if (node->updates[i])
                node->updates[i]->accept(this);
        }
        emit(")");
        print_body(node->body);
    }

    inline void AstCodePrinter::visit(UsingDirectiveSyntax *node)
    {
        emit_indent();
        emit("using ");
        if (node->target)
            node->target->accept(this);
        emit(";");
        emit_newline();
    }

#pragma region Declarations

    inline void AstCodePrinter::visit(VariableDeclSyntax *node)
    {
        emit_indent();
        print_modifiers(node->modifiers);
        node->variable->accept(this);
        if (node->initializer)
        {
            emit(" = ");
            node->initializer->accept(this);
        }
        emit(";");
        emit_newline();
    }

    inline void AstCodePrinter::visit(PropertyDeclSyntax *node)
    {
        emit_indent();
        print_modifiers(node->modifiers);

        if (node->variable)
        {
            if (node->variable->variable->type)
            {
                node->variable->variable->type->accept(this);
                emit(" ");
            }
            else
            {
                emit("var ");
            }
            node->variable->variable->name->accept(this);

            if (node->variable->initializer)
            {
                emit(" = ");
                node->variable->initializer->accept(this);
            }
        }

        if (node->getter || node->setter)
        {
            emit(" {");
            emit_newline();
            {
                IndentGuard guard(indentLevel);
                if (node->getter)
                    node->getter->accept(this);
                if (node->setter)
                    node->setter->accept(this);
            }
            emit_indent();
            emit("}");
            emit_newline();
        }
        else
        {
            emit(";");
            emit_newline();
        }
    }

    inline void AstCodePrinter::visit(ParameterDeclSyntax *node)
    {
        print_modifiers(node->modifiers);
        node->param->accept(this);
        if (node->defaultValue)
        {
            emit(" = ");
            node->defaultValue->accept(this);
        }
    }

    inline void AstCodePrinter::visit(FunctionDeclSyntax *node)
    {
        emit_indent();
        print_modifiers(node->modifiers);
        emit("fn ");
        node->name->accept(this);

        emit("(");
        for (size_t i = 0; i < node->parameters.size(); i++)
        {
            if (i > 0)
                emit(", ");
            node->parameters[i]->accept(this);
        }
        emit(")");

        if (node->returnType)
        {
            emit(": ");
            node->returnType->accept(this);
        }

        if (node->body)
        {
            emit_newline();
            emit_indent();
            node->body->accept(this);
            emit_newline();
        }
        else
        {
            emit(";");
            emit_newline();
        }
    }

    inline void AstCodePrinter::visit(ConstructorDeclSyntax *node)
    {
        emit_indent();
        print_modifiers(node->modifiers);
        emit("new(");
        for (size_t i = 0; i < node->parameters.size(); i++)
        {
            if (i > 0)
                emit(", ");
            node->parameters[i]->accept(this);
        }
        emit(")");
        emit_newline();
        emit_indent();
        node->body->accept(this);
        emit_newline();
    }

    inline void AstCodePrinter::visit(PropertyAccessorSyntax *node)
    {
        emit_indent();
        print_modifiers(node->modifiers);
        emit(node->kind == PropertyKind::Get ? "get" : "set");

        if (auto expr = std::get_if<BaseExprSyntax *>(&node->body))
        {
            emit(" => ");
            (*expr)->accept(this);
            emit(";");
        }
        else if (auto block = std::get_if<BlockSyntax *>(&node->body))
        {
            emit(" ");
            (*block)->accept(this);
        }
        else
        {
            emit(";");
        }
        emit_newline();
    }

    inline void AstCodePrinter::visit(EnumCaseDeclSyntax *node)
    {
        emit_indent();
        print_modifiers(node->modifiers);
        emit("case ");
        node->name->accept(this);
        if (!node->associatedData.empty())
        {
            emit("(");
            for (size_t i = 0; i < node->associatedData.size(); i++)
            {
                if (i > 0)
                    emit(", ");
                node->associatedData[i]->accept(this);
            }
            emit(")");
        }
        emit(",");
        emit_newline();
    }

    inline void AstCodePrinter::visit(TypeDeclSyntax *node)
    {
        emit_indent();
        print_modifiers(node->modifiers);
        emit("type ");
        node->name->accept(this);

        if (!node->baseTypes.empty())
        {
            emit(" : ");
            for (size_t i = 0; i < node->baseTypes.size(); i++)
            {
                if (i > 0)
                    emit(", ");
                node->baseTypes[i]->accept(this);
            }
        }

        emit_newline();
        emit_indent();
        emit("{");
        emit_newline();
        {
            IndentGuard guard(indentLevel);
            for (auto member : node->members)
            {
                if (member)
                    member->accept(this);
            }
        }
        emit_indent();
        emit("}");
        emit_newline();
    }

    inline void AstCodePrinter::visit(NamespaceDeclSyntax *node)
    {
        emit_indent();
        print_modifiers(node->modifiers);
        emit("namespace ");
        if (node->name)
            node->name->accept(this);

        if (node->isFileScoped)
        {
            emit(";");
            emit_newline();
        }
        else if (node->body)
        {
            emit_newline();
            emit_indent();
            emit("{");
            emit_newline();
            {
                IndentGuard guard(indentLevel);
                for (auto stmt : *node->body)
                {
                    if (stmt)
                        stmt->accept(this);
                }
            }
            emit_indent();
            emit("}");
            emit_newline();
        }
    }

#pragma region Type Expressions

    inline void AstCodePrinter::visit(ArrayTypeSyntax *node)
    {
        node->baseType->accept(this);
        emit("[");
        if (node->size)
            node->size->accept(this);
        emit("]");
    }

    inline void AstCodePrinter::visit(PointerTypeSyntax *node)
    {
        emit("*");
        node->baseType->accept(this);
    }

#pragma region Root

    inline void AstCodePrinter::visit(CompilationUnitSyntax *node)
    {
        for (auto stmt : node->topLevelStatements)
        {
            if (stmt)
                stmt->accept(this);
        }
    }

} // namespace Fern
