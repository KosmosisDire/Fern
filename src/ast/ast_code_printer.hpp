#pragma once

#include "ast/ast.hpp"
#include "common/logger.hpp"
#include <iostream>
#include <sstream>
#include <variant>

namespace Fern
{

    class AstToCodePrinter : public Visitor
    {
    private:
        int indentLevel = 0;
        std::ostringstream output;

        // RAII helper for managing indentation levels safely.
        class IndentGuard
        {
        private:
            int &level;

        public:
            IndentGuard(int &level) : level(level) { this->level++; }
            ~IndentGuard() { this->level--; }
        };

        std::string get_indent()
        {
            return std::string(indentLevel * 2, ' ');
        }

        void emit(const std::string &text)
        {
            output << text;
        }

        void emit_indent()
        {
            output << get_indent();
        }

        void emit_newline()
        {
            output << "\n";
        }

        void print_modifiers(const ModifierKindFlags &modifiers)
        {
            emit(to_string(modifiers) + " ");
        }

        void print_body(BaseStmtSyntax *body)
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

    public:
        std::string get_result()
        {
            std::string result = output.str();
            if (!result.empty() && result.back() == '\n')
            {
                result.pop_back();
            }
            return result;
        }

        // --- Base Node Types (unchanged) ---
        void visit(BaseSyntax *node) override { emit("[AbstractNode]"); }
        void visit(BaseExprSyntax *node) override { emit("[AbstractExpression]"); }
        void visit(BaseStmtSyntax *node) override
        {
            emit_indent();
            emit("[AbstractStatement]");
            emit_newline();
        }
        void visit(BaseDeclSyntax *node) override
        {
            emit_indent();
            print_modifiers(node->modifiers);
            emit("[AbstractDeclaration]");
            emit_newline();
        }

        // --- Basic Building Blocks & Errors (unchanged) ---
        void visit(BaseNameExprSyntax *node) override { emit(node->get_name()); }
        void visit(TypedIdentifier *node) override
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
        void visit(MissingExprSyntax *node) override { emit("[ERROR: " + std::string(node->message) + "]"); }
        void visit(MissingStmtSyntax *node) override
        {
            emit_indent();
            emit("[ERROR: " + std::string(node->message) + "]");
            emit_newline();
        }
        // ErrorTypeRef removed - no longer exists

        // --- Expressions (unchanged) ---
        void visit(LiteralExprSyntax *node) override { emit(std::string(node->value)); }
        void visit(ArrayLiteralExprSyntax *node) override
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
        void visit(SimpleNameSyntax *node) override
        {
            emit(std::string(node->identifier.text));
        }
        void visit(UnaryExprSyntax *node) override
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
        void visit(BinaryExprSyntax *node) override
        {
            node->left->accept(this);
            emit(" " + std::string(to_string(node->op)) + " ");
            node->right->accept(this);
        }
        void visit(AssignmentExprSyntax *node) override
        {
            node->target->accept(this);
            emit(" " + std::string(to_string(node->op)) + " ");
            node->value->accept(this);
        }
        void visit(CallExprSyntax *node) override
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
        void visit(QualifiedNameSyntax *node) override
        {
            if (node->left)
                node->left->accept(this);
            emit(".");
            if (node->right)
                node->right->accept(this);
        }
        void visit(IndexerExprSyntax *node) override
        {
            node->object->accept(this);
            emit("[");
            node->index->accept(this);
            emit("]");
        }
        void visit(CastExprSyntax *node) override
        {
            emit("(");
            node->targetType->accept(this);
            emit(")");
            node->expression->accept(this);
        }
        void visit(NewExprSyntax *node) override
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
        void visit(ThisExprSyntax *node) override { emit("this"); }
        void visit(LambdaExprSyntax *node) override
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
        // --- CORRECTED VISITORS ---

        void visit(BlockSyntax *node) override
        {
            emit("{");
            emit_newline();

            // The guard's scope is explicitly limited to the loop.
            {
                IndentGuard guard(indentLevel);
                for (auto stmt : node->statements)
                {
                    if (stmt)
                        stmt->accept(this);
                }
            } // Guard is destroyed here, indent level is restored.

            emit_indent(); // This now uses the correct, outer indent level.
            emit("}");     // The caller adds the final newline if needed.
        }

        void visit(IfStmtSyntax *node) override
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

        // --- Statements ---
        void visit(ExpressionStmtSyntax *node) override
        {
            emit_indent();
            node->expression->accept(this);
            if (!node->expression->is<IfStmtSyntax>() && !node->expression->is<BlockSyntax>())
            {
                emit(";");
            }
            emit_newline();
        }
        void visit(ReturnStmtSyntax *node) override
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
        void visit(BreakStmtSyntax *node) override
        {
            emit_indent();
            emit("break;");
            emit_newline();
        }
        void visit(ContinueStmtSyntax *node) override
        {
            emit_indent();
            emit("continue;");
            emit_newline();
        }
        void visit(WhileStmtSyntax *node) override
        {
            emit_indent();
            emit("while (");
            node->condition->accept(this);
            emit(")");
            print_body(node->body);
        }
        void visit(ForStmtSyntax *node) override
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

        void visit(UsingDirectiveSyntax *node) override
        {
            emit_indent();
            emit("using ");
            if (node->target)
            {
                node->target->accept(this);
            }
            emit(";");
            emit_newline();
        }

        // --- Declarations (with corrections) ---
        void visit(VariableDeclSyntax *node) override
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

        void visit(PropertyDeclSyntax *node) override
        {
            emit_indent();
            print_modifiers(node->modifiers);
            
            // Print the underlying variable
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


        void visit(ParameterDeclSyntax *node) override
        {
            print_modifiers(node->modifiers);
            node->param->accept(this);
            if (node->defaultValue)
            {
                emit(" = ");
                node->defaultValue->accept(this);
            }
        }

        void visit(FunctionDeclSyntax *node) override
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
        void visit(ConstructorDeclSyntax *node) override
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
        void visit(PropertyAccessorSyntax *node) override
        {
            emit_indent();
            print_modifiers(node->modifiers);
            emit(node->kind == PropertyAccessorSyntax::Kind::Get ? "get" : "set");

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

        void visit(EnumCaseDeclSyntax *node) override
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

        void visit(TypeDeclSyntax *node) override
        {
            emit_indent();
            print_modifiers(node->modifiers);
            switch (node->kind)
            {
            case TypeDeclSyntax::Kind::Type:
                emit("type ");
                break;
            case TypeDeclSyntax::Kind::RefType:
                emit("ref type ");
                break;
            case TypeDeclSyntax::Kind::StaticType:
                emit("static type ");
                break;
            case TypeDeclSyntax::Kind::Enum:
                emit("enum ");
                break;
            }
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

        void visit(NamespaceDeclSyntax *node) override
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

        // --- Type Expressions (now regular expressions) ---

        void visit(ArrayTypeSyntax *node) override
        {
            node->baseType->accept(this);
            emit("[");
            if (node->size)
            {
                node->size->accept(this);
            }
            emit("]");
        }

        void visit(PointerTypeSyntax *node) override
        {
            emit("*");
            node->baseType->accept(this);
        }

        void visit(CompilationUnitSyntax *node) override
        {
            for (auto stmt : node->topLevelStatements)
            {
                if (stmt)
                {
                    stmt->accept(this);
                }
            }
        }
    };

} // namespace Fern