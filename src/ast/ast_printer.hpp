#pragma once

#include "ast/ast.hpp"
#include "semantic/type.hpp"
#include <sstream>
#include <string>

namespace Fern
{
    class AstPrinter : public DefaultVisitor
    {
    public:
        std::string get_string(BaseSyntax *root);

        // Expressions
        void visit(BaseNameExprSyntax *node) override;
        void visit(MissingExprSyntax *node) override;
        void visit(MissingStmtSyntax *node) override;
        void visit(TypedIdentifier *node) override;
        void visit(LiteralExprSyntax *node) override;
        void visit(UnaryExprSyntax *node) override;
        void visit(BinaryExprSyntax *node) override;
        void visit(AssignmentExprSyntax *node) override;
        void visit(ThisExprSyntax *node) override;
        void visit(QualifiedNameSyntax *node) override;
        void visit(ArrayLiteralExprSyntax *node) override;
        void visit(CallExprSyntax *node) override;
        void visit(IndexerExprSyntax *node) override;
        void visit(CastExprSyntax *node) override;
        void visit(NewExprSyntax *node) override;
        void visit(LambdaExprSyntax *node) override;

        // Statements
        void visit(IfStmtSyntax *node) override;
        void visit(BlockSyntax *node) override;
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
        void visit(TypeParameterDeclSyntax *node) override;

        // Root
        void visit(CompilationUnitSyntax *node) override;

    private:
        std::ostringstream output;
        int indentLevel = 0;

        std::string indent();
        void leaf(const BaseSyntax *node, const std::string &name, const std::string &details = "");
        void enter(const BaseSyntax *node, const std::string &name, const std::string &details = "");
        void leave(const std::string &message = "");
    };

#pragma region Implementation

    inline std::string AstPrinter::get_string(BaseSyntax *root)
    {
        if (!root)
            return "[Null AST Node]\n";
        output.str("");
        output.clear();
        root->accept(this);
        return output.str();
    }

    inline std::string AstPrinter::indent()
    {
        return std::string(indentLevel * 2, ' ');
    }

    inline void AstPrinter::leaf(const BaseSyntax *node, const std::string &name, const std::string &details)
    {
        output << indent() << name << details << "\n";
    }

    inline void AstPrinter::enter(const BaseSyntax *node, const std::string &name, const std::string &details)
    {
        output << indent() << name << details << " {\n";
        indentLevel++;
    }

    inline void AstPrinter::leave(const std::string &message)
    {
        indentLevel--;
        output << indent() << "}" << message << "\n";
    }

#pragma region Expressions

    inline void AstPrinter::visit(BaseNameExprSyntax *node)
    {
        leaf(node, "Identifier", " (" + node->get_name() + ")");
    }

    inline void AstPrinter::visit(MissingExprSyntax *node)
    {
        leaf(node, "MissingExprSyntax", " (\"" + std::string(node->message) + "\")");
    }

    inline void AstPrinter::visit(MissingStmtSyntax *node)
    {
        leaf(node, "MissingStmtSyntax", " (\"" + std::string(node->message) + "\")");
    }

    inline void AstPrinter::visit(TypedIdentifier *node)
    {
        enter(node, "TypedIdentifier", " (" + node->name->get_name() + ")");
        if (node->type)
            node->type->accept(this);
        else
            output << indent() << "Type: var (inferred)\n";
        leave();
    }

    inline void AstPrinter::visit(LiteralExprSyntax *node)
    {
        leaf(node, "LiteralExprSyntax", " (Kind: " + std::string(to_string(node->kind)) + ", Value: " + std::string(node->value) + ")");
    }

    inline void AstPrinter::visit(UnaryExprSyntax *node)
    {
        std::string details = " (Op: " + std::string(to_string(node->op));
        if (node->isPostfix)
            details += ", Postfix";
        details += ")";
        enter(node, "UnaryExprSyntax", details);
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(BinaryExprSyntax *node)
    {
        enter(node, "BinaryExprSyntax", " (Op: " + std::string(to_string(node->op)) + ")");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(AssignmentExprSyntax *node)
    {
        enter(node, "AssignmentExprSyntax", " (Op: " + std::string(to_string(node->op)) + ")");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(ThisExprSyntax *node)
    {
        leaf(node, "ThisExprSyntax");
    }

    inline void AstPrinter::visit(QualifiedNameSyntax *node)
    {
        std::string rightName = node->right ? node->right->get_name() : "<unknown>";
        enter(node, "QualifiedNameSyntax", " (Right: " + rightName + ")");
        if (node->left)
            node->left->accept(this);
        if (node->right)
            node->right->accept(this);
        leave();
    }

    inline void AstPrinter::visit(ArrayLiteralExprSyntax *node)
    {
        enter(node, "ArrayLiteralExprSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(CallExprSyntax *node)
    {
        enter(node, "CallExprSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(IndexerExprSyntax *node)
    {
        enter(node, "IndexerExprSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(CastExprSyntax *node)
    {
        enter(node, "CastExprSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(NewExprSyntax *node)
    {
        enter(node, "NewExprSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(LambdaExprSyntax *node)
    {
        enter(node, "LambdaExprSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

#pragma region Statements

    inline void AstPrinter::visit(IfStmtSyntax *node)
    {
        enter(node, "IfStmtSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(BlockSyntax *node)
    {
        enter(node, "BlockSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(ExpressionStmtSyntax *node)
    {
        enter(node, "ExpressionStmtSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(ReturnStmtSyntax *node)
    {
        enter(node, "ReturnStmtSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(BreakStmtSyntax *node)
    {
        leaf(node, "BreakStmtSyntax");
    }

    inline void AstPrinter::visit(ContinueStmtSyntax *node)
    {
        leaf(node, "ContinueStmtSyntax");
    }

    inline void AstPrinter::visit(WhileStmtSyntax *node)
    {
        enter(node, "WhileStmtSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(ForStmtSyntax *node)
    {
        enter(node, "ForStmtSyntax");

        if (node->initializer)
        {
            output << indent() << "Initializer: {\n";
            indentLevel++;
            node->initializer->accept(this);
            indentLevel--;
            output << indent() << "}\n";
        }

        if (node->condition)
        {
            output << indent() << "Condition: {\n";
            indentLevel++;
            node->condition->accept(this);
            indentLevel--;
            output << indent() << "}\n";
        }

        if (!node->updates.empty())
        {
            output << indent() << "Updates: {\n";
            indentLevel++;
            for (auto update : node->updates)
            {
                if (update)
                    update->accept(this);
            }
            indentLevel--;
            output << indent() << "}\n";
        }

        output << indent() << "Body: {\n";
        indentLevel++;
        node->body->accept(this);
        indentLevel--;
        output << indent() << "}\n";

        leave();
    }

    inline void AstPrinter::visit(UsingDirectiveSyntax *node)
    {
        enter(node, "UsingDirectiveSyntax");
        if (node->target)
            node->target->accept(this);
        leave();
    }

#pragma region Declarations

    inline void AstPrinter::visit(VariableDeclSyntax *node)
    {
        std::string name = (node->variable && node->variable->name) ? node->variable->name->get_name() : "<unnamed>";
        enter(node, "VariableDeclSyntax", " (" + name + ")" + to_string(node->modifiers));
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(PropertyDeclSyntax *node)
    {
        std::string name = (node->variable && node->variable->variable && node->variable->variable->name)
                               ? node->variable->variable->name->get_name()
                               : "<invalid>";
        enter(node, "PropertyDeclSyntax", " (" + name + ")" + to_string(node->modifiers));
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(ParameterDeclSyntax *node)
    {
        std::string name = (node->param && node->param->name) ? node->param->name->get_name() : "<unnamed>";
        enter(node, "ParameterDeclSyntax", " (" + name + ")" + to_string(node->modifiers));
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(FunctionDeclSyntax *node)
    {
        enter(node, "FunctionDeclSyntax", " (" + node->name->get_name() + ")" + to_string(node->modifiers));
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(ConstructorDeclSyntax *node)
    {
        enter(node, "ConstructorDeclSyntax", to_string(node->modifiers));
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(PropertyAccessorSyntax *node)
    {
        std::string kind = (node->kind == PropertyKind::Get) ? "Get" : "Set";
        enter(node, "PropertyAccessorSyntax", " (" + kind + ")" + to_string(node->modifiers));
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(EnumCaseDeclSyntax *node)
    {
        enter(node, "EnumCaseDeclSyntax", " (" + node->name->get_name() + ")" + to_string(node->modifiers));
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(TypeDeclSyntax *node)
    {
        std::string kind_str = to_string(node->modifiers) + " type";
        enter(node, "TypeDeclSyntax", " (" + node->name->get_name() + ", Kind: " + kind_str + ")" + to_string(node->modifiers));

        if (!node->typeParameters.empty())
        {
            output << indent() << "TypeParameters: {\n";
            indentLevel++;
            for (auto typeParam : node->typeParameters)
            {
                if (typeParam)
                    typeParam->accept(this);
            }
            indentLevel--;
            output << indent() << "}\n";
        }

        if (!node->baseTypes.empty())
        {
            output << indent() << "BaseTypes: {\n";
            indentLevel++;
            for (auto baseType : node->baseTypes)
            {
                if (baseType)
                    baseType->accept(this);
            }
            indentLevel--;
            output << indent() << "}\n";
        }

        if (!node->members.empty())
        {
            output << indent() << "Members: {\n";
            indentLevel++;
            for (auto member : node->members)
            {
                if (member)
                    member->accept(this);
            }
            indentLevel--;
            output << indent() << "}\n";
        }

        leave(" " + node->name->get_name());
    }

    inline void AstPrinter::visit(NamespaceDeclSyntax *node)
    {
        std::string extra = node->isFileScoped ? ", file-scoped" : "";
        enter(node, "NamespaceDeclSyntax", extra + to_string(node->modifiers));

        output << indent() << "Name: {\n";
        indentLevel++;
        if (node->name)
            node->name->accept(this);
        indentLevel--;
        output << indent() << "}\n";

        if (node->body)
        {
            enter(node, "Body");
            for (auto stmt : *node->body)
                stmt->accept(this);
            leave();
        }
        leave();
    }

#pragma region Type Expressions

    inline void AstPrinter::visit(ArrayTypeSyntax *node)
    {
        enter(node, "ArrayTypeSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(PointerTypeSyntax *node)
    {
        enter(node, "PointerTypeSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

    inline void AstPrinter::visit(TypeParameterDeclSyntax *node)
    {
        leaf(node, "TypeParameterDeclSyntax", " (" + node->name->get_name() + ")");
    }

#pragma region Root

    inline void AstPrinter::visit(CompilationUnitSyntax *node)
    {
        enter(node, "CompilationUnitSyntax");
        DefaultVisitor::visit(node);
        leave();
    }

} // namespace Fern
