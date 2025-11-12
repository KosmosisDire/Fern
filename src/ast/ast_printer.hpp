#pragma once

#include "ast/ast.hpp"
#include "semantic/type.hpp" // For TypePtr and semantic annotations
#include <sstream>
#include <string>
#include <vector>

namespace Fern
{
    /**
     * @brief A visitor that traverses an AST and produces a human-readable string representation,
     * including semantic type annotations for expressions.
     */
    class AstPrinter : public DefaultVisitor
    {
    public:
        /**
         * @brief Prints the given AST node and all its children to a string.
         * @param root The root node of the tree to print.
         * @return A string containing the formatted AST.
         */
        std::string get_string(BaseSyntax *root)
        {
            if (!root)
            {
                return "[Null AST Node]\n";
            }
            output.str(""); // Clear previous content
            output.clear();
            root->accept(this);
            return output.str();
        }

    private:
        std::ostringstream output;
        int indentLevel = 0;

        std::string indent()
        {
            return std::string(indentLevel * 2, ' ');
        }

        // Prints a single line for a leaf node, automatically adding type info.
        void leaf(const BaseSyntax *node, const std::string &name, const std::string &details = "")
        {
            output << indent() << name << details << "\n";
        }

        // Enters a new indentation level for a branch node, automatically adding type info.
        void enter(const BaseSyntax *node, const std::string &name, const std::string &details = "")
        {
            output << indent() << name << details << " {\n";
            indentLevel++;
        }

        // Leaves the current indentation level.
        void leave(const std::string &message = "")
        {
            indentLevel--;
            output << indent() << "}" << message << "\n";
        }

    public:
        // --- Override Visitor Methods ---

        // --- Building Blocks & Errors ---
        void visit(BaseNameExprSyntax *node) override { leaf(node, "Identifier", " (" + node->get_name() + ")"); }
        void visit(MissingExprSyntax *node) override { leaf(node, "MissingExprSyntax", " (\"" + std::string(node->message) + "\")"); }
        void visit(MissingStmtSyntax *node) override { leaf(node, "MissingStmtSyntax", " (\"" + std::string(node->message) + "\")"); }
        void visit(TypedIdentifier *node) override;

        // --- Expressions ---
        void visit(LiteralExprSyntax *node) override;
        void visit(UnaryExprSyntax *node) override;
        void visit(BinaryExprSyntax *node) override;
        void visit(AssignmentExprSyntax *node) override;
        void visit(ThisExprSyntax *node) override { leaf(node, "ThisExprSyntax"); }
        void visit(QualifiedNameSyntax *node) override;
        void visit(ArrayLiteralExprSyntax *node) override { enter(node, "ArrayLiteralExprSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(CallExprSyntax *node) override { enter(node, "CallExprSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(IndexerExprSyntax *node) override { enter(node, "IndexerExprSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(CastExprSyntax *node) override { enter(node, "CastExprSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(NewExprSyntax *node) override { enter(node, "NewExprSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(LambdaExprSyntax *node) override { enter(node, "LambdaExprSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(IfStmtSyntax *node) override { enter(node, "IfStmtSyntax"); DefaultVisitor::visit(node); leave(); }

        // --- Statements ---
        void visit(BlockSyntax *node) override { enter(node, "BlockSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(ExpressionStmtSyntax *node) override { enter(node, "ExpressionStmtSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(ReturnStmtSyntax *node) override { enter(node, "ReturnStmtSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(BreakStmtSyntax *node) override { leaf(node, "BreakStmtSyntax"); }
        void visit(ContinueStmtSyntax *node) override { leaf(node, "ContinueStmtSyntax"); }
        void visit(WhileStmtSyntax *node) override { enter(node, "WhileStmtSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(ForStmtSyntax *node) override 
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
        void visit(UsingDirectiveSyntax *node) override;

        // --- Declarations ---
        void visit(VariableDeclSyntax *node) override;
        void visit(PropertyDeclSyntax *node) override;
        void visit(ParameterDeclSyntax *node) override;
        void visit(FunctionDeclSyntax *node) override;
        void visit(ConstructorDeclSyntax *node) override;
        void visit(PropertyAccessorSyntax *node) override;
        void visit(EnumCaseDeclSyntax *node) override;
        void visit(TypeDeclSyntax *node) override;
        void visit(NamespaceDeclSyntax *node) override;

        // --- Type Expressions ---
        void visit(ArrayTypeSyntax *node) override { enter(node, "ArrayTypeSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(PointerTypeSyntax *node) override { enter(node, "PointerTypeSyntax"); DefaultVisitor::visit(node); leave(); }
        void visit(TypeParameterDeclSyntax *node) override;

        // --- Root ---
        void visit(CompilationUnitSyntax *node) override { enter(node, "CompilationUnitSyntax"); DefaultVisitor::visit(node); leave(); }
    };

    // --- Implementation of methods with more logic ---

    inline void AstPrinter::visit(TypedIdentifier *node)
    {
        enter(node, "TypedIdentifier", " (" + node->name->get_name() + ")");
        if (node->type)
        {
            node->type->accept(this);
        }
        else
        {
            // Use direct output for simple labels not tied to a node
            output << indent() << "Type: var (inferred)\n";
        }
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
        {
            details += ", Postfix";
        }
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
    
    inline void AstPrinter::visit(QualifiedNameSyntax *node)
    {
        std::string rightName = node->right ? node->right->get_name() : "<unknown>";
        enter(node, "QualifiedNameSyntax", " (Right: " + rightName + ")");
        if (node->left) node->left->accept(this);
        if (node->right) node->right->accept(this);
        leave();
    }

    inline void AstPrinter::visit(UsingDirectiveSyntax *node)
    {
        enter(node, "UsingDirectiveSyntax");
        if (node->target)
            node->target->accept(this);
        leave();
    }

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
        std::string kind = (node->kind == PropertyAccessorSyntax::Kind::Get) ? "Get" : "Set";
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
        std::string kind_str;
        switch (node->kind)
        {
        case TypeDeclSyntax::Kind::Type: kind_str = "type"; break;
        case TypeDeclSyntax::Kind::RefType: kind_str = "ref type"; break;
        case TypeDeclSyntax::Kind::StaticType: kind_str = "static type"; break;
        case TypeDeclSyntax::Kind::Enum: kind_str = "enum"; break;
        }
        enter(node, "TypeDeclSyntax", " (" + node->name->get_name() + ", Kind: " + kind_str + ")" + to_string(node->modifiers));
        
        // Print type parameters with label
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

        // Print base types with label
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

        // Print members with label
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
        {
            node->name->accept(this);
        }
        indentLevel--;
        output << indent() << "}\n";

        if (node->body)
        {
            enter(node, "Body");
            for(auto stmt : *node->body)
            {
                stmt->accept(this);
            }
            leave();
        }
        leave();
    }

    inline void AstPrinter::visit(TypeParameterDeclSyntax *node)
    {
        leaf(node, "TypeParameterDeclSyntax", " (" + node->name->get_name() + ")");
    }

} // namespace Fern