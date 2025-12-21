#pragma once

#include "bound_tree.hpp"
#include <sstream>
#include <string>

namespace Fern
{
    class BoundTreePrinter : public DefaultBoundVisitor
    {
    public:
        std::string get_string(BoundNode *root);

        // Expressions
        void visit(BoundLiteralExpression *node) override;
        void visit(BoundNameExpression *node) override;
        void visit(BoundBinaryExpression *node) override;
        void visit(BoundUnaryExpression *node) override;
        void visit(BoundAssignmentExpression *node) override;
        void visit(BoundCallExpression *node) override;
        void visit(BoundMemberAccessExpression *node) override;
        void visit(BoundIndexExpression *node) override;
        void visit(BoundNewExpression *node) override;
        void visit(BoundArrayCreationExpression *node) override;
        void visit(BoundCastExpression *node) override;
        void visit(BoundThisExpression *node) override;
        void visit(BoundConversionExpression *node) override;
        void visit(BoundTypeExpression *node) override;

        // Statements
        void visit(BoundBlockStatement *node) override;
        void visit(BoundExpressionStatement *node) override;
        void visit(BoundIfStatement *node) override;
        void visit(BoundWhileStatement *node) override;
        void visit(BoundForStatement *node) override;
        void visit(BoundBreakStatement *node) override;
        void visit(BoundContinueStatement *node) override;
        void visit(BoundReturnStatement *node) override;
        void visit(BoundUsingStatement *node) override;

        // Declarations
        void visit(BoundVariableDeclaration *node) override;
        void visit(BoundFunctionDeclaration *node) override;
        void visit(BoundPropertyDeclaration *node) override;
        void visit(BoundTypeDeclaration *node) override;
        void visit(BoundNamespaceDeclaration *node) override;

        // Root
        void visit(BoundCompilationUnit *node) override;

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

        std::string indent();
        void printNode(const std::string &nodeName, const std::string &props = "");
        std::string typeToString(TypePtr type);
        std::string symbolToString(Symbol *sym);
        std::string valueCategoryToString(ValueCategory cat);
        std::string buildExpressionProps(BoundExpression *expr, const std::string &extra = "");
    };

#pragma region Implementation

    inline std::string BoundTreePrinter::get_string(BoundNode *root)
    {
        if (!root)
            return "[Null Bound Node]\n";
        output.str("");
        output.clear();
        root->accept(this);
        return output.str();
    }

    inline std::string BoundTreePrinter::indent()
    {
        return std::string(indentLevel * 2, ' ');
    }

    inline void BoundTreePrinter::printNode(const std::string &nodeName, const std::string &props)
    {
        output << indent() << nodeName;
        if (!props.empty())
            output << " [" << props << "]";
        output << "\n";
    }

    inline std::string BoundTreePrinter::typeToString(TypePtr type)
    {
        if (!type)
            return "null";
        if (type->is<UnresolvedType>())
            return "?" + std::to_string(type->as<UnresolvedType>()->id);
        return type->get_name();
    }

    inline std::string BoundTreePrinter::symbolToString(Symbol *sym)
    {
        if (!sym)
            return "null";
        return sym->get_qualified_name();
    }

    inline std::string BoundTreePrinter::valueCategoryToString(ValueCategory cat)
    {
        return cat == ValueCategory::LValue ? "lvalue" : "rvalue";
    }

    inline std::string BoundTreePrinter::buildExpressionProps(BoundExpression *expr, const std::string &extra)
    {
        std::stringstream ss;
        ss << "type:" << typeToString(expr->type);
        ss << " cat:" << valueCategoryToString(expr->valueCategory);

        if (expr->is_constant())
        {
            ss << " const:";
            if (std::holds_alternative<int64_t>(expr->constantValue))
                ss << std::get<int64_t>(expr->constantValue);
            else if (std::holds_alternative<uint64_t>(expr->constantValue))
                ss << std::get<uint64_t>(expr->constantValue);
            else if (std::holds_alternative<bool>(expr->constantValue))
                ss << (std::get<bool>(expr->constantValue) ? "true" : "false");
            else if (std::holds_alternative<std::string>(expr->constantValue))
                ss << "\"" << std::get<std::string>(expr->constantValue) << "\"";
            else if (std::holds_alternative<double>(expr->constantValue))
                ss << std::get<double>(expr->constantValue);
        }

        if (!extra.empty())
            ss << " " << extra;

        return ss.str();
    }

#pragma region Expressions

    inline void BoundTreePrinter::visit(BoundLiteralExpression *node)
    {
        std::stringstream extra;
        extra << "kind:" << static_cast<int>(node->literalKind);
        printNode("Literal", buildExpressionProps(node, extra.str()));
    }

    inline void BoundTreePrinter::visit(BoundNameExpression *node)
    {
        std::stringstream extra;
        extra << "name:";
        for (size_t i = 0; i < node->parts.size(); ++i)
        {
            if (i > 0)
                extra << ".";
            extra << node->parts[i];
        }
        extra << " sym:" << symbolToString(node->symbol);
        printNode("Name", buildExpressionProps(node, extra.str()));
    }

    inline void BoundTreePrinter::visit(BoundBinaryExpression *node)
    {
        std::stringstream extra;
        extra << "op:" << to_string(node->operatorKind);
        printNode("Binary", buildExpressionProps(node, extra.str()));
        IndentGuard guard(indentLevel);

        output << indent() << "left:\n";
        {
            IndentGuard leftGuard(indentLevel);
            if (node->left)
                node->left->accept(this);
            else
                printNode("null");
        }

        output << indent() << "right:\n";
        {
            IndentGuard rightGuard(indentLevel);
            if (node->right)
                node->right->accept(this);
            else
                printNode("null");
        }
    }

    inline void BoundTreePrinter::visit(BoundUnaryExpression *node)
    {
        std::stringstream extra;
        extra << "op:" << to_string(node->operatorKind);
        printNode("Unary", buildExpressionProps(node, extra.str()));
        IndentGuard guard(indentLevel);

        output << indent() << "operand:\n";
        {
            IndentGuard opGuard(indentLevel);
            if (node->operand)
                node->operand->accept(this);
            else
                printNode("null");
        }
    }

    inline void BoundTreePrinter::visit(BoundAssignmentExpression *node)
    {
        std::stringstream extra;
        extra << "op:" << to_string(node->operatorKind);
        printNode("Assignment", buildExpressionProps(node, extra.str()));
        IndentGuard guard(indentLevel);

        output << indent() << "target:\n";
        {
            IndentGuard targetGuard(indentLevel);
            if (node->target)
                node->target->accept(this);
            else
                printNode("null");
        }

        output << indent() << "value:\n";
        {
            IndentGuard valueGuard(indentLevel);
            if (node->value)
                node->value->accept(this);
            else
                printNode("null");
        }
    }

    inline void BoundTreePrinter::visit(BoundCallExpression *node)
    {
        std::stringstream extra;
        extra << "method:" << symbolToString(node->method);
        extra << " args:" << node->arguments.size();
        printNode("Call", buildExpressionProps(node, extra.str()));
        IndentGuard guard(indentLevel);

        output << indent() << "callee:\n";
        {
            IndentGuard calleeGuard(indentLevel);
            if (node->callee)
                node->callee->accept(this);
            else
                printNode("null");
        }

        if (!node->arguments.empty())
        {
            output << indent() << "arguments:\n";
            IndentGuard argsGuard(indentLevel);
            for (size_t i = 0; i < node->arguments.size(); ++i)
            {
                output << indent() << "[" << i << "]:\n";
                IndentGuard argGuard(indentLevel);
                if (node->arguments[i])
                    node->arguments[i]->accept(this);
                else
                    printNode("null");
            }
        }
    }

    inline void BoundTreePrinter::visit(BoundMemberAccessExpression *node)
    {
        std::stringstream extra;
        extra << "member:" << node->memberName;
        extra << " sym:" << symbolToString(node->member);
        printNode("MemberAccess", buildExpressionProps(node, extra.str()));
        IndentGuard guard(indentLevel);

        output << indent() << "object:\n";
        {
            IndentGuard objGuard(indentLevel);
            if (node->object)
                node->object->accept(this);
            else
                printNode("null");
        }
    }

    inline void BoundTreePrinter::visit(BoundIndexExpression *node)
    {
        printNode("Index", buildExpressionProps(node));
        IndentGuard guard(indentLevel);

        output << indent() << "object:\n";
        {
            IndentGuard objGuard(indentLevel);
            if (node->object)
                node->object->accept(this);
            else
                printNode("null");
        }

        output << indent() << "index:\n";
        {
            IndentGuard indexGuard(indentLevel);
            if (node->index)
                node->index->accept(this);
            else
                printNode("null");
        }
    }

    inline void BoundTreePrinter::visit(BoundNewExpression *node)
    {
        std::stringstream extra;
        extra << "ctor:" << symbolToString(node->constructor);
        extra << " args:" << node->arguments.size();
        printNode("New", buildExpressionProps(node, extra.str()));
        IndentGuard guard(indentLevel);

        output << indent() << "typeExpr:\n";
        {
            IndentGuard typeGuard(indentLevel);
            if (node->typeExpression)
                node->typeExpression->accept(this);
            else
                printNode("null");
        }

        if (!node->arguments.empty())
        {
            output << indent() << "arguments:\n";
            IndentGuard argsGuard(indentLevel);
            for (size_t i = 0; i < node->arguments.size(); ++i)
            {
                output << indent() << "[" << i << "]:\n";
                IndentGuard argGuard(indentLevel);
                if (node->arguments[i])
                    node->arguments[i]->accept(this);
                else
                    printNode("null");
            }
        }
    }

    inline void BoundTreePrinter::visit(BoundArrayCreationExpression *node)
    {
        std::stringstream extra;
        extra << "inits:" << node->initializers.size();
        printNode("ArrayCreation", buildExpressionProps(node, extra.str()));
        IndentGuard guard(indentLevel);

        output << indent() << "elementType:\n";
        {
            IndentGuard typeGuard(indentLevel);
            if (node->elementTypeExpression)
                node->elementTypeExpression->accept(this);
            else
                printNode("null");
        }

        if (node->size)
        {
            output << indent() << "size:\n";
            IndentGuard sizeGuard(indentLevel);
            node->size->accept(this);
        }

        if (!node->initializers.empty())
        {
            output << indent() << "initializers:\n";
            IndentGuard initsGuard(indentLevel);
            for (size_t i = 0; i < node->initializers.size(); ++i)
            {
                output << indent() << "[" << i << "]:\n";
                IndentGuard initGuard(indentLevel);
                if (node->initializers[i])
                    node->initializers[i]->accept(this);
                else
                    printNode("null");
            }
        }
    }

    inline void BoundTreePrinter::visit(BoundCastExpression *node)
    {
        std::stringstream extra;
        extra << "conversion:" << to_string(node->conversionKind);
        printNode("Cast", buildExpressionProps(node, extra.str()));
        IndentGuard guard(indentLevel);

        output << indent() << "expression:\n";
        {
            IndentGuard exprGuard(indentLevel);
            if (node->expression)
                node->expression->accept(this);
            else
                printNode("null");
        }

        output << indent() << "targetType:\n";
        {
            IndentGuard typeGuard(indentLevel);
            if (node->targetTypeExpression)
                node->targetTypeExpression->accept(this);
            else
                printNode("null");
        }
    }

    inline void BoundTreePrinter::visit(BoundThisExpression *node)
    {
        std::stringstream extra;
        extra << "containingType:" << symbolToString(node->containingType);
        printNode("This", buildExpressionProps(node, extra.str()));
    }

    inline void BoundTreePrinter::visit(BoundConversionExpression *node)
    {
        std::stringstream extra;
        extra << "conversion:" << to_string(node->conversionKind);
        printNode("Conversion", buildExpressionProps(node, extra.str()));
        IndentGuard guard(indentLevel);

        output << indent() << "expression:\n";
        {
            IndentGuard exprGuard(indentLevel);
            if (node->expression)
                node->expression->accept(this);
            else
                printNode("null");
        }
    }

    inline void BoundTreePrinter::visit(BoundTypeExpression *node)
    {
        std::stringstream extra;
        extra << "name:";
        for (size_t i = 0; i < node->parts.size(); ++i)
        {
            if (i > 0)
                extra << ".";
            extra << node->parts[i];
        }
        extra << " resolved:" << typeToString(node->resolvedTypeReference);
        printNode("TypeExpr", buildExpressionProps(node, extra.str()));

        if (!node->typeArguments.empty())
        {
            IndentGuard guard(indentLevel);
            output << indent() << "typeArgs:\n";
            IndentGuard argsGuard(indentLevel);
            for (size_t i = 0; i < node->typeArguments.size(); ++i)
            {
                output << indent() << "[" << i << "]:\n";
                IndentGuard argGuard(indentLevel);
                if (node->typeArguments[i])
                    node->typeArguments[i]->accept(this);
                else
                    printNode("null");
            }
        }
    }

#pragma region Statements

    inline void BoundTreePrinter::visit(BoundBlockStatement *node)
    {
        printNode("Block", "stmts:" + std::to_string(node->statements.size()));
        IndentGuard guard(indentLevel);
        for (size_t i = 0; i < node->statements.size(); ++i)
        {
            if (node->statements[i])
                node->statements[i]->accept(this);
            else
                printNode("null");
        }
    }

    inline void BoundTreePrinter::visit(BoundExpressionStatement *node)
    {
        printNode("ExpressionStmt");
        IndentGuard guard(indentLevel);
        if (node->expression)
            node->expression->accept(this);
        else
            printNode("null");
    }

    inline void BoundTreePrinter::visit(BoundIfStatement *node)
    {
        printNode("If");
        IndentGuard guard(indentLevel);

        output << indent() << "condition:\n";
        {
            IndentGuard condGuard(indentLevel);
            if (node->condition)
                node->condition->accept(this);
            else
                printNode("null");
        }

        output << indent() << "then:\n";
        {
            IndentGuard thenGuard(indentLevel);
            if (node->thenStatement)
                node->thenStatement->accept(this);
            else
                printNode("null");
        }

        if (node->elseStatement)
        {
            output << indent() << "else:\n";
            IndentGuard elseGuard(indentLevel);
            node->elseStatement->accept(this);
        }
    }

    inline void BoundTreePrinter::visit(BoundWhileStatement *node)
    {
        printNode("While");
        IndentGuard guard(indentLevel);

        output << indent() << "condition:\n";
        {
            IndentGuard condGuard(indentLevel);
            if (node->condition)
                node->condition->accept(this);
            else
                printNode("null");
        }

        output << indent() << "body:\n";
        {
            IndentGuard bodyGuard(indentLevel);
            if (node->body)
                node->body->accept(this);
            else
                printNode("null");
        }
    }

    inline void BoundTreePrinter::visit(BoundForStatement *node)
    {
        printNode("For", "incs:" + std::to_string(node->incrementors.size()));
        IndentGuard guard(indentLevel);

        if (node->initializer)
        {
            output << indent() << "init:\n";
            IndentGuard initGuard(indentLevel);
            node->initializer->accept(this);
        }

        if (node->condition)
        {
            output << indent() << "condition:\n";
            IndentGuard condGuard(indentLevel);
            node->condition->accept(this);
        }

        if (!node->incrementors.empty())
        {
            output << indent() << "incrementors:\n";
            IndentGuard incsGuard(indentLevel);
            for (size_t i = 0; i < node->incrementors.size(); ++i)
            {
                output << indent() << "[" << i << "]:\n";
                IndentGuard incGuard(indentLevel);
                if (node->incrementors[i])
                    node->incrementors[i]->accept(this);
                else
                    printNode("null");
            }
        }

        output << indent() << "body:\n";
        {
            IndentGuard bodyGuard(indentLevel);
            if (node->body)
                node->body->accept(this);
            else
                printNode("null");
        }
    }

    inline void BoundTreePrinter::visit(BoundBreakStatement *node)
    {
        printNode("Break");
    }

    inline void BoundTreePrinter::visit(BoundContinueStatement *node)
    {
        printNode("Continue");
    }

    inline void BoundTreePrinter::visit(BoundReturnStatement *node)
    {
        printNode("Return", node->value ? "hasValue" : "void");
        if (node->value)
        {
            IndentGuard guard(indentLevel);
            node->value->accept(this);
        }
    }

    inline void BoundTreePrinter::visit(BoundUsingStatement *node)
    {
        std::stringstream extra;
        extra << "namespace:";
        for (size_t i = 0; i < node->namespaceParts.size(); ++i)
        {
            if (i > 0)
                extra << ".";
            extra << node->namespaceParts[i];
        }
        extra << " target:" << symbolToString(node->targetNamespace);
        printNode("Using", extra.str());
    }

#pragma region Declarations

    inline void BoundTreePrinter::visit(BoundVariableDeclaration *node)
    {
        std::stringstream extra;
        extra << "name:" << node->name;
        extra << " sym:" << symbolToString(node->symbol);
        extra << " mods:" << static_cast<int>(node->modifiers);
        if (node->isParameter)
            extra << " param";
        if (node->isLocal)
            extra << " local";
        if (node->isField)
            extra << " field";
        printNode("VarDecl", extra.str());
        IndentGuard guard(indentLevel);

        if (node->typeExpression)
        {
            output << indent() << "type:\n";
            IndentGuard typeGuard(indentLevel);
            node->typeExpression->accept(this);
        }

        if (node->initializer)
        {
            output << indent() << "init:\n";
            IndentGuard initGuard(indentLevel);
            node->initializer->accept(this);
        }
    }

    inline void BoundTreePrinter::visit(BoundFunctionDeclaration *node)
    {
        std::stringstream extra;
        extra << "name:" << node->name;
        extra << " sym:" << symbolToString(node->symbol);
        extra << " mods:" << static_cast<int>(node->modifiers);
        if (node->isConstructor)
            extra << " ctor";
        extra << " params:" << node->parameters.size();
        printNode("FunctionDecl", extra.str());
        IndentGuard guard(indentLevel);

        if (node->returnTypeExpression)
        {
            output << indent() << "returnType:\n";
            IndentGuard typeGuard(indentLevel);
            node->returnTypeExpression->accept(this);
        }

        if (!node->parameters.empty())
        {
            output << indent() << "parameters:\n";
            IndentGuard paramsGuard(indentLevel);
            for (auto *param : node->parameters)
            {
                if (param)
                    param->accept(this);
                else
                    printNode("null");
            }
        }

        if (node->body)
        {
            output << indent() << "body:\n";
            IndentGuard bodyGuard(indentLevel);
            node->body->accept(this);
        }
    }

    inline void BoundTreePrinter::visit(BoundPropertyDeclaration *node)
    {
        std::stringstream extra;
        extra << "name:" << node->name;
        extra << " sym:" << symbolToString(node->symbol);
        extra << " mods:" << static_cast<int>(node->modifiers);
        printNode("PropertyDecl", extra.str());
        IndentGuard guard(indentLevel);

        if (node->typeExpression)
        {
            output << indent() << "type:\n";
            IndentGuard typeGuard(indentLevel);
            node->typeExpression->accept(this);
        }

        if (node->getter)
        {
            output << indent() << "getter:\n";
            IndentGuard getterGuard(indentLevel);
            printNode("PropertyAccessor", "kind:get");
            IndentGuard accessorGuard(indentLevel);

            if (node->getter->expression)
            {
                output << indent() << "expression:\n";
                IndentGuard exprGuard(indentLevel);
                node->getter->expression->accept(this);
            }

            if (node->getter->body)
            {
                output << indent() << "body:\n";
                IndentGuard bodyGuard(indentLevel);
                node->getter->body->accept(this);
            }
        }

        if (node->setter)
        {
            output << indent() << "setter:\n";
            IndentGuard setterGuard(indentLevel);
            printNode("PropertyAccessor", "kind:set");
            IndentGuard accessorGuard(indentLevel);

            if (node->setter->expression)
            {
                output << indent() << "expression:\n";
                IndentGuard exprGuard(indentLevel);
                node->setter->expression->accept(this);
            }

            if (node->setter->body)
            {
                output << indent() << "body:\n";
                IndentGuard bodyGuard(indentLevel);
                node->setter->body->accept(this);
            }
        }
    }

    inline void BoundTreePrinter::visit(BoundTypeDeclaration *node)
    {
        std::stringstream extra;
        extra << "name:" << node->name;
        extra << " sym:" << symbolToString(node->symbol);
        extra << " mods:" << static_cast<int>(node->modifiers);
        extra << " members:" << node->members.size();
        printNode("TypeDecl", extra.str());
        IndentGuard guard(indentLevel);

        if (!node->members.empty())
        {
            output << indent() << "members:\n";
            IndentGuard membersGuard(indentLevel);
            for (auto *member : node->members)
            {
                if (member)
                    member->accept(this);
                else
                    printNode("null");
            }
        }
    }

    inline void BoundTreePrinter::visit(BoundNamespaceDeclaration *node)
    {
        std::stringstream extra;
        extra << "name:" << node->name;
        extra << " sym:" << symbolToString(node->symbol);
        extra << " members:" << node->members.size();
        printNode("NamespaceDecl", extra.str());
        IndentGuard guard(indentLevel);

        if (!node->members.empty())
        {
            output << indent() << "members:\n";
            IndentGuard membersGuard(indentLevel);
            for (auto *member : node->members)
            {
                if (member)
                    member->accept(this);
                else
                    printNode("null");
            }
        }
    }

#pragma region Root

    inline void BoundTreePrinter::visit(BoundCompilationUnit *node)
    {
        printNode("CompilationUnit", "stmts:" + std::to_string(node->statements.size()));
        IndentGuard guard(indentLevel);
        for (auto *stmt : node->statements)
        {
            if (stmt)
                stmt->accept(this);
            else
                printNode("null");
        }
    }

} // namespace Fern
