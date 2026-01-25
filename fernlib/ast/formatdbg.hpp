#pragma once

#include <ast/ast.hpp>
#include <sstream>

namespace Fern
{

class AstDebugFormatter : public DefaultAstVisitor
{
    std::ostringstream out;
    int indent = 0;

    void write_indent()
    {
        for (int i = 0; i < indent; ++i)
            out << "  ";
    }

    void write_child(std::string_view name, BaseSyntax* node)
    {
        write_indent();
        out << name << ":";
        if (node)
        {
            out << "\n";
            ++indent;
            node->accept(this);
            --indent;
        }
        else
        {
            out << " null";
        }
    }

    template<typename T>
    void write_children(std::string_view name, const std::vector<T*>& nodes)
    {
        write_indent();
        out << name << ": [";
        if (!nodes.empty())
        {
            out << "\n";
            ++indent;
            for (auto* node : nodes)
            {
                if (node) node->accept(this);
            }
            --indent;
            write_indent();
        }
        out << "]";
    }

    void begin_node(BaseSyntax* node)
    {
        write_indent();
        out << node->syntax_node_name() << " (span: " << node->span.format() << ")";
    }

    void begin_node_with_name(BaseSyntax* node, std::string_view name)
    {
        write_indent();
        out << node->syntax_node_name() << " (name: \"" << name << "\", span: " << node->span.format() << ")";
    }

public:
    void visit(IdentifierExprSyntax* node) override
    {
        begin_node_with_name(node, node->name);
        out << "\n";
    }

    void visit(LiteralExprSyntax* node) override
    {
        write_indent();
        out << node->syntax_node_name() << " (value: " << node->value << ", span: " << node->span.format() << ")\n";
    }

    void visit(ParenExprSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("expression", node->expression);
        out << "\n";
        --indent;
        write_indent();
        out << "}\n";
    }

    void visit(BlockExprSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_children("statements", node->statements);
        out << ",\n";
        write_child("tailExpression", node->tailExpression);
        out << "\n";
        --indent;
        write_indent();
        out << "}\n";
    }

    void visit(CallExprSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("callee", node->callee);
        out << ",\n";
        write_children("arguments", node->arguments);
        out << "\n";
        --indent;
        write_indent();
        out << "}\n";
    }

    void visit(BinaryExprSyntax* node) override
    {
        write_indent();
        out << node->syntax_node_name() << " (op: " << to_string(node->op) << ", span: " << node->span.format() << ")\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("left", node->left);
        out << ",\n";
        write_child("right", node->right);
        out << "\n";
        --indent;
        write_indent();
        out << "}\n";
    }

    void visit(AssignmentExprSyntax* node) override
    {
        write_indent();
        out << node->syntax_node_name() << " (op: " << to_string(node->op) << ", span: " << node->span.format() << ")\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("target", node->target);
        out << ",\n";
        write_child("value", node->value);
        out << "\n";
        --indent;
        write_indent();
        out << "}\n";
    }

    void visit(TypeExprSyntax* node) override
    {
        begin_node_with_name(node, node->name);
        out << "\n";
    }

    void visit(ReturnStmtSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("value", node->value);
        out << "\n";
        --indent;
        write_indent();
        out << "}\n";
    }

    void visit(ExpressionStmtSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("expression", node->expression);
        out << "\n";
        --indent;
        write_indent();
        out << "}\n";
    }

    void visit(ParameterDeclSyntax* node) override
    {
        begin_node_with_name(node, node->name);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("type", node->type);
        out << "\n";
        --indent;
        write_indent();
        out << "}\n";
    }

    void visit(VariableDeclSyntax* node) override
    {
        begin_node_with_name(node, node->name);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("type", node->type);
        out << ",\n";
        write_child("initializer", node->initializer);
        out << "\n";
        --indent;
        write_indent();
        out << "}\n";
    }

    void visit(FunctionDeclSyntax* node) override
    {
        begin_node_with_name(node, node->name);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_children("parameters", node->parameters);
        out << ",\n";
        write_child("returnType", node->returnType);
        out << ",\n";
        write_child("body", node->body);
        out << "\n";
        --indent;
        write_indent();
        out << "}\n";
    }

    void visit(ProgramSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_children("declarations", node->declarations);
        out << "\n";
        --indent;
        write_indent();
        out << "}\n";
    }

public:
    static std::string format(BaseSyntax* node)
    {
        if (!node) return "";
        AstDebugFormatter formatter;
        node->accept(&formatter);
        return formatter.out.str();
    }
};

}
