#pragma once

#include <ast/ast.hpp>
#include <sstream>

namespace Fern
{

class AstDebugFormatter : public DefaultAstVisitor
{
    std::ostringstream out;
    int indent = 0;
    bool suppressNextIndent = false;

#pragma region Helpers

    void write_indent()
    {
        for (int i = 0; i < indent; ++i)
            out << "  ";
    }

    void maybe_write_indent()
    {
        if (!suppressNextIndent)
            write_indent();
        suppressNextIndent = false;
    }

    void write_child(std::string_view name, BaseSyntax* node, bool addComma = false)
    {
        write_indent();
        out << name << ": ";
        if (node)
        {
            suppressNextIndent = true;
            node->accept(this);
        }
        else
        {
            out << "null";
        }
        if (addComma) out << ",";
        out << "\n";
    }

    template<typename T>
    void write_children(std::string_view name, const std::vector<T*>& nodes, bool addComma = false)
    {
        write_indent();
        out << name << ": [";
        if (!nodes.empty())
        {
            out << "\n";
            ++indent;
            for (auto* node : nodes)
            {
                if (node)
                {
                    node->accept(this);
                    out << "\n";
                }
            }
            --indent;
            write_indent();
        }
        out << "]";
        if (addComma) out << ",";
        out << "\n";
    }

    void begin_node(BaseSyntax* node)
    {
        maybe_write_indent();
        out << node->syntax_node_name() << " (span: " << node->span.format() << ")";
    }

    void begin_node_with_name(BaseSyntax* node, std::string_view name)
    {
        maybe_write_indent();
        out << node->syntax_node_name() << " (name: \"" << name << "\", span: " << node->span.format() << ")";
    }

#pragma region Expression Visitors

public:
    void visit(IdentifierExprSyntax* node) override
    {
        begin_node_with_name(node, node->name);
    }

    void visit(LiteralExprSyntax* node) override
    {
        maybe_write_indent();
        out << node->syntax_node_name() << " (value: " << node->value << ", span: " << node->span.format() << ")";
    }

    void visit(ParenExprSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("expression", node->expression);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(BlockExprSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_children("statements", node->statements);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(CallExprSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("callee", node->callee, true);
        write_children("arguments", node->arguments);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(BinaryExprSyntax* node) override
    {
        maybe_write_indent();
        out << node->syntax_node_name() << " (op: " << Fern::format(node->op) << ", span: " << node->span.format() << ")\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("left", node->left, true);
        write_child("right", node->right);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(AssignmentExprSyntax* node) override
    {
        maybe_write_indent();
        out << node->syntax_node_name() << " (op: " << Fern::format(node->op) << ", span: " << node->span.format() << ")\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("target", node->target, true);
        write_child("value", node->value);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(TypeExprSyntax* node) override
    {
        begin_node_with_name(node, node->name);
    }

#pragma region Statement Visitors

    void visit(ReturnStmtSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("value", node->value);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(ExpressionStmtSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("expression", node->expression);
        --indent;
        write_indent();
        out << "}";
    }

#pragma region Declaration Visitors

    void visit(ParameterDeclSyntax* node) override
    {
        begin_node_with_name(node, node->name);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("type", node->type);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(VariableDeclSyntax* node) override
    {
        begin_node_with_name(node, node->name);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("type", node->type, true);
        write_child("initializer", node->initializer);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(FunctionDeclSyntax* node) override
    {
        begin_node_with_name(node, node->name);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_children("parameters", node->parameters, true);
        write_child("returnType", node->returnType, true);
        write_child("body", node->body);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(ProgramSyntax* node) override
    {
        begin_node(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_children("declarations", node->declarations);
        --indent;
        write_indent();
        out << "}";
    }

#pragma region Public

public:
    static std::string format(BaseSyntax* node)
    {
        if (!node) return "";
        AstDebugFormatter formatter;
        node->accept(&formatter);
        return formatter.out.str() + "\n";
    }
};

}
