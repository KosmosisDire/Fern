#pragma once

#include <ast/ast.hpp>
#include <semantic/binder/binding.hpp>
#include <symbol/symbol.hpp>
#include <sstream>

namespace Fern
{

class AnnotatedAstFormatter : public DefaultAstVisitor
{
    const AstBinding& info;
    std::ostringstream out;
    int indent = 0;
    bool suppressNextIndent = false;

#pragma region Helpers

    void write_indent()
    {
        if (!suppressNextIndent)
            for (int i = 0; i < indent; ++i)
            out << "  ";
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

    void write_modifiers(Modifier mods)
    {
        if (mods != Modifier::None)
        {
            out << "[" << Fern::format(mods) << "] ";
        }
    }

    void write_attributes(const std::vector<AttributeSyntax*>& attrs)
    {
        for (auto* attr : attrs)
        {
            out << "@";
            if (attr->value)
            {
                suppressNextIndent = true;
                attr->value->accept(this);
            }
            out << "\n";
            write_indent();
        }
    }

    void write_annotation(BaseExprSyntax* expr)
    {
        TypeSymbol* type = info.get_type(expr);
        if (type)
        {
            out << " : " << type->name;
        }

        Symbol* sym = info.get_symbol(expr);
        if (sym)
        {
            out << " -> " << format_symbol_ref(sym);
        }
    }

    static std::string format_symbol_ref(Symbol* sym)
    {
        std::string qname = sym->qualified_name();
        switch (sym->kind)
        {
            case SymbolKind::Namespace: return "Namespace(" + qname + ")";
            case SymbolKind::Type: return "Type(" + qname + ")";
            case SymbolKind::Field: return "Field(" + qname + ")";
            case SymbolKind::Method: return "Method(" + qname + ")";
            case SymbolKind::Parameter: return "Param(" + qname + ")";
            case SymbolKind::Local: return "Local(" + qname + ")";
        }
        return "?(" + qname + ")";
    }

#pragma region Expression Visitors

public:
    void visit(IdentifierExprSyntax* node) override
    {
        write_indent();
        out << "IdentifierExpr \"" << node->name.lexeme << "\"";
        write_annotation(node);
    }

    void visit(LiteralExprSyntax* node) override
    {
        write_indent();
        out << "LiteralExpr " << node->token.lexeme;
        write_annotation(node);
    }

    void visit(ParenExprSyntax* node) override
    {
        write_indent();
        out << "ParenExpr";
        write_annotation(node);
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
        write_indent();
        out << "BlockExpr";
        write_annotation(node);
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
        write_indent();
        out << "CallExpr";
        write_annotation(node);
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

    void visit(InitializerExprSyntax* node) override
    {
        write_indent();
        out << "InitializerExpr";
        write_annotation(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("target", node->target, true);
        write_children("initializers", node->initializers);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(UnaryExprSyntax* node) override
    {
        write_indent();
        out << "UnaryExpr (op: " << Fern::format(node->op) << ")";
        write_annotation(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("operand", node->operand);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(BinaryExprSyntax* node) override
    {
        write_indent();
        out << "BinaryExpr (op: " << Fern::format(node->op) << ")";
        write_annotation(node);
        out << "\n";
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
        write_indent();
        out << "AssignmentExpr (op: " << Fern::format(node->op) << ")";
        write_annotation(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("target", node->target, true);
        write_child("value", node->value);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(MemberAccessExprSyntax* node) override
    {
        write_indent();
        out << "MemberAccessExpr \"" << node->right.lexeme << "\"";
        write_annotation(node);
        out << "\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("left", node->left);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(ThisExprSyntax* node) override
    {
        write_indent();
        out << "ThisExpr";
        write_annotation(node);
    }

    void visit(TypeExprSyntax* node) override
    {
        write_indent();
        out << "TypeExpr \"" << node->name.lexeme << "\"";
        write_annotation(node);
    }

#pragma region Statement Visitors

    void visit(ReturnStmtSyntax* node) override
    {
        write_indent();
        out << "ReturnStmt\n";
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
        write_indent();
        out << "ExpressionStmt\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("expression", node->expression);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(IfStmtSyntax* node) override
    {
        write_indent();
        out << "IfStmt\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("condition", node->condition, true);
        write_child("then", node->thenBody, true);
        write_child("elseIf", node->elseIf, true);
        write_child("else", node->elseBlock);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(WhileStmtSyntax* node) override
    {
        write_indent();
        out << "WhileStmt\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("condition", node->condition, true);
        write_child("body", node->body);
        --indent;
        write_indent();
        out << "}";
    }

#pragma region Declaration Visitors

    void visit(ParameterDeclSyntax* node) override
    {
        write_indent();
        out << "ParameterDecl \"" << node->name.lexeme << "\"\n";
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
        write_indent();
        write_attributes(node->attributes);
        write_modifiers(node->modifiers);
        out << "VariableDecl \"" << node->name.lexeme << "\"\n";
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
        write_indent();
        write_attributes(node->attributes);
        write_modifiers(node->modifiers);
        out << "FunctionDecl \"" << node->name.lexeme << "\"\n";
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

    void visit(OperatorDeclSyntax* node) override
    {
        write_indent();
        write_attributes(node->attributes);
        write_modifiers(node->modifiers);
        out << "OperatorDecl (op: " << Fern::format(node->op.kind) << ")\n";
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

    void visit(InitDeclSyntax* node) override
    {
        write_indent();
        write_attributes(node->attributes);
        write_modifiers(node->modifiers);
        out << "InitDecl\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_children("parameters", node->parameters, true);
        write_child("body", node->body);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(TypeDeclSyntax* node) override
    {
        write_indent();
        write_attributes(node->attributes);
        write_modifiers(node->modifiers);
        out << "TypeDecl \"" << node->name.lexeme << "\"\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_children("declarations", node->declarations);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(FieldDeclSyntax* node) override
    {
        write_indent();
        write_attributes(node->attributes);
        write_modifiers(node->modifiers);
        out << "FieldDecl \"" << node->name.lexeme << "\"\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("type", node->type);
        write_child("initializer", node->initializer);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(FieldInitSyntax* node) override
    {
        write_indent();
        out << "FieldInit\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_child("target", node->target);
        write_child("value", node->value);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(NamespaceDeclSyntax* node) override
    {
        write_indent();
        write_attributes(node->attributes);
        write_modifiers(node->modifiers);
        out << "NamespaceDecl \"" << node->name.lexeme << "\"\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_children("declarations", node->declarations);
        --indent;
        write_indent();
        out << "}";
    }

    void visit(RootSyntax* node) override
    {
        write_indent();
        out << "Root\n";
        write_indent();
        out << "{\n";
        ++indent;
        write_children("declarations", node->declarations);
        --indent;
        write_indent();
        out << "}";
    }

#pragma region Public Interface

public:
    static std::string format(BaseSyntax* node, const AstBinding& info)
    {
        if (!node) return "";
        AnnotatedAstFormatter formatter(info);
        node->accept(&formatter);
        return formatter.out.str() + "\n";
    }

private:
    AnnotatedAstFormatter(const AstBinding& info) : info(info) {}
};

}
