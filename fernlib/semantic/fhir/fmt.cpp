#include "fmt.hpp"
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

#pragma region Helpers

void FhirFormatter::write_indent()
{
    for (int i = 0; i < indent; ++i)
        out << "    ";
}

void FhirFormatter::write_child(FhirNode* node)
{
    if (node) node->accept(this);
    else out << "null";
}

void FhirFormatter::write_args(const std::vector<FhirExpr*>& args)
{
    out << "(";
    for (size_t i = 0; i < args.size(); ++i)
    {
        if (i > 0) out << ", ";
        write_child(args[i]);
    }
    out << ")";
}

std::string FhirFormatter::method_label(MethodSymbol* method)
{
    if (!method) return "?";
    auto* parent = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
    if (parent) return format_type_name(parent) + "." + method->name;
    return method->name;
}

#pragma region Expression Visitors

void FhirFormatter::visit(FhirLiteralExpr* node)
{
    out << node->value.format();
}

void FhirFormatter::visit(FhirLocalRefExpr* node)
{
    out << (node->local ? node->local->name : "?");
}

void FhirFormatter::visit(FhirParamRefExpr* node)
{
    out << (node->parameter ? node->parameter->name : "?");
}

void FhirFormatter::visit(FhirFieldAccessExpr* node)
{
    write_child(node->object);
    out << "." << (node->field ? node->field->name : "?");
}

void FhirFormatter::visit(FhirThisExpr* node)
{
    out << "this";
}

void FhirFormatter::visit(FhirIntrinsicExpr* node)
{
    if (node->args.size() == 2)
    {
        out << "(";
        write_child(node->args[0]);
        out << " " << op_symbol(node->op) << " ";
        write_child(node->args[1]);
        out << ")";
    }
    else if (node->args.size() == 1)
    {
        out << "(" << op_symbol(node->op);
        write_child(node->args[0]);
        out << ")";
    }
}

void FhirFormatter::visit(FhirCallExpr* node)
{
    out << method_label(node->target);
    write_args(node->arguments);
}

void FhirFormatter::visit(FhirMethodCallExpr* node)
{
    write_child(node->receiver);
    out << "." << (node->method ? node->method->name : "?");
    write_args(node->arguments);
}

void FhirFormatter::visit(FhirObjectCreateExpr* node)
{
    auto* ctor = node->constructor;
    auto* parent = ctor && ctor->parent ? ctor->parent->as<NamedTypeSymbol>() : nullptr;
    out << "new " << (parent ? format_type_name(parent) : "?");
    write_args(node->arguments);
}

void FhirFormatter::visit(FhirAssignExpr* node)
{
    write_child(node->target);
    out << " = ";
    write_child(node->value);
}

void FhirFormatter::visit(FhirCastExpr* node)
{
    out << "(";
    write_child(node->operand);
    out << " as " << (node->type ? format_type_name(node->type) : "?") << ")";
}

void FhirFormatter::visit(FhirErrorExpr* node)
{
    out << "<error>";
}

void FhirFormatter::visit(FhirBlock* node)
{
    out << "\n";
    write_indent();
    out << "{\n";
    ++indent;
    for (auto* stmt : node->statements)
    {
        write_indent();
        write_child(stmt);
        out << "\n";
    }
    --indent;
    write_indent();
    out << "}";
}

#pragma region Statement Visitors

void FhirFormatter::visit(FhirVarDeclStmt* node)
{
    out << "var " << (node->local ? node->local->name : "?");
    if (node->local && node->local->type)
        out << ": " << format_type_name(node->local->type);
    if (node->initializer)
    {
        out << " = ";
        write_child(node->initializer);
    }
}

void FhirFormatter::visit(FhirExprStmt* node)
{
    write_child(node->expression);
}

void FhirFormatter::visit(FhirReturnStmt* node)
{
    out << "return ";
    write_child(node->value);
}

void FhirFormatter::visit(FhirIfStmt* node)
{
    out << "if ";
    write_child(node->condition);
    if (node->thenBlock)
    {
        write_child(node->thenBlock);
    }
    if (node->elseIf)
    {
        out << "\n";
        write_indent();
        out << "else ";
        write_child(node->elseIf);
    }
    else if (node->elseBlock)
    {
        out << "\n";
        write_indent();
        out << "else";
        write_child(node->elseBlock);
    }
}

void FhirFormatter::visit(FhirWhileStmt* node)
{
    out << "while ";
    write_child(node->condition);
    if (node->body)
    {
        write_child(node->body);
    }
}

#pragma region Public

std::string FhirFormatter::format(FhirMethod* method)
{
    if (!method) return "";
    FhirFormatter fmt;

    fmt.out << "fn " << fmt.method_label(method->symbol);
    if (method->symbol)
    {
        fmt.out << "(";
        for (size_t i = 0; i < method->symbol->parameters.size(); ++i)
        {
            if (i > 0) fmt.out << ", ";
            auto* param = method->symbol->parameters[i];
            fmt.out << param->name << ": " << format_type_name(param->type);
        }
        fmt.out << ")";
        if (method->symbol->get_return_type())
            fmt.out << " -> " << format_type_name(method->symbol->get_return_type());
    }

    if (method->body)
    {
        method->body->accept(&fmt);
    }

    return fmt.out.str() + "\n";
}

}
