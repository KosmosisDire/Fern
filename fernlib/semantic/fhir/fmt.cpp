#include "fmt.hpp"
#include <semantic/symbol/symbol.hpp>

#include <format>

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

void FhirFormatter::write_child(FhirExpr* node)
{
    if (!node) { out << "null"; return; }

    const auto& c = node->get_constant();
    if (c)
    {
        out << "(";
        node->accept(this);
        out << ")=" << c->format();
    }
    else
    {
        node->accept(this);
    }
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
    if (parent) return std::format("{}.{}", format_type_name(parent), method->name);
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

void FhirFormatter::visit(FhirFieldRefExpr* node)
{
    write_child(node->thisRef);
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
    auto* callee = node->callee;
    auto* method = callee ? callee->method : nullptr;
    if (callee && callee->thisRef)
    {
        write_child(callee->thisRef);
        out << "." << (method ? method->name : "?");
    }
    else
    {
        out << method_label(method);
    }
    write_args(node->arguments);
}

void FhirFormatter::visit(FhirConstructionExpr* node)
{
    auto* ctor = node->call && node->call->callee ? node->call->callee->method : nullptr;
    auto* parent = ctor && ctor->parent ? ctor->parent->as<NamedTypeSymbol>() : nullptr;
    out << "new " << (parent ? format_type_name(parent) : "?");
    if (node->call)
        write_args(node->call->arguments);
    else
        out << "()";
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
    out << "ERROR(";
    if (node->inner) write_child(node->inner);
    out << ")";
}

void FhirFormatter::visit(FhirNamespaceRefExpr* node)
{
    out << "&" << (node->namespaceSymbol ? node->namespaceSymbol->qualified_name() : "?");
}

void FhirFormatter::visit(FhirMethodGroupRefExpr* node)
{
    if (node->thisRef)
    {
        write_child(node->thisRef);
        out << ".";
    }
    out << "&" << (node->enclosingScope ? std::format("{}.", node->enclosingScope->qualified_name()) : "")
        << std::string(node->name) << "(?)";
}

void FhirFormatter::visit(FhirMethodRefExpr* node)
{
    if (node->thisRef)
    {
        write_child(node->thisRef);
        out << ".";
    }
    out << "&" << (node->method ? node->method->name : "?");
}

void FhirFormatter::visit(FhirTypeRef* node)
{
    out << "&" << (node->referenced ? format_type_name(node->referenced) : "?");
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

    return std::format("{}\n", fmt.out.str());
}

}
