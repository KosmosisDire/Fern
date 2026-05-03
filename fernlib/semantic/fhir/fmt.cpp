#include "fmt.hpp"
#include <semantic/symbol/symbol.hpp>

#include <format>

namespace Fern
{

#pragma region Pretty Helpers

void FhirPrettyFormatter::write_indent()
{
    for (int i = 0; i < indent; ++i)
        out << "    ";
}

void FhirPrettyFormatter::write_child(FhirNode* node)
{
    if (node) node->accept(this);
    else out << "null";
}

void FhirPrettyFormatter::write_child(FhirExpr* node)
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

void FhirPrettyFormatter::write_args(const std::vector<FhirExpr*>& args)
{
    out << "(";
    for (size_t i = 0; i < args.size(); ++i)
    {
        if (i > 0) out << ", ";
        write_child(args[i]);
    }
    out << ")";
}

std::string FhirPrettyFormatter::method_label(MethodSymbol* method)
{
    if (!method) return "?";
    auto* parent = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
    if (parent) return std::format("{}.{}", format_type_name(parent), method->name);
    return method->name;
}

#pragma region Pretty Expression Visitors

void FhirPrettyFormatter::visit(FhirLiteralExpr* node)
{
    out << node->value.format();
}

void FhirPrettyFormatter::visit(FhirLocalRefExpr* node)
{
    out << (node->local ? node->local->name : "?");
}

void FhirPrettyFormatter::visit(FhirParamRefExpr* node)
{
    out << (node->parameter ? node->parameter->name : "?");
}

void FhirPrettyFormatter::visit(FhirFieldRefExpr* node)
{
    write_child(node->thisRef);
    out << "." << (node->field ? node->field->name : "?");
}

void FhirPrettyFormatter::visit(FhirThisExpr* node)
{
    out << "this";
}

void FhirPrettyFormatter::visit(FhirIntrinsicExpr* node)
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

void FhirPrettyFormatter::visit(FhirCallExpr* node)
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

void FhirPrettyFormatter::visit(FhirConstructionExpr* node)
{
    auto* ctor = node->call && node->call->callee ? node->call->callee->method : nullptr;
    auto* parent = ctor && ctor->parent ? ctor->parent->as<NamedTypeSymbol>() : nullptr;
    out << "new " << (parent ? format_type_name(parent) : "?");
    if (node->call)
        write_args(node->call->arguments);
    else
        out << "()";
}

void FhirPrettyFormatter::visit(FhirAssignExpr* node)
{
    write_child(node->target);
    out << " = ";
    write_child(node->value);
}

void FhirPrettyFormatter::visit(FhirCastExpr* node)
{
    out << "(";
    write_child(node->operand);
    out << " as " << (node->type ? format_type_name(node->type) : "?") << ")";
}

void FhirPrettyFormatter::visit(FhirErrorExpr* node)
{
    out << "ERROR(";
    if (node->inner) write_child(node->inner);
    out << ")";
}

void FhirPrettyFormatter::visit(FhirNamespaceRefExpr* node)
{
    out << "&" << (node->namespaceSymbol ? node->namespaceSymbol->qualified_name() : "?");
}

void FhirPrettyFormatter::visit(FhirMethodGroupRefExpr* node)
{
    if (node->thisRef)
    {
        write_child(node->thisRef);
        out << ".";
    }
    out << "&" << (node->enclosingScope ? std::format("{}.", node->enclosingScope->qualified_name()) : "")
        << std::string(node->name) << "(?)";
}

void FhirPrettyFormatter::visit(FhirMethodRefExpr* node)
{
    if (node->thisRef)
    {
        write_child(node->thisRef);
        out << ".";
    }
    out << "&" << (node->method ? node->method->name : "?");
}

void FhirPrettyFormatter::visit(FhirTypeRef* node)
{
    out << "&" << (node->referenced ? format_type_name(node->referenced) : "?");
}

void FhirPrettyFormatter::visit(FhirBlock* node)
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

#pragma region Pretty Statement Visitors

void FhirPrettyFormatter::visit(FhirVarDeclStmt* node)
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

void FhirPrettyFormatter::visit(FhirExprStmt* node)
{
    write_child(node->expression);
}

void FhirPrettyFormatter::visit(FhirReturnStmt* node)
{
    out << "return ";
    write_child(node->value);
}

void FhirPrettyFormatter::visit(FhirIfStmt* node)
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

void FhirPrettyFormatter::visit(FhirWhileStmt* node)
{
    out << "while ";
    write_child(node->condition);
    if (node->body)
    {
        write_child(node->body);
    }
}

#pragma region Pretty Public

std::string FhirPrettyFormatter::format(FhirMethod* method)
{
    if (!method) return "";
    FhirPrettyFormatter fmt;

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

std::string FhirPrettyFormatter::format(FhirNode* node)
{
    if (!node) return "";
    FhirPrettyFormatter fmt;
    node->accept(&fmt);
    return fmt.out.str();
}

#pragma region Debug Helpers

void FhirDebugFormatter::write_indent()
{
    if (!suppressNextIndent)
        for (int i = 0; i < indent; ++i)
            out << "  ";
    suppressNextIndent = false;
}

void FhirDebugFormatter::open_block()
{
    out << "\n";
    write_indent();
    out << "{\n";
    ++indent;
}

void FhirDebugFormatter::close_block()
{
    --indent;
    write_indent();
    out << "}";
}

void FhirDebugFormatter::begin_node(FhirNode* node)
{
    write_indent();
    out << node->node_name() << " (span: " << node->span.format() << ")";
}

void FhirDebugFormatter::begin_node(FhirNode* node, std::string_view extra)
{
    write_indent();
    out << node->node_name() << " (" << extra << ", span: " << node->span.format() << ")";
}

void FhirDebugFormatter::write_child(std::string_view name, FhirNode* node, bool addComma)
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

void FhirDebugFormatter::write_field(std::string_view name, std::string_view value, bool addComma)
{
    write_indent();
    out << name << ": " << value;
    if (addComma) out << ",";
    out << "\n";
}

std::string FhirDebugFormatter::type_attr(FhirExpr* expr)
{
    return std::format("type: {}", expr && expr->type ? format_type_name(expr->type) : "?");
}

std::string FhirDebugFormatter::symbol_label(Symbol* sym)
{
    if (!sym) return "null";
    return sym->qualified_name();
}

std::string FhirDebugFormatter::method_label(MethodSymbol* method)
{
    if (!method) return "null";
    auto* ret = method->get_return_type();
    return std::format("{}({}) -> {}",
                       symbol_label(method),
                       method->format_parameters(),
                       ret ? format_type_name(ret) : "void");
}

#pragma region Debug Expression Visitors

void FhirDebugFormatter::visit(FhirLiteralExpr* node)
{
    begin_node(node, std::format("value: {}, {}", node->value.format(), type_attr(node)));
}

void FhirDebugFormatter::visit(FhirLocalRefExpr* node)
{
    std::string local = node->local
        ? std::format("\"{}\": {}", node->local->name, format_type_name(node->local->type))
        : std::string("null");
    begin_node(node, std::format("local: {}, {}", local, type_attr(node)));
}

void FhirDebugFormatter::visit(FhirParamRefExpr* node)
{
    std::string param = node->parameter
        ? std::format("\"{}\": {}", node->parameter->name, format_type_name(node->parameter->type))
        : std::string("null");
    begin_node(node, std::format("param: {}, {}", param, type_attr(node)));
}

void FhirDebugFormatter::visit(FhirFieldRefExpr* node)
{
    std::string field = node->field
        ? std::format("\"{}\": {}", symbol_label(node->field), format_type_name(node->field->type))
        : std::string("null");
    begin_node(node, std::format("field: {}, {}", field, type_attr(node)));
    open_block();
    write_child("thisRef", node->thisRef);
    close_block();
}

void FhirDebugFormatter::visit(FhirThisExpr* node)
{
    begin_node(node, type_attr(node));
}

void FhirDebugFormatter::visit(FhirIntrinsicExpr* node)
{
    std::string method = node->method ? std::format(", method: {}", method_label(node->method)) : "";
    begin_node(node, std::format("op: {}{}, {}", Fern::format(node->op), method, type_attr(node)));
    open_block();
    write_children("args", node->args);
    close_block();
}

void FhirDebugFormatter::visit(FhirCallExpr* node)
{
    begin_node(node, type_attr(node));
    open_block();
    write_child("callee", node->callee, true);
    write_children("arguments", node->arguments);
    close_block();
}

void FhirDebugFormatter::visit(FhirConstructionExpr* node)
{
    begin_node(node, type_attr(node));
    open_block();
    write_child("typeRef", node->typeRef, true);
    write_child("call", node->call);
    close_block();
}

void FhirDebugFormatter::visit(FhirAssignExpr* node)
{
    begin_node(node, type_attr(node));
    open_block();
    write_child("target", node->target, true);
    write_child("value", node->value);
    close_block();
}

void FhirDebugFormatter::visit(FhirCastExpr* node)
{
    std::string method = node->method ? std::format(", method: {}", method_label(node->method)) : "";
    begin_node(node, std::format("implicit: {}{}, {}",
                                  node->isImplicit ? "true" : "false",
                                  method,
                                  type_attr(node)));
    open_block();
    write_child("operand", node->operand, true);
    write_child("typeRef", node->typeRef);
    close_block();
}

void FhirDebugFormatter::visit(FhirErrorExpr* node)
{
    begin_node(node, type_attr(node));
    open_block();
    write_child("inner", node->inner);
    close_block();
}

void FhirDebugFormatter::visit(FhirNamespaceRefExpr* node)
{
    std::string ns = node->namespaceSymbol ? std::format("\"{}\"", node->namespaceSymbol->qualified_name()) : std::string("null");
    begin_node(node, std::format("namespace: {}", ns));
}

void FhirDebugFormatter::visit(FhirMethodGroupRefExpr* node)
{
    std::string scope = node->enclosingScope ? std::format("\"{}\"", node->enclosingScope->qualified_name()) : std::string("null");
    begin_node(node, std::format("name: \"{}\", scope: {}, {}", node->name, scope, type_attr(node)));
    open_block();
    write_child("thisRef", node->thisRef);
    close_block();
}

void FhirDebugFormatter::visit(FhirMethodRefExpr* node)
{
    begin_node(node, std::format("method: {}, {}", method_label(node->method), type_attr(node)));
    open_block();
    write_child("thisRef", node->thisRef);
    close_block();
}

void FhirDebugFormatter::visit(FhirTypeRef* node)
{
    std::string ref = node->referenced ? format_type_name(node->referenced) : "?";
    begin_node(node, std::format("referenced: {}, {}", ref, type_attr(node)));
    open_block();
    write_children("args", node->args);
    close_block();
}

void FhirDebugFormatter::visit(FhirBlock* node)
{
    begin_node(node);
    open_block();
    write_children("statements", node->statements);
    close_block();
}

#pragma region Debug Statement Visitors

void FhirDebugFormatter::visit(FhirVarDeclStmt* node)
{
    std::string local = node->local
        ? std::format("\"{}\": {}", node->local->name, format_type_name(node->local->type))
        : std::string("null");
    begin_node(node, std::format("local: {}", local));
    open_block();
    write_child("typeRef", node->typeRef, true);
    write_child("initializer", node->initializer);
    close_block();
}

void FhirDebugFormatter::visit(FhirExprStmt* node)
{
    begin_node(node);
    open_block();
    write_child("expression", node->expression);
    close_block();
}

void FhirDebugFormatter::visit(FhirReturnStmt* node)
{
    begin_node(node);
    open_block();
    write_child("value", node->value);
    close_block();
}

void FhirDebugFormatter::visit(FhirIfStmt* node)
{
    begin_node(node);
    open_block();
    write_child("condition", node->condition, true);
    write_child("thenBlock", node->thenBlock, true);
    write_child("elseIf", node->elseIf, true);
    write_child("elseBlock", node->elseBlock);
    close_block();
}

void FhirDebugFormatter::visit(FhirWhileStmt* node)
{
    begin_node(node);
    open_block();
    write_child("condition", node->condition, true);
    write_child("body", node->body);
    close_block();
}

#pragma region Debug Public

std::string FhirDebugFormatter::format(FhirMethod* method)
{
    if (!method) return "";
    FhirDebugFormatter fmt;

    fmt.out << "FhirMethod (signature: " << fmt.method_label(method->symbol) << ")";
    fmt.open_block();
    fmt.write_child("body", method->body);
    fmt.close_block();

    return std::format("{}\n", fmt.out.str());
}

std::string FhirDebugFormatter::format(FhirNode* node)
{
    if (!node) return "";
    FhirDebugFormatter fmt;
    node->accept(&fmt);
    return std::format("{}\n", fmt.out.str());
}

}
