#include "fmt.hpp"
#include <semantic/symbol/fmt.hpp>
#include <semantic/symbol/symbol.hpp>

#include <format>

namespace Fern
{

#pragma region Pretty Helpers

void FlirPrettyFormatter::write_indent()
{
    for (int i = 0; i < indent; ++i)
        out << "    ";
}

void FlirPrettyFormatter::write_child(FlirNode* node)
{
    if (node) node->accept(this);
    else out << "null";
}

void FlirPrettyFormatter::write_child(FlirExpr* node)
{
    if (!node) { out << "null"; return; }
    node->accept(this);
}

void FlirPrettyFormatter::write_args(const std::vector<FlirExpr*>& args)
{
    out << "(";
    for (size_t i = 0; i < args.size(); ++i)
    {
        if (i > 0) out << ", ";
        write_child(args[i]);
    }
    out << ")";
}

std::string FlirPrettyFormatter::method_label(MethodSymbol* method)
{
    if (!method) return "?";
    auto* parent = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
    if (parent) return std::format("{}.{}", format_type(parent), method->name);
    return method->name;
}

std::string FlirPrettyFormatter::local_label(FlirLocal* local)
{
    if (!local) return "?";
    if (local->name.empty()) return std::format("${}", local->index);
    return std::string(local->name);
}

#pragma region Pretty Expression Visitors

void FlirPrettyFormatter::visit(FlirConst* node)
{
    out << node->value.format();
}

void FlirPrettyFormatter::visit(FlirLoadLocal* node)
{
    out << local_label(node->local);
}

void FlirPrettyFormatter::visit(FlirLoadField* node)
{
    write_child(node->base);
    out << "." << (node->field ? node->field->name : "?");
}

void FlirPrettyFormatter::visit(FlirCall* node)
{
    if (node->thisArg)
    {
        write_child(node->thisArg);
        out << "." << (node->method ? node->method->name : "?");
    }
    else
    {
        out << method_label(node->method);
    }
    write_args(node->args);
}

void FlirPrettyFormatter::visit(FlirIntrinsic* node)
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

void FlirPrettyFormatter::visit(FlirCast* node)
{
    out << "(";
    write_child(node->operand);
    out << " as " << (node->targetType ? format_type(node->targetType) : "?") << ")";
}

void FlirPrettyFormatter::visit(FlirAlloc* node)
{
    out << "alloc " << (node->allocType ? format_type(node->allocType) : "?");
}

void FlirPrettyFormatter::visit(FlirSequence* node)
{
    out << "seq {\n";
    ++indent;
    for (auto* stmt : node->sideEffects)
    {
        write_indent();
        write_child(stmt);
        out << "\n";
    }
    write_indent();
    out << "yield ";
    write_child(node->value);
    out << "\n";
    --indent;
    write_indent();
    out << "}";
}

#pragma region Pretty Statement Visitors

void FlirPrettyFormatter::visit(FlirBlock* node)
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

void FlirPrettyFormatter::visit(FlirAssign* node)
{
    write_child(node->target);
    out << " = ";
    write_child(node->value);
}

void FlirPrettyFormatter::visit(FlirExprStmt* node)
{
    write_child(node->expression);
}

void FlirPrettyFormatter::visit(FlirIf* node)
{
    out << "if ";
    write_child(node->condition);
    if (node->thenBlock)
    {
        write_child(node->thenBlock);
    }
    if (node->elseBlock)
    {
        out << "\n";
        write_indent();
        out << "else";
        write_child(node->elseBlock);
    }
}

void FlirPrettyFormatter::visit(FlirLoop* node)
{
    out << "loop";
    if (node->body) write_child(node->body);
}

void FlirPrettyFormatter::visit(FlirBreak*)
{
    out << "break";
}

void FlirPrettyFormatter::visit(FlirReturn* node)
{
    out << "return";
    if (node->value)
    {
        out << " ";
        write_child(node->value);
    }
}

#pragma region Pretty Public

std::string FlirPrettyFormatter::format(FlirMethod* method)
{
    if (!method) return "";
    FlirPrettyFormatter fmt;

    fmt.out << "fn " << fmt.method_label(method->symbol);
    if (method->symbol)
    {
        fmt.out << "(";
        for (size_t i = 0; i < method->symbol->parameters.size(); ++i)
        {
            if (i > 0) fmt.out << ", ";
            auto* param = method->symbol->parameters[i];
            fmt.out << param->name << ": " << format_type(param->type);
        }
        fmt.out << ")";
        if (method->symbol->get_return_type())
            fmt.out << " -> " << format_type(method->symbol->get_return_type());
    }

    if (!method->locals.empty())
    {
        fmt.out << "\n";
        fmt.write_indent();
        fmt.out << "locals:";
        for (auto* local : method->locals)
        {
            fmt.out << "\n    " << fmt.local_label(local) << ": "
                    << (local && local->type ? format_type(local->type) : "?");
        }
    }

    if (method->body)
    {
        method->body->accept(&fmt);
    }

    return std::format("{}\n", fmt.out.str());
}

std::string FlirPrettyFormatter::format(FlirNode* node)
{
    if (!node) return "";
    FlirPrettyFormatter fmt;
    node->accept(&fmt);
    return fmt.out.str();
}

#pragma region Debug Helpers

void FlirDebugFormatter::write_indent()
{
    if (!suppressNextIndent)
        for (int i = 0; i < indent; ++i)
            out << "  ";
    suppressNextIndent = false;
}

void FlirDebugFormatter::open_block()
{
    out << "\n";
    write_indent();
    out << "{\n";
    ++indent;
}

void FlirDebugFormatter::close_block()
{
    --indent;
    write_indent();
    out << "}";
}

void FlirDebugFormatter::begin_node(FlirNode* node)
{
    write_indent();
    out << node->node_name() << " (span: " << node->span.format() << ")";
}

void FlirDebugFormatter::begin_node(FlirNode* node, std::string_view extra)
{
    write_indent();
    out << node->node_name() << " (" << extra << ", span: " << node->span.format() << ")";
}

void FlirDebugFormatter::write_child(std::string_view name, FlirNode* node, bool addComma)
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

void FlirDebugFormatter::write_field(std::string_view name, std::string_view value, bool addComma)
{
    write_indent();
    out << name << ": " << value;
    if (addComma) out << ",";
    out << "\n";
}

std::string FlirDebugFormatter::type_attr(FlirExpr* expr)
{
    return std::format("type: {}", expr && expr->type ? format_type(expr->type) : "?");
}

std::string FlirDebugFormatter::symbol_label(Symbol* sym)
{
    if (!sym) return "null";
    return sym->qualified_name();
}

std::string FlirDebugFormatter::method_label(MethodSymbol* method)
{
    if (!method) return "null";
    auto* ret = method->get_return_type();
    return std::format("{}({}) -> {}",
                       symbol_label(method),
                       format_parameter_list(method),
                       ret ? format_type(ret) : "void");
}

std::string FlirDebugFormatter::local_label(FlirLocal* local)
{
    if (!local) return "null";
    std::string name = local->name.empty() ? std::format("$l{}", local->index) : std::string(local->name);
    return std::format("\"{}\": {}", name, local->type ? format_type(local->type) : "?");
}

#pragma region Debug Expression Visitors

void FlirDebugFormatter::visit(FlirConst* node)
{
    begin_node(node, std::format("value: {}, {}", node->value.format(), type_attr(node)));
}

void FlirDebugFormatter::visit(FlirLoadLocal* node)
{
    begin_node(node, std::format("local: {}, {}", local_label(node->local), type_attr(node)));
}

void FlirDebugFormatter::visit(FlirLoadField* node)
{
    std::string field = node->field
        ? std::format("\"{}\": {}", symbol_label(node->field), format_type(node->field->type))
        : std::string("null");
    begin_node(node, std::format("field: {}, {}", field, type_attr(node)));
    open_block();
    write_child("base", node->base);
    close_block();
}

void FlirDebugFormatter::visit(FlirCall* node)
{
    begin_node(node, std::format("method: {}, {}", method_label(node->method), type_attr(node)));
    open_block();
    write_child("thisArg", node->thisArg, true);
    write_children("args", node->args);
    close_block();
}

void FlirDebugFormatter::visit(FlirIntrinsic* node)
{
    begin_node(node, std::format("op: {}, {}", Fern::format(node->op), type_attr(node)));
    open_block();
    write_children("args", node->args);
    close_block();
}

void FlirDebugFormatter::visit(FlirCast* node)
{
    std::string target = node->targetType ? format_type(node->targetType) : "?";
    begin_node(node, std::format("target: {}, {}", target, type_attr(node)));
    open_block();
    write_child("operand", node->operand);
    close_block();
}

void FlirDebugFormatter::visit(FlirAlloc* node)
{
    std::string target = node->allocType ? format_type(node->allocType) : "?";
    begin_node(node, std::format("allocType: {}, {}", target, type_attr(node)));
}

void FlirDebugFormatter::visit(FlirSequence* node)
{
    begin_node(node, type_attr(node));
    open_block();
    write_children("sideEffects", node->sideEffects, true);
    write_child("value", node->value);
    close_block();
}

#pragma region Debug Statement Visitors

void FlirDebugFormatter::visit(FlirBlock* node)
{
    begin_node(node);
    open_block();
    write_children("statements", node->statements);
    close_block();
}

void FlirDebugFormatter::visit(FlirAssign* node)
{
    begin_node(node);
    open_block();
    write_child("target", node->target, true);
    write_child("value", node->value);
    close_block();
}

void FlirDebugFormatter::visit(FlirExprStmt* node)
{
    begin_node(node);
    open_block();
    write_child("expression", node->expression);
    close_block();
}

void FlirDebugFormatter::visit(FlirIf* node)
{
    begin_node(node);
    open_block();
    write_child("condition", node->condition, true);
    write_child("thenBlock", node->thenBlock, true);
    write_child("elseBlock", node->elseBlock);
    close_block();
}

void FlirDebugFormatter::visit(FlirLoop* node)
{
    begin_node(node);
    open_block();
    write_child("body", node->body);
    close_block();
}

void FlirDebugFormatter::visit(FlirBreak* node)
{
    begin_node(node);
}

void FlirDebugFormatter::visit(FlirReturn* node)
{
    begin_node(node);
    open_block();
    write_child("value", node->value);
    close_block();
}

#pragma region Debug Public

std::string FlirDebugFormatter::format(FlirMethod* method)
{
    if (!method) return "";
    FlirDebugFormatter fmt;

    fmt.out << "FlirMethod (signature: " << fmt.method_label(method->symbol) << ")";
    fmt.open_block();
    fmt.write_indent();
    fmt.out << "locals: [";
    if (!method->locals.empty())
    {
        fmt.out << "\n";
        ++fmt.indent;
        for (auto* local : method->locals)
        {
            fmt.write_indent();
            fmt.out << fmt.local_label(local) << "\n";
        }
        --fmt.indent;
        fmt.write_indent();
    }
    fmt.out << "],\n";
    fmt.write_child("body", method->body);
    fmt.close_block();

    return std::format("{}\n", fmt.out.str());
}

std::string FlirDebugFormatter::format(FlirNode* node)
{
    if (!node) return "";
    FlirDebugFormatter fmt;
    node->accept(&fmt);
    return std::format("{}\n", fmt.out.str());
}

}
