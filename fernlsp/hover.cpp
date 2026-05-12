#include "hover.hpp"

#include <ast/ast.hpp>
#include <ast/formatdbg.hpp>
#include <ast/hittest.hpp>
#include <semantic/fhir/fhir.hpp>
#include <semantic/fhir/fmt.hpp>
#include <semantic/fhir/hittest.hpp>
#include <semantic/symbol/fmt.hpp>
#include <semantic/symbol/symbol.hpp>

#include <format>
#include <string>

using namespace Fern;

namespace
{

struct HoverContent
{
    std::string signature;
    std::string description;
};

struct HoverFormatter : FhirVisitor
{
    SymbolFormat fmt = SymbolFormat::hover_long();
    HoverContent content;

    void set_signature(std::string sig)
    {
        content.signature = std::move(sig);
    }

    void set_local_or_type(LocalSymbol* local)
    {
        if (local && local->is_auto_generated()) set_signature(format_type(local->type, fmt));
        else set_signature(format_local(local, fmt));
    }

    void visit(FhirLiteralExpr* n) override
    {
        set_signature(std::format("(literal): {}", format_type(n->type, fmt)));
    }

    void visit(FhirLocalRefExpr* n) override
    {
        set_local_or_type(n->local);
    }

    void visit(FhirParamRefExpr* n) override
    {
        set_signature(format_parameter(n->parameter, fmt));
    }

    void visit(FhirFieldRefExpr* n) override
    {
        set_signature(format_field(n->field, fmt));
    }

    void visit(FhirThisExpr* n) override
    {
        set_signature(std::format("this: {}", format_type(n->type, fmt)));
    }

    void visit(FhirOpExpr* n) override
    {
        if (n->method) set_signature(format_method(n->method, fmt));
        else set_signature(format_type(n->type, fmt));
    }

    void visit(FhirCallExpr* n) override
    {
        if (n->callee && n->callee->method) set_signature(format_method(n->callee->method, fmt));
        else set_signature(std::format("(call) -> {}", format_type(n->type, fmt)));
    }

    void visit(FhirConstructionExpr* n) override
    {
        if (n->call && n->call->callee && n->call->callee->method)
        {
            set_signature(format_method(n->call->callee->method, fmt));
        }
        else
        {
            set_signature(format_type(n->type, fmt));
        }
    }

    void visit(FhirAssignExpr* n) override
    {
        set_signature(std::format("(assign): {}", format_type(n->type, fmt)));
    }

    void visit(FhirCastExpr* n) override
    {
        set_signature(std::format("(cast) -> {}", format_type(n->type, fmt)));
    }

    void visit(FhirErrorExpr* n) override
    {
        if (n->inner)
        {
            HoverFormatter inner;
            inner.fmt = fmt;
            n->inner->accept(&inner);
            content = std::move(inner.content);
        }
        else
        {
            set_signature("(error)");
        }
    }

    void visit(FhirNamespaceRefExpr* n) override
    {
        set_signature(format_namespace(n->namespaceSymbol, fmt));
    }

    void visit(FhirMethodGroupRefExpr* n) override
    {
        set_signature(std::format("(method group) {}", n->name));
    }

    void visit(FhirMethodRefExpr* n) override
    {
        set_signature(format_method(n->method, fmt));
    }

    void visit(FhirTypeRef* n) override
    {
        if (n->referenced)
        {
            set_signature(format_type(n->referenced, fmt));
        }
        else
        {
            set_signature("(type) ?");
        }
    }

    void visit(FhirBlock*) override { set_signature("(block)"); }

    void visit(FhirVarDeclStmt* n) override
    {
        set_local_or_type(n->local);
    }

    void visit(FhirExprStmt*) override { set_signature("(expression statement)"); }
    void visit(FhirReturnStmt*) override { set_signature("(return)"); }
    void visit(FhirIfStmt*) override { set_signature("(if)"); }
    void visit(FhirWhileStmt*) override { set_signature("(while)"); }
};

std::string render_markdown(const HoverContent& content)
{
    std::string description = content.description.empty()
        ? std::string("*Description placeholder*")
        : content.description;
    return std::format("```fern\n{}\n```\n\n---\n\n{}", content.signature, description);
}

}

namespace
{

lsp::Range to_lsp_range(const Span& span)
{
    lsp::Range range;
    range.start.line = span.startLine;
    range.start.character = span.startColumn;
    range.end.line = span.endLine;
    range.end.character = span.endColumn;
    return range;
}

lsp::Hover make_debug_hover(std::string_view header, const std::string& body, const Span& span)
{
    lsp::Hover hover;
    lsp::MarkupContent markup;
    markup.kind = lsp::MarkupKind::Markdown;
    markup.value = std::format("**{}**\n\n```\n{}\n```", header, body);
    hover.contents = std::move(markup);
    hover.range = to_lsp_range(span);
    return hover;
}

const Fern::CompilationUnit* find_unit(const DocumentState& doc)
{
    if (!doc.compilation) return nullptr;
    for (const auto& unit : doc.compilation->get_units())
    {
        if (unit && unit->sourceFile && unit->sourceFile->file_id() == doc.fileId)
        {
            return unit.get();
        }
    }
    return nullptr;
}

}

std::optional<lsp::Hover> compute_hover(
    const DocumentState& doc,
    uint32_t line,
    uint32_t col)
{
    if (!doc.compilation) return std::nullopt;

    for (auto* method : doc.compilation->semantic().methods)
    {
        if (!method || !method->body) continue;
        if (method->body->span.fileId != doc.fileId) continue;
        if (!method->body->span.contains(line, col)) continue;

        FhirHitResult hit = FhirHitTest::find(method->body, line, col);
        FhirNode* node = hit.node;
        if (!node) node = method->body;

        HoverFormatter formatter;
        if (auto* ns = hit.nameTarget ? hit.nameTarget->as<NamespaceSymbol>() : nullptr)
        {
            formatter.set_signature(format_namespace(ns, formatter.fmt));
        }
        else if (auto* type = hit.nameTarget ? hit.nameTarget->as<NamedTypeSymbol>() : nullptr)
        {
            formatter.set_signature(format_type(type, formatter.fmt));
        }
        else
        {
            node->accept(&formatter);
        }
        if (formatter.content.signature.empty()) return std::nullopt;

        lsp::Hover hover;
        lsp::MarkupContent markup;
        markup.kind = lsp::MarkupKind::Markdown;
        markup.value = render_markdown(formatter.content);
        hover.contents = std::move(markup);
        hover.range = to_lsp_range(node->span);
        return hover;
    }

    return std::nullopt;
}

std::optional<lsp::Hover> compute_ast_debug_hover(
    const DocumentState& doc,
    uint32_t line,
    uint32_t col)
{
    const Fern::CompilationUnit* unit = find_unit(doc);
    if (!unit || !unit->ast) return std::nullopt;

    BaseSyntax* node = HitTestVisitor::find(unit->ast, line, col);
    if (!node) return std::nullopt;

    std::string body = AstDebugFormatter::format(node);
    return make_debug_hover(node->syntax_node_name(), body, node->span);
}

std::optional<lsp::Hover> compute_fhir_debug_hover(
    const DocumentState& doc,
    uint32_t line,
    uint32_t col)
{
    if (!doc.compilation) return std::nullopt;

    for (auto* method : doc.compilation->semantic().methods)
    {
        if (!method || !method->body) continue;
        if (method->body->span.fileId != doc.fileId) continue;
        if (!method->body->span.contains(line, col)) continue;

        FhirHitResult hit = FhirHitTest::find(method->body, line, col);
        FhirNode* node = hit.node ? hit.node : method->body;

        std::string body = FhirDebugFormatter::format(node);
        return make_debug_hover(node->node_name(), body, node->span);
    }

    return std::nullopt;
}
