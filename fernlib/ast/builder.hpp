#pragma once

#include <arena.hpp>
#include <ast/ast.hpp>

namespace Fern
{

struct AstBuilder
{
    AllocArena& arena;

    explicit AstBuilder(AllocArena& arena) : arena(arena) {}

#pragma region Span Helpers

    static void merge_if(Span& span, BaseSyntax* node)
    {
        if (node) span = span.merge(node->span);
    }

    static void merge_if(Span& span, const Token* token)
    {
        if (token) span = span.merge(token->span);
    }

#pragma region Names and Literals

    IdentifierExprSyntax* identifier(Token name)
    {
        auto* node = arena.alloc<IdentifierExprSyntax>();
        node->name = name;
        node->span = name.span;
        return node;
    }

    LiteralExprSyntax* literal(Token tok)
    {
        auto* node = arena.alloc<LiteralExprSyntax>();
        node->token = tok;
        node->span = tok.span;
        return node;
    }

    ThisExprSyntax* this_expr(Token tok)
    {
        auto* node = arena.alloc<ThisExprSyntax>();
        node->token = tok;
        node->span = tok.span;
        return node;
    }

#pragma region Expressions

    ParenExprSyntax* paren(BaseExprSyntax* inner, Span span)
    {
        auto* node = arena.alloc<ParenExprSyntax>();
        node->expression = inner;
        node->span = span;
        return node;
    }

    UnaryExprSyntax* unary(UnaryOp op, BaseExprSyntax* operand, Span span)
    {
        auto* node = arena.alloc<UnaryExprSyntax>();
        node->op = op;
        node->operand = operand;
        node->span = span;
        return node;
    }

    BinaryExprSyntax* binary(BaseExprSyntax* left, BinaryOp op, BaseExprSyntax* right)
    {
        auto* node = arena.alloc<BinaryExprSyntax>();
        node->left = left;
        node->op = op;
        node->right = right;
        Span span = left->span;
        merge_if(span, right);
        node->span = span;
        return node;
    }

    AssignmentExprSyntax* assignment(BaseExprSyntax* target, AssignOp op, BaseExprSyntax* value)
    {
        auto* node = arena.alloc<AssignmentExprSyntax>();
        node->target = target;
        node->op = op;
        node->value = value;
        Span span = target->span;
        merge_if(span, value);
        node->span = span;
        return node;
    }

    MemberAccessExprSyntax* member_access(BaseExprSyntax* left, SimpleNameExprSyntax* right)
    {
        auto* node = arena.alloc<MemberAccessExprSyntax>();
        node->left = left;
        node->right = right;
        Span span = left->span;
        merge_if(span, right);
        node->span = span;
        return node;
    }

    CastExprSyntax* cast(TypeExprSyntax* type, BaseExprSyntax* operand, Span span)
    {
        auto* node = arena.alloc<CastExprSyntax>();
        node->type = type;
        node->operand = operand;
        node->span = span;
        return node;
    }

    IndexExprSyntax* index(BaseExprSyntax* object, BaseExprSyntax* idx, Span span)
    {
        auto* node = arena.alloc<IndexExprSyntax>();
        node->object = object;
        node->index = idx;
        node->span = span;
        return node;
    }

    LiteralSuffixExprSyntax* literal_suffix(BaseExprSyntax* operand, Token suffix)
    {
        auto* node = arena.alloc<LiteralSuffixExprSyntax>();
        node->operand = operand;
        node->suffix = suffix;
        node->span = operand->span.merge(suffix.span);
        return node;
    }

    ArrayTypeExprSyntax* array_type(TypeExprSyntax* elementType, Span span)
    {
        auto* node = arena.alloc<ArrayTypeExprSyntax>();
        node->elementType = elementType;
        node->span = span;
        return node;
    }

    FieldInitSyntax* field_init(BaseExprSyntax* target, BaseExprSyntax* value, Span span)
    {
        auto* node = arena.alloc<FieldInitSyntax>();
        node->target = target;
        node->value = value;
        node->span = span;
        return node;
    }

    AttributeSyntax* attribute(BaseExprSyntax* value, Span span)
    {
        auto* node = arena.alloc<AttributeSyntax>();
        node->value = value;
        node->span = span;
        return node;
    }

#pragma region Statements

    ExpressionStmtSyntax* expr_stmt(BaseExprSyntax* expr)
    {
        auto* node = arena.alloc<ExpressionStmtSyntax>();
        node->expression = expr;
        node->span = expr->span;
        return node;
    }

    ErrorStmtSyntax* error_stmt(Span span)
    {
        auto* node = arena.alloc<ErrorStmtSyntax>();
        node->span = span;
        return node;
    }

    ErrorStmtSyntax* error_stmt(BaseSyntax* wrapped, Span span)
    {
        auto* node = arena.alloc<ErrorStmtSyntax>();
        node->wrapped = wrapped;
        node->span = span;
        return node;
    }

#pragma region Declarations

    void attach_metadata(BaseDeclSyntax* decl,
                         Modifier mods,
                         Span modSpan,
                         std::vector<Token>& modTokens,
                         std::vector<AttributeSyntax*>& attrs)
    {
        decl->modifiers = decl->modifiers | mods;
        decl->modifierTokens = std::move(modTokens);
        decl->attributes = std::move(attrs);
        if (mods != Modifier::None)
        {
            decl->span = modSpan.merge(decl->span);
        }
        if (!decl->attributes.empty())
        {
            decl->span = decl->attributes.front()->span.merge(decl->span);
        }
    }
};

}
