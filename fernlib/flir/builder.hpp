#pragma once

#include <string_view>
#include <utility>
#include <vector>

#include <arena.hpp>
#include <ast/ast.hpp>
#include <flir/flir.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

struct FlirBuilder
{
    AllocArena& arena;

    explicit FlirBuilder(AllocArena& arena) : arena(arena) {}

#pragma region Expressions

    FlirConst* constant(BaseSyntax* syntax, TypeSymbol* type, ConstantValue value)
    {
        auto* node = arena.alloc<FlirConst>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->value = value;
        return node;
    }

    FlirLoadLocal* load_local(BaseSyntax* syntax, FlirLocal* local)
    {
        auto* node = arena.alloc<FlirLoadLocal>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = local ? local->type : nullptr;
        node->local = local;
        return node;
    }

    FlirLoadField* load_field(BaseSyntax* syntax, FlirExpr* base, FieldSymbol* field)
    {
        auto* node = arena.alloc<FlirLoadField>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = field ? field->type : nullptr;
        node->base = base;
        node->field = field;
        return node;
    }

    FlirCall* call(BaseSyntax* syntax, TypeSymbol* type, MethodSymbol* method,
                   FlirExpr* thisArg, std::vector<FlirExpr*> args)
    {
        auto* node = arena.alloc<FlirCall>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->method = method;
        node->thisArg = thisArg;
        node->args = std::move(args);
        return node;
    }

    FlirIntrinsic* intrinsic(BaseSyntax* syntax, TypeSymbol* type, IntrinsicOp op,
                             std::vector<FlirExpr*> args)
    {
        auto* node = arena.alloc<FlirIntrinsic>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->op = op;
        node->args = std::move(args);
        return node;
    }

    FlirCast* cast(BaseSyntax* syntax, TypeSymbol* targetType, FlirExpr* operand)
    {
        auto* node = arena.alloc<FlirCast>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = targetType;
        node->targetType = targetType;
        node->operand = operand;
        return node;
    }

    FlirAlloc* alloc_expr(BaseSyntax* syntax, TypeSymbol* type)
    {
        auto* node = arena.alloc<FlirAlloc>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->allocType = type;
        return node;
    }

#pragma region Statements

    FlirBlock* block(BaseSyntax* syntax)
    {
        auto* node = arena.alloc<FlirBlock>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        return node;
    }

    FlirAssign* assign(BaseSyntax* syntax, FlirExpr* target, FlirExpr* value)
    {
        auto* node = arena.alloc<FlirAssign>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->target = target;
        node->value = value;
        return node;
    }

    FlirExprStmt* expr_stmt(BaseSyntax* syntax, FlirExpr* expression)
    {
        auto* node = arena.alloc<FlirExprStmt>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->expression = expression;
        return node;
    }

    FlirIf* if_stmt(BaseSyntax* syntax, FlirExpr* condition,
                    FlirBlock* thenBlock, FlirBlock* elseBlock)
    {
        auto* node = arena.alloc<FlirIf>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->condition = condition;
        node->thenBlock = thenBlock;
        node->elseBlock = elseBlock;
        return node;
    }

    FlirLoop* loop(BaseSyntax* syntax, FlirBlock* body)
    {
        auto* node = arena.alloc<FlirLoop>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->body = body;
        return node;
    }

    FlirBreak* break_stmt(BaseSyntax* syntax)
    {
        auto* node = arena.alloc<FlirBreak>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        return node;
    }

    FlirReturn* return_stmt(BaseSyntax* syntax, FlirExpr* value)
    {
        auto* node = arena.alloc<FlirReturn>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->value = value;
        return node;
    }

#pragma region Locals and Methods

    FlirLocal* local(FlirMethod* method, std::string_view name, TypeSymbol* type)
    {
        auto* node = arena.alloc<FlirLocal>();
        node->name = name;
        node->type = type;
        node->index = static_cast<int>(method->locals.size());
        method->locals.push_back(node);
        return node;
    }

    FlirLocal* param(FlirMethod* method, std::string_view name, TypeSymbol* type)
    {
        auto* node = arena.alloc<FlirLocal>();
        node->name = name;
        node->type = type;
        node->index = static_cast<int>(method->parameters.size());
        method->parameters.push_back(node);
        return node;
    }

    FlirMethod* method(MethodSymbol* symbol, FlirBlock* body)
    {
        auto* node = arena.alloc<FlirMethod>();
        node->symbol = symbol;
        node->body = body;
        return node;
    }
};

}
