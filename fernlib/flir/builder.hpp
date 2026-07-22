#pragma once

#include <format>
#include <string_view>
#include <utility>
#include <vector>

#include <arena.hpp>
#include <ast/ast.hpp>
#include <flir/flir.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

// A user value type moves as raw bytes through an address. Scalars, String, and ref handles move as a value.
inline bool is_memory_class(TypeSymbol* type)
{
    auto* named = type ? type->as<NamedTypeSymbol>() : nullptr;
    if (!named) return false;
    return !named->is_builtin() && !named->is_ref();
}

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

    FlirLocalAddr* local_addr(BaseSyntax* syntax, FlirLocal* local)
    {
        auto* node = arena.alloc<FlirLocalAddr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = local ? local->type : nullptr;
        node->local = local;
        return node;
    }

    FlirFieldAddr* field_addr(BaseSyntax* syntax, FlirExpr* base, FieldSymbol* field)
    {
        auto* node = arena.alloc<FlirFieldAddr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = field ? field->type : nullptr;
        node->base = base;
        node->field = field;
        return node;
    }

    FlirElemAddr* elem_addr(BaseSyntax* syntax, FlirExpr* base, FlirExpr* index, TypeSymbol* elemType)
    {
        auto* node = arena.alloc<FlirElemAddr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = elemType;
        node->base = base;
        node->index = index;
        node->elemType = elemType;
        return node;
    }

    FlirLoad* load(BaseSyntax* syntax, TypeSymbol* type, FlirExpr* address)
    {
        auto* node = arena.alloc<FlirLoad>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->address = address;
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

    FlirIntrinsic* intrinsic(BaseSyntax* syntax, TypeSymbol* type, IntrinsicKind op,
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

    FlirSequence* sequence(BaseSyntax* syntax, std::vector<FlirStmt*> sideEffects, FlirExpr* value)
    {
        auto* node = arena.alloc<FlirSequence>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = value ? value->type : nullptr;
        node->sideEffects = std::move(sideEffects);
        node->value = value;
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

    FlirStore* store(BaseSyntax* syntax, FlirExpr* address, FlirExpr* value)
    {
        auto* node = arena.alloc<FlirStore>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->address = address;
        node->value = value;
        return node;
    }

    FlirCopy* copy(BaseSyntax* syntax, FlirExpr* dest, FlirExpr* src, TypeSymbol* type)
    {
        auto* node = arena.alloc<FlirCopy>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->dest = dest;
        node->src = src;
        node->type = type;
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

    // Synthetic locals get a $ prefix, and index appended to make sure they are unique
    FlirLocal* synthetic_local(FlirMethod* method, std::string_view hint, TypeSymbol* type)
    {
        int index = static_cast<int>(method->locals.size());
        std::string_view name = arena.alloc_string(std::format("${}_{}", hint, index));
        auto* node = arena.alloc<FlirLocal>();
        node->name = name;
        node->type = type;
        node->index = index;
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
