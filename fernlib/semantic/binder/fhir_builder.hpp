#pragma once

#include <initializer_list>
#include <vector>

#include <arena.hpp>
#include <ast/ast.hpp>
#include <semantic/fhir/fhir.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

struct NamedTypeSymbol;

struct FhirBuilder
{
    AllocArena& arena;

    explicit FhirBuilder(AllocArena& arena) : arena(arena) {}

#pragma region Expressions

    FhirLiteralExpr* literal(BaseSyntax* syntax, TypeSymbol* type)
    {
        auto* node = arena.alloc<FhirLiteralExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        return node;
    }

    FhirLocalRefExpr* local_ref(BaseSyntax* syntax, LocalSymbol* local)
    {
        auto* node = arena.alloc<FhirLocalRefExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = local ? local->type : nullptr;
        node->local = local;
        return node;
    }

    FhirParamRefExpr* param_ref(BaseSyntax* syntax, ParameterSymbol* param)
    {
        auto* node = arena.alloc<FhirParamRefExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = param ? param->type : nullptr;
        node->parameter = param;
        return node;
    }

    FhirFieldAccessExpr* field_access(BaseSyntax* syntax, FhirExpr* object, FieldSymbol* field)
    {
        auto* node = arena.alloc<FhirFieldAccessExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = field ? field->type : nullptr;
        node->object = object;
        node->field = field;
        return node;
    }

    FhirThisExpr* this_expr(BaseSyntax* syntax, TypeSymbol* type)
    {
        auto* node = arena.alloc<FhirThisExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        return node;
    }

    FhirIntrinsicExpr* intrinsic(BaseSyntax* syntax, TypeSymbol* type, IntrinsicOp op,
                                 std::initializer_list<FhirExpr*> args)
    {
        auto* node = arena.alloc<FhirIntrinsicExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->op = op;
        node->args = args;
        return node;
    }

    FhirCallExpr* call(BaseSyntax* syntax, TypeSymbol* type, MethodSymbol* target,
                       std::vector<FhirExpr*> args)
    {
        auto* node = arena.alloc<FhirCallExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->target = target;
        node->arguments = std::move(args);
        return node;
    }

    FhirMethodCallExpr* method_call(BaseSyntax* syntax, TypeSymbol* type,
                                    FhirExpr* receiver, MethodSymbol* method,
                                    std::vector<FhirExpr*> args)
    {
        auto* node = arena.alloc<FhirMethodCallExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->receiver = receiver;
        node->method = method;
        node->arguments = std::move(args);
        return node;
    }

    FhirObjectCreateExpr* object_create(BaseSyntax* syntax, TypeSymbol* type,
                                        MethodSymbol* ctor, std::vector<FhirExpr*> args)
    {
        auto* node = arena.alloc<FhirObjectCreateExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->constructor = ctor;
        node->arguments = std::move(args);
        return node;
    }

    FhirCastExpr* cast(BaseSyntax* syntax, TypeSymbol* targetType, FhirExpr* operand, bool isImplicit)
    {
        auto* node = arena.alloc<FhirCastExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = targetType;
        node->operand = operand;
        node->isImplicit = isImplicit;
        return node;
    }

    FhirErrorExpr* error_expr(BaseSyntax* syntax, TypeSymbol* type = nullptr)
    {
        auto* node = arena.alloc<FhirErrorExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        return node;
    }

    FhirAssignExpr* assign(BaseSyntax* syntax, FhirExpr* target, FhirExpr* value)
    {
        auto* node = arena.alloc<FhirAssignExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = target ? target->type : nullptr;
        node->target = target;
        node->value = value;
        return node;
    }

#pragma region Statements

    FhirBlock* block(BaseSyntax* syntax)
    {
        auto* node = arena.alloc<FhirBlock>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        return node;
    }

    FhirExprStmt* expr_stmt(BaseSyntax* syntax, FhirExpr* expression)
    {
        auto* node = arena.alloc<FhirExprStmt>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->expression = expression;
        return node;
    }

    FhirReturnStmt* return_stmt(BaseSyntax* syntax, FhirExpr* value)
    {
        auto* node = arena.alloc<FhirReturnStmt>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->value = value;
        return node;
    }

    FhirVarDeclStmt* var_decl(BaseSyntax* syntax, LocalSymbol* local, FhirExpr* initializer)
    {
        auto* node = arena.alloc<FhirVarDeclStmt>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->local = local;
        node->initializer = initializer;
        return node;
    }

    FhirIfStmt* if_stmt(BaseSyntax* syntax, FhirExpr* condition, FhirBlock* thenBlock,
                        FhirIfStmt* elseIf, FhirBlock* elseBlock)
    {
        auto* node = arena.alloc<FhirIfStmt>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->condition = condition;
        node->thenBlock = thenBlock;
        node->elseIf = elseIf;
        node->elseBlock = elseBlock;
        return node;
    }

    FhirWhileStmt* while_stmt(BaseSyntax* syntax, FhirExpr* condition, FhirBlock* body)
    {
        auto* node = arena.alloc<FhirWhileStmt>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->condition = condition;
        node->body = body;
        return node;
    }

    FhirMethod* method(MethodSymbol* symbol, FhirBlock* body)
    {
        auto* node = arena.alloc<FhirMethod>();
        node->symbol = symbol;
        node->body = body;
        return node;
    }
};

}
