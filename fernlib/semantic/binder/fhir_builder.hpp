#pragma once

#include <cassert>
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

    FhirFieldRefExpr* field_ref(BaseSyntax* syntax, FhirExpr* thisRef, FieldSymbol* field)
    {
        auto* node = arena.alloc<FhirFieldRefExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = field ? field->type : nullptr;
        node->thisRef = thisRef;
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

    FhirOpExpr* op(BaseSyntax* syntax, TypeSymbol* type, IntrinsicOp op,
                   std::initializer_list<FhirExpr*> args, MethodSymbol* method = nullptr)
    {
        auto* node = arena.alloc<FhirOpExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->op = op;
        node->method = method;
        node->args = args;
        return node;
    }

    FhirCallExpr* call(BaseSyntax* syntax, TypeSymbol* type, FhirMethodRefExpr* callee,
                       std::vector<FhirExpr*> args)
    {
        auto* node = arena.alloc<FhirCallExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->callee = callee;
        node->arguments = std::move(args);
        return node;
    }

    FhirCallExpr* call(BaseSyntax* syntax, TypeSymbol* type, MethodSymbol* method,
                       std::vector<FhirExpr*> args)
    {
        return call(syntax, type, method_ref(syntax, method, nullptr), std::move(args));
    }

    FhirCallExpr* call(BaseSyntax* syntax, TypeSymbol* type, MethodSymbol* method,
                       FhirExpr* thisRef, std::vector<FhirExpr*> args)
    {
        return call(syntax, type, method_ref(syntax, method, thisRef), std::move(args));
    }

    FhirConstructionExpr* construction(BaseSyntax* syntax, TypeSymbol* type,
                                       FhirTypeRef* typeRef, MethodSymbol* ctor,
                                       std::vector<FhirExpr*> args)
    {
        assert(typeRef && "construction requires a typeRef. Synthesize one with fhir.type_ref(syntax, type) if no user written type exists.");

        auto* node = arena.alloc<FhirConstructionExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->typeRef = typeRef;

        auto* innerCallee = unchecked_method_ref(syntax, ctor, nullptr);
        auto* innerCall = arena.alloc<FhirCallExpr>();
        innerCall->syntax = syntax;
        innerCall->span = syntax ? syntax->span : Span{};
        innerCall->type = type;
        innerCall->callee = innerCallee;
        innerCall->arguments = std::move(args);
        node->call = innerCall;

        return node;
    }

    FhirCastExpr* cast(BaseSyntax* syntax, TypeSymbol* targetType, FhirExpr* operand, MethodSymbol* method, FhirTypeRef* typeRef = nullptr)
    {
        auto* node = arena.alloc<FhirCastExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = targetType;
        node->operand = operand;
        node->typeRef = typeRef;
        node->method = method;
        return node;
    }

    FhirIndexExpr* index_expr(BaseSyntax* syntax, TypeSymbol* type, FhirExpr* object, FhirExpr* index, MethodSymbol* getter, MethodSymbol* setter = nullptr)
    {
        auto* node = arena.alloc<FhirIndexExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->object = object;
        node->index = index;
        node->getter = getter;
        node->setter = setter;
        return node;
    }

    FhirInitializerExpr* initializer_expr(BaseSyntax* syntax, TypeSymbol* type, FhirExpr* construction, std::vector<FhirInitializerEntry> entries)
    {
        auto* node = arena.alloc<FhirInitializerExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->construction = construction;
        node->entries = std::move(entries);
        return node;
    }

    FhirErrorExpr* error_expr(BaseSyntax* syntax, TypeSymbol* type = nullptr, FhirExpr* inner = nullptr)
    {
        auto* node = arena.alloc<FhirErrorExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = type;
        node->inner = inner;
        return node;
    }

    FhirNamespaceRefExpr* namespace_ref(BaseSyntax* syntax, NamespaceSymbol* ns)
    {
        auto* node = arena.alloc<FhirNamespaceRefExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = nullptr;
        node->namespaceSymbol = ns;
        return node;
    }

    FhirMethodGroupRefExpr* method_group_ref(BaseSyntax* syntax, Symbol* enclosingScope,
                                             std::string_view name, FhirExpr* thisRef = nullptr)
    {
        auto* node = arena.alloc<FhirMethodGroupRefExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = nullptr;
        node->enclosingScope = enclosingScope;
        node->name = name;
        node->thisRef = thisRef;
        return node;
    }

    FhirMethodRefExpr* method_ref(BaseSyntax* syntax, MethodSymbol* method, FhirExpr* thisRef = nullptr)
    {
        // Constructors only ever appear inside FhirConstructionExpr.call,
        // never as a top level method ref. Use construction() for those.
        assert(!method || !method->is_constructor());
        return unchecked_method_ref(syntax, method, thisRef);
    }

    FhirMethodRefExpr* unchecked_method_ref(BaseSyntax* syntax, MethodSymbol* method, FhirExpr* thisRef = nullptr)
    {
        auto* node = arena.alloc<FhirMethodRefExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->type = nullptr;
        node->method = method;
        node->thisRef = thisRef;
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

    FhirCompoundAssignExpr* compound_assign(BaseSyntax* syntax, FhirExpr* target, IntrinsicOp op, MethodSymbol* method, FhirExpr* value)
    {
        auto* node = arena.alloc<FhirCompoundAssignExpr>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        // TODO: assignments wil later no longer be expressions so they will not have a type
        node->type = value ? value->type : nullptr;
        node->target = target;
        node->op = op;
        node->method = method;
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

    FhirVarDeclStmt* var_decl(BaseSyntax* syntax, LocalSymbol* local, FhirExpr* initializer, FhirTypeRef* typeRef = nullptr)
    {
        auto* node = arena.alloc<FhirVarDeclStmt>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->local = local;
        node->typeRef = typeRef;
        node->initializer = initializer;
        return node;
    }

    FhirTypeRef* type_ref(BaseSyntax* syntax, TypeSymbol* referenced, std::vector<FhirTypeRef*> args = {})
    {
        auto* node = arena.alloc<FhirTypeRef>();
        node->syntax = syntax;
        node->span = syntax ? syntax->span : Span{};
        node->referenced = referenced;
        node->args = std::move(args);
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
