#include "lower.hpp"

#include <semantic/context.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

#pragma region Construction

FlirLowerer::FlirLowerer(SemanticContext& semantic, FlirContext& flir)
    : semantic(semantic)
    , flir(flir)
    , builder(flir.arena)
{
}

#pragma region Method Entry

FlirMethod* FlirLowerer::lower_method(FhirMethod* method)
{
    if (!method) return nullptr;

    currentMethod = builder.method(method->symbol, nullptr);

    if (method->symbol)
    {
        auto* parentType = method->symbol->parent ? method->symbol->parent->as<NamedTypeSymbol>() : nullptr;
        bool isStatic = has_modifier(method->symbol->modifiers, Modifier::Static);
        if (parentType && (method->symbol->is_constructor() || !isStatic))
        {
            builder.param(currentMethod, "this", parentType);
        }

        for (auto* param : method->symbol->parameters)
        {
            auto* slot = builder.param(currentMethod, param->name, param->type);
            flir.slots[param] = slot;
        }
    }

    currentMethod->body = lower_block(method->body);

    FlirMethod* result = currentMethod;
    currentMethod = nullptr;
    return result;
}

#pragma region Blocks and Statements

FlirBlock* FlirLowerer::lower_block(FhirBlock* block)
{
    if (!block) return nullptr;

    auto* result = builder.block(block->syntax);
    for (auto* stmt : block->statements)
        lower_stmt(stmt, result->statements);
    return result;
}

void FlirLowerer::lower_stmt(FhirStmt* stmt, std::vector<FlirStmt*>& out)
{
    if (!stmt) return;

    if (auto* s = stmt->as<FhirVarDeclStmt>())    return lower_var_decl(s, out);
    if (auto* s = stmt->as<FhirExprStmt>())       return lower_expr_stmt(s, out);
    if (auto* s = stmt->as<FhirReturnStmt>())     return lower_return(s, out);
    if (auto* s = stmt->as<FhirIfStmt>())         return lower_if(s, out);
    if (auto* s = stmt->as<FhirWhileStmt>())      return lower_while(s, out);
}

void FlirLowerer::lower_var_decl(FhirVarDeclStmt* stmt, std::vector<FlirStmt*>& out)
{
    if (!stmt->local) return;
    auto* slot = builder.local(currentMethod, stmt->local->name, stmt->local->type);
    flir.slots[stmt->local] = slot;
    if (stmt->initializer)
    {
        auto* target = builder.load_local(stmt->syntax, slot);
        auto* value = lower_expr(stmt->initializer);
        out.push_back(builder.assign(stmt->syntax, target, value));
    }
}

void FlirLowerer::lower_expr_stmt(FhirExprStmt* stmt, std::vector<FlirStmt*>& out)
{
    if (auto* assign = stmt->expression ? stmt->expression->as<FhirAssignExpr>() : nullptr)
    {
        lower_assign_stmt(assign, out);
        return;
    }
    auto* lowered = lower_expr(stmt->expression);
    out.push_back(builder.expr_stmt(stmt->syntax, lowered));
}

void FlirLowerer::lower_assign_stmt(FhirAssignExpr* assign, std::vector<FlirStmt*>& out)
{
    if (auto* indexTarget = assign->target ? assign->target->as<FhirIndexExpr>() : nullptr;
        indexTarget && indexTarget->setter)
    {
        lower_index_assign(indexTarget, assign->value, assign->syntax, out);
        return;
    }
    auto* target = lower_expr(assign->target);
    auto* value = lower_expr(assign->value);
    out.push_back(builder.assign(assign->syntax, target, value));
}

void FlirLowerer::lower_return(FhirReturnStmt* stmt, std::vector<FlirStmt*>& out)
{
    out.push_back(builder.return_stmt(stmt->syntax, lower_expr(stmt->value)));
}

void FlirLowerer::lower_if(FhirIfStmt* stmt, std::vector<FlirStmt*>& out)
{
    auto* condition = lower_expr(stmt->condition);
    auto* thenBlock = lower_block(stmt->thenBlock);

    FlirBlock* elseBlock = nullptr;
    if (stmt->elseIf)
    {
        elseBlock = builder.block(stmt->elseIf->syntax);
        lower_if(stmt->elseIf, elseBlock->statements);
    }
    else if (stmt->elseBlock)
    {
        elseBlock = lower_block(stmt->elseBlock);
    }

    out.push_back(builder.if_stmt(stmt->syntax, condition, thenBlock, elseBlock));
}

void FlirLowerer::lower_while(FhirWhileStmt* stmt, std::vector<FlirStmt*>& out)
{
    auto* cond = lower_expr(stmt->condition);
    auto* condType = cond ? cond->type : nullptr;
    auto* notCond = builder.intrinsic(stmt->syntax, condType, IntrinsicOp::Not, { cond });

    auto* breakBlock = builder.block(stmt->syntax);
    breakBlock->statements.push_back(builder.break_stmt(stmt->syntax));

    auto* bodySyntax = stmt->body ? stmt->body->syntax : stmt->syntax;
    auto* loopBody = builder.block(bodySyntax);
    loopBody->statements.push_back(builder.if_stmt(stmt->syntax, notCond, breakBlock, nullptr));

    if (stmt->body)
    {
        for (auto* s : stmt->body->statements)
            lower_stmt(s, loopBody->statements);
    }

    out.push_back(builder.loop(stmt->syntax, loopBody));
}

#pragma region Expressions

FlirExpr* FlirLowerer::lower_expr(FhirExpr* expr)
{
    if (!expr) return nullptr;

    if (auto* e = expr->as<FhirLiteralExpr>())      return lower_literal(e);
    if (auto* e = expr->as<FhirLocalRefExpr>())     return lower_local_ref(e);
    if (auto* e = expr->as<FhirParamRefExpr>())     return lower_param_ref(e);
    if (auto* e = expr->as<FhirFieldRefExpr>())     return lower_field_ref(e);
    if (auto* e = expr->as<FhirThisExpr>())         return lower_this(e);
    if (auto* e = expr->as<FhirOpExpr>())           return lower_op(e);
    if (auto* e = expr->as<FhirCallExpr>())         return lower_call(e);
    if (auto* e = expr->as<FhirConstructionExpr>()) return lower_construction(e);
    if (auto* e = expr->as<FhirAssignExpr>())       return lower_assign(e);
    if (auto* e = expr->as<FhirCompoundAssignExpr>()) return lower_compound_assign(e);
    if (auto* e = expr->as<FhirCastExpr>())         return lower_cast(e);
    if (auto* e = expr->as<FhirIndexExpr>())        return lower_index(e);
    if (auto* e = expr->as<FhirInitializerExpr>())  return lower_initializer(e);

    return nullptr;
}

FlirExpr* FlirLowerer::lower_literal(FhirLiteralExpr* expr)
{
    return builder.constant(expr->syntax, expr->type, expr->value);
}

FlirExpr* FlirLowerer::lower_local_ref(FhirLocalRefExpr* expr)
{
    return builder.load_local(expr->syntax, flir.lookup_local_symbol(expr->local));
}

FlirExpr* FlirLowerer::lower_param_ref(FhirParamRefExpr* expr)
{
    return builder.load_local(expr->syntax, flir.lookup_param_symbol(expr->parameter));
}

FlirExpr* FlirLowerer::lower_field_ref(FhirFieldRefExpr* expr)
{
    auto* base = lower_expr(expr->thisRef);
    return builder.load_field(expr->syntax, base, expr->field);
}

FlirExpr* FlirLowerer::lower_this(FhirThisExpr* expr)
{
    if (!currentMethod || currentMethod->parameters.empty()) return nullptr;
    return builder.load_local(expr->syntax, currentMethod->parameters[0]);
}

FlirExpr* FlirLowerer::lower_op(FhirOpExpr* expr)
{
    std::vector<FlirExpr*> args;
    args.reserve(expr->args.size());
    for (auto* a : expr->args)
        args.push_back(lower_expr(a));

    return builder.call_or_intrinsic(expr->syntax, expr->type, expr->op, expr->method, std::move(args));
}

FlirExpr* FlirLowerer::lower_call(FhirCallExpr* expr)
{
    auto* callee = expr->callee;
    MethodSymbol* method = callee ? callee->method : nullptr;
    FlirExpr* thisArg = callee ? lower_expr(callee->thisRef) : nullptr;

    std::vector<FlirExpr*> args;
    args.reserve(expr->arguments.size());
    for (auto* a : expr->arguments)
        args.push_back(lower_expr(a));

    return builder.call(expr->syntax, expr->type, method, thisArg, std::move(args));
}

FlirExpr* FlirLowerer::lower_construction(FhirConstructionExpr* expr)
{
    if (!expr->call || !expr->call->callee) return nullptr;

    auto* ctor = expr->call->callee->method;
    auto* type = expr->type;

    auto* alloc = builder.alloc_expr(expr->syntax, type);

    std::vector<FlirExpr*> args;
    args.reserve(expr->call->arguments.size());
    for (auto* a : expr->call->arguments)
        args.push_back(lower_expr(a));

    return builder.call(expr->syntax, type, ctor, alloc, std::move(args));
}

// TODO: Assigns will no longer be expressions in the future
FlirExpr* FlirLowerer::lower_assign(FhirAssignExpr*) { return nullptr; }


// TODO: We should not be using load_local as the target of an assign. 
// Need to refactor hwo assignment works
FlirExpr* FlirLowerer::lower_compound_assign(FhirCompoundAssignExpr* expr)
{
    BaseSyntax* syntax = expr->syntax;
    TypeSymbol* type = expr->type;
    std::vector<FlirStmt*> sideEffects;
    FlirExpr* loweredValue = lower_expr(expr->value);

    TypeSymbol* rhsType = expr->value ? expr->value->type : nullptr;

    if (auto* idx = expr->target->as<FhirIndexExpr>())
    {
        TypeSymbol* elementType = idx->getter ? idx->getter->get_return_type() : type;
        TypeSymbol* setterReturn = idx->setter ? idx->setter->get_return_type() : nullptr;

        auto* tmpObj = builder.synthetic_local(currentMethod, "tmp_obj", idx->object ? idx->object->type : nullptr);
        auto* tmpIdx = builder.synthetic_local(currentMethod, "tmp_idx", idx->index ? idx->index->type : nullptr);
        auto* tmpRhs = builder.synthetic_local(currentMethod, "tmp_rhs", rhsType);
        auto* tmpVal = builder.synthetic_local(currentMethod, "tmp_val", elementType);

        sideEffects.push_back(builder.assign(syntax, builder.load_local(syntax, tmpObj), lower_expr(idx->object)));
        sideEffects.push_back(builder.assign(syntax, builder.load_local(syntax, tmpIdx), lower_expr(idx->index)));
        sideEffects.push_back(builder.assign(syntax, builder.load_local(syntax, tmpRhs), loweredValue));

        auto* readGetter = builder.call(syntax, elementType, idx->getter, nullptr,
            { builder.load_local(syntax, tmpObj), builder.load_local(syntax, tmpIdx) });
        auto* binResult = builder.call_or_intrinsic(syntax, elementType, expr->op, expr->method, { readGetter, builder.load_local(syntax, tmpRhs) });
        sideEffects.push_back(builder.assign(syntax, builder.load_local(syntax, tmpVal), binResult));

        auto* setterCall = builder.call(syntax, setterReturn, idx->setter, nullptr,
            { builder.load_local(syntax, tmpObj), builder.load_local(syntax, tmpIdx), builder.load_local(syntax, tmpVal) });
        sideEffects.push_back(builder.expr_stmt(syntax, setterCall));

        return builder.sequence(syntax, std::move(sideEffects), builder.load_local(syntax, tmpVal));
    }

    if (auto* field = expr->target->as<FhirFieldRefExpr>())
    {
        auto* tmpRhs = builder.synthetic_local(currentMethod, "tmp_rhs", rhsType);
        auto* tmpVal = builder.synthetic_local(currentMethod, "tmp_val", type);

        FlirLocal* tmpObj = nullptr;
        if (field->thisRef)
        {
            tmpObj = builder.synthetic_local(currentMethod, "tmp_obj", field->thisRef->type);
            sideEffects.push_back(builder.assign(syntax, builder.load_local(syntax, tmpObj), lower_expr(field->thisRef)));
        }

        sideEffects.push_back(builder.assign(syntax, builder.load_local(syntax, tmpRhs), loweredValue));

        auto* readField = builder.load_field(syntax, tmpObj ? builder.load_local(syntax, tmpObj) : nullptr, field->field);
        auto* binResult = builder.call_or_intrinsic(syntax, type, expr->op, expr->method, { readField, builder.load_local(syntax, tmpRhs) });
        sideEffects.push_back(builder.assign(syntax, builder.load_local(syntax, tmpVal), binResult));

        auto* writeField = builder.load_field(syntax, tmpObj ? builder.load_local(syntax, tmpObj) : nullptr, field->field);
        sideEffects.push_back(builder.assign(syntax, writeField, builder.load_local(syntax, tmpVal)));

        return builder.sequence(syntax, std::move(sideEffects), builder.load_local(syntax, tmpVal));
    }

    auto* tmpRhs = builder.synthetic_local(currentMethod, "tmp_rhs", rhsType);
    auto* tmpVal = builder.synthetic_local(currentMethod, "tmp_val", type);

    sideEffects.push_back(builder.assign(syntax, builder.load_local(syntax, tmpRhs), loweredValue));

    auto* read = lower_expr(expr->target);
    auto* binResult = builder.call_or_intrinsic(syntax, type, expr->op, expr->method, { read, builder.load_local(syntax, tmpRhs) });
    sideEffects.push_back(builder.assign(syntax, builder.load_local(syntax, tmpVal), binResult));

    auto* write = lower_expr(expr->target);
    sideEffects.push_back(builder.assign(syntax, write, builder.load_local(syntax, tmpVal)));

    return builder.sequence(syntax, std::move(sideEffects), builder.load_local(syntax, tmpVal));
}

FlirExpr* FlirLowerer::lower_cast(FhirCastExpr* expr)
{
    auto* operand = lower_expr(expr->operand);
    if (expr->method && !expr->method->is_intrinsic())
    {
        return builder.call(expr->syntax, expr->type, expr->method, nullptr, { operand });
    }
    return builder.cast(expr->syntax, expr->type, operand);
}

FlirExpr* FlirLowerer::lower_index(FhirIndexExpr* expr)
{
    auto* object = lower_expr(expr->object);
    auto* index = lower_expr(expr->index);
    return builder.call(expr->syntax, expr->type, expr->getter, nullptr, { object, index });
}

void FlirLowerer::lower_index_assign(FhirIndexExpr* target, FhirExpr* valueExpr, BaseSyntax* syntax, std::vector<FlirStmt*>& out)
{
    auto* object = lower_expr(target->object);
    auto* index = lower_expr(target->index);
    auto* value = lower_expr(valueExpr);
    TypeSymbol* setterReturn = target->setter ? target->setter->get_return_type() : nullptr;
    auto* call = builder.call(syntax, setterReturn, target->setter, nullptr, { object, index, value });
    out.push_back(builder.expr_stmt(syntax, call));
}

// Lowers Foo { a = 1, b.c = 2 } to a Sequence that holds the constructed
// instance in a temp, writes each entry's value to its field chain off the
// temp, then yields the temp.
FlirExpr* FlirLowerer::lower_initializer(FhirInitializerExpr* expr)
{
    BaseSyntax* syntax = expr->syntax;
    TypeSymbol* type = expr->type;

    auto* tmp = builder.local(currentMethod, "$init", type);

    std::vector<FlirStmt*> sideEffects;
    sideEffects.push_back(builder.assign(syntax, builder.load_local(syntax, tmp), lower_expr(expr->construction)));

    for (const auto& entry : expr->entries)
    {
        if (entry.path.empty() || !entry.value) continue;
        FlirExpr* chain = builder.load_local(syntax, tmp);
        for (auto* field : entry.path)
        {
            chain = builder.load_field(syntax, chain, field);
        }
        auto* value = lower_expr(entry.value);
        sideEffects.push_back(builder.assign(syntax, chain, value));
    }

    return builder.sequence(syntax, std::move(sideEffects), builder.load_local(syntax, tmp));
}

#pragma region Local Helpers

}
