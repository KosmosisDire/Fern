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

#pragma region Representation Helpers

// Reads a value at an address. Aggregates stay addresses, scalars and handles load.
FlirExpr* FlirLowerer::address_load(BaseSyntax* syntax, FlirExpr* address, TypeSymbol* type)
{
    if (is_memory_class(type)) return address;
    return builder.load(syntax, type, address);
}

FlirExpr* FlirLowerer::read_slot(BaseSyntax* syntax, FlirLocal* slot)
{
    auto* type = slot ? slot->type : nullptr;
    return address_load(syntax, builder.local_addr(syntax, slot), type);
}

// Writes a value into a destination address. Aggregates copy their bytes, scalars and handles store.
void FlirLowerer::emit_assign(BaseSyntax* syntax, FlirExpr* destAddr, FlirExpr* value, TypeSymbol* type, std::vector<FlirStmt*>& out)
{
    if (is_memory_class(type))
        out.push_back(builder.copy(syntax, destAddr, value, type));
    else
        out.push_back(builder.store(syntax, destAddr, value));
}

// Builds a call. An aggregate return is written into a temp through the result destination,
// and the call expression evaluates to that temp's address.
FlirExpr* FlirLowerer::build_call(BaseSyntax* syntax, TypeSymbol* retType, MethodSymbol* method, FlirExpr* thisArg, std::vector<FlirExpr*> args)
{
    auto* call = builder.call(syntax, retType, method, thisArg, std::move(args));
    if (!is_memory_class(retType)) return call;

    auto* temp = builder.synthetic_local(currentMethod, "ret", retType);
    call->resultDest = builder.local_addr(syntax, temp);

    std::vector<FlirStmt*> effects;
    effects.push_back(builder.expr_stmt(syntax, call));
    return builder.sequence(syntax, std::move(effects), builder.local_addr(syntax, temp));
}

// Applies a binary operator, either a user method (aggregate aware) or a primitive intrinsic.
FlirExpr* FlirLowerer::apply_bin(BaseSyntax* syntax, TypeSymbol* type, FhirOpExpr* binaryOp, FlirExpr* lhs, FlirExpr* rhs)
{
    if (binaryOp->method && !binaryOp->method->is_intrinsic())
        return build_call(syntax, type, binaryOp->method, nullptr, { lhs, rhs });
    return builder.intrinsic(syntax, type, binaryOp->op, { lhs, rhs });
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
        auto* value = lower_expr(stmt->initializer);
        emit_assign(stmt->syntax, builder.local_addr(stmt->syntax, slot), value, slot->type, out);
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
    if (lowered)
        out.push_back(builder.expr_stmt(stmt->syntax, lowered));
}

void FlirLowerer::lower_assign_stmt(FhirAssignExpr* assign, std::vector<FlirStmt*>& out)
{
    auto* value = lower_expr(assign->value);
    lower_store(assign->target, value, assign->syntax, out);
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
    auto* notCond = builder.intrinsic(stmt->syntax, condType, IntrinsicKind::BoolNot, { cond });

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
    if (auto* e = expr->as<FhirObjectBuilderExpr>())  return lower_object_builder(e);
    if (auto* e = expr->as<FhirArrayLiteralExpr>()) return lower_array_literal(e);

    return nullptr;
}

FlirExpr* FlirLowerer::lower_literal(FhirLiteralExpr* expr)
{
    return builder.constant(expr->syntax, expr->type, expr->value);
}

FlirExpr* FlirLowerer::lower_local_ref(FhirLocalRefExpr* expr)
{
    return read_slot(expr->syntax, flir.lookup_local_symbol(expr->symbol));
}

FlirExpr* FlirLowerer::lower_param_ref(FhirParamRefExpr* expr)
{
    return read_slot(expr->syntax, flir.lookup_param_symbol(expr->symbol));
}

FlirExpr* FlirLowerer::lower_field_ref(FhirFieldRefExpr* expr)
{
    auto* base = lower_expr(expr->thisRef);
    auto* addr = builder.field_addr(expr->syntax, base, expr->symbol);
    auto* fieldType = expr->symbol ? expr->symbol->type : nullptr;
    return address_load(expr->syntax, addr, fieldType);
}

FlirExpr* FlirLowerer::lower_this(FhirThisExpr* expr)
{
    if (!currentMethod || currentMethod->parameters.empty()) return nullptr;
    return read_slot(expr->syntax, currentMethod->parameters[0]);
}

FlirExpr* FlirLowerer::lower_op(FhirOpExpr* expr)
{
    if (expr->op == IntrinsicKind::BoolAnd || expr->op == IntrinsicKind::BoolOr)
        return lower_short_circuit(expr);

    std::vector<FlirExpr*> args;
    args.reserve(expr->args.size());
    for (auto* a : expr->args)
        args.push_back(lower_expr(a));

    if (expr->method && !expr->method->is_intrinsic())
        return build_call(expr->syntax, expr->type, expr->method, nullptr, std::move(args));
    return builder.intrinsic(expr->syntax, expr->type, expr->op, std::move(args));
}

// Rewrites a && b to (tmp = a; if (tmp) tmp = b; yield tmp) and a || b to
// (tmp = a; if (!tmp) tmp = b; yield tmp) so the right side only runs when needed
FlirExpr* FlirLowerer::lower_short_circuit(FhirOpExpr* expr)
{
    BaseSyntax* syntax = expr->syntax;
    TypeSymbol* type = expr->type;
    bool isAnd = expr->op == IntrinsicKind::BoolAnd;

    auto* tmp = builder.synthetic_local(currentMethod, "sc", type);

    std::vector<FlirStmt*> sideEffects;
    sideEffects.push_back(builder.store(syntax, builder.local_addr(syntax, tmp), lower_expr(expr->args[0])));

    auto* thenBlock = builder.block(syntax);
    thenBlock->statements.push_back(builder.store(syntax, builder.local_addr(syntax, tmp), lower_expr(expr->args[1])));

    FlirExpr* condition = builder.load(syntax, type, builder.local_addr(syntax, tmp));
    if (!isAnd)
        condition = builder.intrinsic(syntax, type, IntrinsicKind::BoolNot, { condition });

    sideEffects.push_back(builder.if_stmt(syntax, condition, thenBlock, nullptr));

    return builder.sequence(syntax, std::move(sideEffects), builder.load(syntax, type, builder.local_addr(syntax, tmp)));
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

    return build_call(expr->syntax, expr->type, method, thisArg, std::move(args));
}

FlirExpr* FlirLowerer::lower_construction(FhirConstructionExpr* expr)
{
    if (!expr->call || !expr->call->callee) return nullptr;

    auto* ctor = expr->call->callee->method;
    auto* type = expr->type;

    std::vector<FlirExpr*> args;
    args.reserve(expr->call->arguments.size());
    for (auto* a : expr->call->arguments)
        args.push_back(lower_expr(a));

    auto* named = type ? type->as<NamedTypeSymbol>() : nullptr;

    if (named && named->is_ref())
    {
        // Ref types allocate a heap handle, run the constructor through it, and yield the handle.
        auto* temp = builder.synthetic_local(currentMethod, "new", type);
        std::vector<FlirStmt*> sideEffects;
        sideEffects.push_back(builder.store(expr->syntax, builder.local_addr(expr->syntax, temp), builder.alloc_expr(expr->syntax, type)));
        auto* handle = builder.load(expr->syntax, type, builder.local_addr(expr->syntax, temp));
        sideEffects.push_back(builder.expr_stmt(expr->syntax, builder.call(expr->syntax, nullptr, ctor, handle, std::move(args))));
        return builder.sequence(expr->syntax, std::move(sideEffects), builder.load(expr->syntax, type, builder.local_addr(expr->syntax, temp)));
    }

    // Value types construct in place: the constructor writes through a temp's address, which is yielded.
    auto* temp = builder.synthetic_local(currentMethod, "new", type);
    std::vector<FlirStmt*> sideEffects;
    sideEffects.push_back(builder.expr_stmt(expr->syntax, builder.call(expr->syntax, nullptr, ctor, builder.local_addr(expr->syntax, temp), std::move(args))));
    return builder.sequence(expr->syntax, std::move(sideEffects), builder.local_addr(expr->syntax, temp));
}

FlirExpr* FlirLowerer::lower_assign(FhirAssignExpr* expr)
{
    BaseSyntax* syntax = expr->syntax;
    TypeSymbol* type = expr->type;

    std::vector<FlirStmt*> sideEffects;
    FlirExpr* loweredValue = lower_expr(expr->value);

    if (is_memory_class(type))
    {
        // Aggregate: stage into a temp so the value evaluates once, copy into the target, yield the temp.
        auto* tmp = builder.synthetic_local(currentMethod, "val", type);
        sideEffects.push_back(builder.copy(syntax, builder.local_addr(syntax, tmp), loweredValue, type));
        lower_store(expr->target, builder.local_addr(syntax, tmp), syntax, sideEffects);
        return builder.sequence(syntax, std::move(sideEffects), builder.local_addr(syntax, tmp));
    }

    auto* tmp = builder.synthetic_local(currentMethod, "val", type);
    sideEffects.push_back(builder.store(syntax, builder.local_addr(syntax, tmp), loweredValue));
    lower_store(expr->target, builder.load(syntax, type, builder.local_addr(syntax, tmp)), syntax, sideEffects);
    return builder.sequence(syntax, std::move(sideEffects), builder.load(syntax, type, builder.local_addr(syntax, tmp)));
}

FlirExpr* FlirLowerer::lower_compound_assign(FhirCompoundAssignExpr* expr)
{
    BaseSyntax* syntax = expr->syntax;
    TypeSymbol* type = expr->type;
    auto* binOp = expr->binaryOp;
    FhirExpr* targetExpr = expr->target();
    FhirExpr* valueExpr = expr->value();

    std::vector<FlirStmt*> sideEffects;
    FlirExpr* loweredValue = lower_expr(valueExpr);
    TypeSymbol* rhsType = valueExpr ? valueExpr->type : nullptr;

    if (auto* idx = targetExpr->as<FhirIndexExpr>())
    {
        TypeSymbol* elementType = idx->type;
        TypeSymbol* setterReturn = idx->setter ? idx->setter->get_return_type() : nullptr;
        TypeSymbol* objType = idx->object ? idx->object->type : nullptr;
        TypeSymbol* idxType = idx->index ? idx->index->type : nullptr;

        auto* tmpObj = builder.synthetic_local(currentMethod, "obj", objType);
        auto* tmpIdx = builder.synthetic_local(currentMethod, "idx", idxType);
        auto* tmpRhs = builder.synthetic_local(currentMethod, "rhs", rhsType);

        emit_assign(syntax, builder.local_addr(syntax, tmpObj), lower_expr(idx->object), objType, sideEffects);
        emit_assign(syntax, builder.local_addr(syntax, tmpIdx), lower_expr(idx->index), idxType, sideEffects);
        emit_assign(syntax, builder.local_addr(syntax, tmpRhs), loweredValue, rhsType, sideEffects);

        auto* current = build_call(syntax, elementType, idx->getter, nullptr,
            { read_slot(syntax, tmpObj), read_slot(syntax, tmpIdx) });
        auto* result = apply_bin(syntax, type, binOp, current, read_slot(syntax, tmpRhs));

        auto* tmpVal = builder.synthetic_local(currentMethod, "val", elementType);
        emit_assign(syntax, builder.local_addr(syntax, tmpVal), result, elementType, sideEffects);

        auto* setterCall = builder.call(syntax, setterReturn, idx->setter, nullptr,
            { read_slot(syntax, tmpObj), read_slot(syntax, tmpIdx), read_slot(syntax, tmpVal) });
        sideEffects.push_back(builder.expr_stmt(syntax, setterCall));

        return builder.sequence(syntax, std::move(sideEffects), read_slot(syntax, tmpVal));
    }

    if (auto* field = targetExpr->as<FhirFieldRefExpr>())
    {
        TypeSymbol* fieldType = field->symbol ? field->symbol->type : type;
        TypeSymbol* baseType = field->thisRef ? field->thisRef->type : nullptr;

        FlirLocal* tmpObj = nullptr;
        if (field->thisRef)
        {
            tmpObj = builder.synthetic_local(currentMethod, "obj", baseType);
            emit_assign(syntax, builder.local_addr(syntax, tmpObj), lower_expr(field->thisRef), baseType, sideEffects);
        }
        auto base_read = [&]() -> FlirExpr* { return tmpObj ? read_slot(syntax, tmpObj) : nullptr; };

        auto* tmpRhs = builder.synthetic_local(currentMethod, "rhs", rhsType);
        emit_assign(syntax, builder.local_addr(syntax, tmpRhs), loweredValue, rhsType, sideEffects);

        auto* current = address_load(syntax, builder.field_addr(syntax, base_read(), field->symbol), fieldType);
        auto* result = apply_bin(syntax, type, binOp, current, read_slot(syntax, tmpRhs));

        auto* tmpVal = builder.synthetic_local(currentMethod, "val", type);
        emit_assign(syntax, builder.local_addr(syntax, tmpVal), result, type, sideEffects);

        emit_assign(syntax, builder.field_addr(syntax, base_read(), field->symbol), read_slot(syntax, tmpVal), fieldType, sideEffects);

        return builder.sequence(syntax, std::move(sideEffects), read_slot(syntax, tmpVal));
    }

    auto* tmpRhs = builder.synthetic_local(currentMethod, "rhs", rhsType);
    emit_assign(syntax, builder.local_addr(syntax, tmpRhs), loweredValue, rhsType, sideEffects);

    auto* current = lower_expr(targetExpr);
    auto* result = apply_bin(syntax, type, binOp, current, read_slot(syntax, tmpRhs));

    auto* tmpVal = builder.synthetic_local(currentMethod, "val", type);
    emit_assign(syntax, builder.local_addr(syntax, tmpVal), result, type, sideEffects);

    lower_store(targetExpr, read_slot(syntax, tmpVal), syntax, sideEffects);

    return builder.sequence(syntax, std::move(sideEffects), read_slot(syntax, tmpVal));
}

FlirExpr* FlirLowerer::lower_cast(FhirCastExpr* expr)
{
    auto* operand = lower_expr(expr->operand);
    if (expr->method && !expr->method->is_intrinsic())
        return build_call(expr->syntax, expr->type, expr->method, nullptr, { operand });
    return builder.cast(expr->syntax, expr->type, operand);
}

FlirExpr* FlirLowerer::lower_index(FhirIndexExpr* expr)
{
    auto* object = lower_expr(expr->object);
    auto* index = lower_expr(expr->index);
    return build_call(expr->syntax, expr->type, expr->getter, nullptr, { object, index });
}

void FlirLowerer::lower_store(FhirExpr* target, FlirExpr* value, BaseSyntax* syntax, std::vector<FlirStmt*>& out)
{
    if (!target) return;

    if (auto* local = target->as<FhirLocalRefExpr>())
    {
        auto* slot = flir.lookup_local_symbol(local->symbol);
        emit_assign(syntax, builder.local_addr(syntax, slot), value, slot ? slot->type : nullptr, out);
        return;
    }
    if (auto* param = target->as<FhirParamRefExpr>())
    {
        auto* slot = flir.lookup_param_symbol(param->symbol);
        emit_assign(syntax, builder.local_addr(syntax, slot), value, slot ? slot->type : nullptr, out);
        return;
    }
    if (auto* field = target->as<FhirFieldRefExpr>())
    {
        auto* base = lower_expr(field->thisRef);
        auto* addr = builder.field_addr(syntax, base, field->symbol);
        emit_assign(syntax, addr, value, field->symbol ? field->symbol->type : nullptr, out);
        return;
    }
    if (auto* idx = target->as<FhirIndexExpr>(); idx && idx->setter)
    {
        auto* object = lower_expr(idx->object);
        auto* index = lower_expr(idx->index);
        TypeSymbol* setterReturn = idx->setter->get_return_type();
        auto* call = builder.call(syntax, setterReturn, idx->setter, nullptr, { object, index, value });
        out.push_back(builder.expr_stmt(syntax, call));
        return;
    }
}

// Lowers Foo { a = 1, b.c = 2 } to a sequence of field writes into a temp
FlirExpr* FlirLowerer::lower_object_builder(FhirObjectBuilderExpr* expr)
{
    BaseSyntax* syntax = expr->syntax;
    TypeSymbol* type = expr->type;

    auto* tmp = builder.local(currentMethod, "$init", type);

    std::vector<FlirStmt*> sideEffects;
    emit_assign(syntax, builder.local_addr(syntax, tmp), lower_expr(expr->construction), type, sideEffects);

    for (const auto& entry : expr->entries)
    {
        if (entry.path.empty() || !entry.value) continue;
        FlirExpr* addr = read_slot(syntax, tmp);
        for (size_t i = 0; i + 1 < entry.path.size(); ++i)
        {
            addr = builder.field_addr(syntax, addr, entry.path[i]);
            auto* interType = entry.path[i] ? entry.path[i]->type : nullptr;
            if (!is_memory_class(interType))
                addr = builder.load(syntax, interType, addr);
        }
        auto* value = lower_expr(entry.value);
        auto* fieldType = entry.path.back() ? entry.path.back()->type : nullptr;
        emit_assign(syntax, builder.field_addr(syntax, addr, entry.path.back()), value, fieldType, sideEffects);
    }

    return builder.sequence(syntax, std::move(sideEffects), read_slot(syntax, tmp));
}

// Lowers [a, b, c] to an alloc plus constructor, then one indexed setter per element
FlirExpr* FlirLowerer::lower_array_literal(FhirArrayLiteralExpr* expr)
{
    BaseSyntax* syntax = expr->syntax;
    TypeSymbol* type = expr->type;
    TypeSymbol* i32Type = semantic.resolve_type_name("i32");
    int count = static_cast<int>(expr->elements.size());

    auto* tmp = builder.local(currentMethod, "$arr", type);

    std::vector<FlirStmt*> sideEffects;
    sideEffects.push_back(builder.store(syntax, builder.local_addr(syntax, tmp), builder.alloc_expr(syntax, type)));

    auto* countConst = builder.constant(syntax, i32Type, ConstantValue::make_int(count));
    sideEffects.push_back(builder.expr_stmt(syntax, builder.call(syntax, nullptr, expr->ctor, read_slot(syntax, tmp), { countConst })));

    TypeSymbol* setterReturn = expr->setter ? expr->setter->get_return_type() : nullptr;
    for (int i = 0; i < count; ++i)
    {
        auto* indexConst = builder.constant(syntax, i32Type, ConstantValue::make_int(i));
        auto* value = lower_expr(expr->elements[i]);
        auto* setCall = builder.call(syntax, setterReturn, expr->setter, nullptr,
            { read_slot(syntax, tmp), indexConst, value });
        sideEffects.push_back(builder.expr_stmt(syntax, setCall));
    }

    return builder.sequence(syntax, std::move(sideEffects), read_slot(syntax, tmp));
}

#pragma region Local Helpers

}
