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
    , layout(semantic, TargetInfo{})
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
            auto* thisSlot = builder.param(currentMethod, "this", parentType);
            thisSlot->byAddress = is_memory_class(parentType);
        }

        for (auto* param : method->symbol->parameters)
        {
            auto* slot = builder.param(currentMethod, param->name, param->type);
            slot->byAddress = is_memory_class(param->type);
            flir.slots[param] = slot;
        }

        // An aggregate return is written through a hidden by-address destination. A ref return hands
        // back the address of an existing place instead, so it never needs one.
        auto* returnType = method->symbol->get_return_type();
        if (is_memory_class(returnType) && !method->symbol->returnsRef)
        {
            currentMethod->sretParam = builder.slot("sret", returnType);
            currentMethod->sretParam->byAddress = true;
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

// Stages each aggregate argument into a fresh copy and passes the copy's address, so the callee
// cannot mutate the caller's value through a by-address parameter.
void FlirLowerer::caller_copy_args(BaseSyntax* syntax, std::vector<FlirExpr*>& args, std::vector<FlirStmt*>& out)
{
    for (auto*& arg : args)
    {
        if (!arg || !is_memory_class(arg->type)) continue;
        auto* temp = builder.synthetic_local(currentMethod, "arg", arg->type);
        out.push_back(builder.copy(syntax, builder.local_addr(syntax, temp), arg, arg->type));
        arg = builder.local_addr(syntax, temp);
    }
}

static bool is_layout_query(IntrinsicKind kind)
{
    return kind == IntrinsicKind::LayoutSize ||
           kind == IntrinsicKind::LayoutStride ||
           kind == IntrinsicKind::LayoutAlign;
}

// The single choke point for invoking a method. An intrinsic method becomes a FlirIntrinsic, never a
// call. A real call caller copies its aggregate arguments and routes an aggregate return through a
// temp destination, evaluating to that temp's address.
FlirExpr* FlirLowerer::build_call(BaseSyntax* syntax, TypeSymbol* retType, MethodSymbol* method, FlirExpr* thisArg, std::vector<FlirExpr*> args)
{
    if (method && is_layout_query(method->intrinsic()))
        return layout_query(syntax, retType, method, method->intrinsic());

    if (method && method->is_intrinsic())
        return builder.intrinsic(syntax, retType, method, thisArg, std::move(args));

    std::vector<FlirStmt*> pre;
    caller_copy_args(syntax, args, pre);

    auto* call = builder.call(syntax, retType, method, thisArg, std::move(args));

    // A ref returning call evaluates to the returned address itself, so it takes no result temp.
    if (is_memory_class(retType) && !(method && method->returnsRef))
    {
        auto* temp = builder.synthetic_local(currentMethod, "ret", retType);
        call->resultDest = builder.local_addr(syntax, temp);
        pre.push_back(builder.expr_stmt(syntax, call));
        return builder.sequence(syntax, std::move(pre), builder.local_addr(syntax, temp));
    }

    if (pre.empty()) return call;
    return builder.sequence(syntax, std::move(pre), call);
}

// Applies a binary operator through the call choke point, which routes intrinsics and user methods.
FlirExpr* FlirLowerer::apply_bin(BaseSyntax* syntax, TypeSymbol* type, FhirOpExpr* binaryOp, FlirExpr* lhs, FlirExpr* rhs)
{
    return build_call(syntax, type, binaryOp->method, nullptr, { lhs, rhs });
}

// A layout query folds here, after the layout pass, so no backend ever sees the intrinsic. The
// measured type is the carrier's argument, as in MemoryLayout<i32>.Size().
FlirExpr* FlirLowerer::layout_query(BaseSyntax* syntax, TypeSymbol* retType, MethodSymbol* method, IntrinsicKind kind)
{
    auto* carrier = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
    auto* measured = carrier && !carrier->typeArguments.empty()
        ? carrier->typeArguments[0]->as<NamedTypeSymbol>()
        : nullptr;

    int64_t value = 0;
    if (measured)
    {
        if (kind == IntrinsicKind::LayoutSize)        value = measured->sizeInBytes;
        else if (kind == IntrinsicKind::LayoutStride) value = measured->strideInBytes;
        else                                          value = measured->alignment;
    }
    return builder.constant(syntax, retType, ConstantValue::make_int(value));
}


// Finds an intrinsic method by kind on a type, for negations the lowerer synthesizes.
MethodSymbol* FlirLowerer::intrinsic_method(TypeSymbol* type, IntrinsicKind kind)
{
    auto* named = type ? type->as<NamedTypeSymbol>() : nullptr;
    if (!named) return nullptr;
    for (auto* method : named->methods)
        if (method && method->is_intrinsic() && method->intrinsic() == kind)
            return method;
    return nullptr;
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
    if (currentMethod->symbol && currentMethod->symbol->returnsRef)
    {
        out.push_back(builder.return_stmt(stmt->syntax, lower_place(stmt->value)));
        return;
    }
    if (currentMethod->sretParam)
    {
        auto* value = lower_expr(stmt->value);
        out.push_back(builder.copy(stmt->syntax, builder.local_addr(stmt->syntax, currentMethod->sretParam), value, currentMethod->sretParam->type));
        out.push_back(builder.return_stmt(stmt->syntax, nullptr));
        return;
    }
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
    auto* notCond = build_call(stmt->syntax, condType, intrinsic_method(condType, IntrinsicKind::BoolNot), nullptr, { cond });

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
    auto* fieldType = expr->symbol ? expr->symbol->type : nullptr;
    return address_load(expr->syntax, lower_place(expr), fieldType);
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

    return build_call(expr->syntax, expr->type, expr->method, nullptr, std::move(args));
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
        condition = build_call(syntax, type, intrinsic_method(type, IntrinsicKind::BoolNot), nullptr, { condition });

    sideEffects.push_back(builder.if_stmt(syntax, condition, thenBlock, nullptr));

    return builder.sequence(syntax, std::move(sideEffects), builder.load(syntax, type, builder.local_addr(syntax, tmp)));
}

// A ref returning call yields a place, which reads like any other place in value position.
FlirExpr* FlirLowerer::lower_call(FhirCallExpr* expr)
{
    auto* call = call_expr(expr);
    if (expr->is_place()) return address_load(expr->syntax, call, expr->type);
    return call;
}

FlirExpr* FlirLowerer::call_expr(FhirCallExpr* expr)
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

    if (ctor && ctor->is_intrinsic())
    {
        // An intrinsic constructor is the allocation itself, producing the constructed value directly.
        auto* temp = builder.synthetic_local(currentMethod, "new", type);
        std::vector<FlirStmt*> sideEffects;
        sideEffects.push_back(builder.store(expr->syntax, builder.local_addr(expr->syntax, temp), build_call(expr->syntax, type, ctor, nullptr, std::move(args))));
        return builder.sequence(expr->syntax, std::move(sideEffects), builder.load(expr->syntax, type, builder.local_addr(expr->syntax, temp)));
    }

    auto* named = type ? type->as<NamedTypeSymbol>() : nullptr;

    if (named && named->is_ref())
    {
        // Ref types allocate a heap handle, run the constructor through it, and yield the handle.
        auto* temp = builder.synthetic_local(currentMethod, "new", type);
        std::vector<FlirStmt*> sideEffects;
        sideEffects.push_back(builder.store(expr->syntax, builder.local_addr(expr->syntax, temp), builder.alloc_expr(expr->syntax, type)));
        auto* handle = builder.load(expr->syntax, type, builder.local_addr(expr->syntax, temp));
        sideEffects.push_back(builder.expr_stmt(expr->syntax, build_call(expr->syntax, nullptr, ctor, handle, std::move(args))));
        return builder.sequence(expr->syntax, std::move(sideEffects), builder.load(expr->syntax, type, builder.local_addr(expr->syntax, temp)));
    }

    // Value types construct in place: the constructor writes through a temp's address, which is yielded.
    auto* temp = builder.synthetic_local(currentMethod, "new", type);
    std::vector<FlirStmt*> sideEffects;
    sideEffects.push_back(builder.expr_stmt(expr->syntax, build_call(expr->syntax, nullptr, ctor, builder.local_addr(expr->syntax, temp), std::move(args))));
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
    TypeSymbol* rhsType = valueExpr ? valueExpr->type : nullptr;
    TypeSymbol* targetType = targetExpr ? targetExpr->type : type;
    auto* tmpVal = builder.synthetic_local(currentMethod, "val", type);

    // A value indexer has no place to write through, so the object and index are staged once and the
    // getter and setter both run on the staged temps.
    if (auto* idx = targetExpr->as<FhirIndexExpr>(); idx && !idx->is_place())
    {
        TypeSymbol* objType = idx->object ? idx->object->type : nullptr;
        TypeSymbol* idxType = idx->index ? idx->index->type : nullptr;

        auto* tmpObj = builder.synthetic_local(currentMethod, "obj", objType);
        auto* tmpIdx = builder.synthetic_local(currentMethod, "idx", idxType);
        auto* tmpRhs = builder.synthetic_local(currentMethod, "rhs", rhsType);

        emit_assign(syntax, builder.local_addr(syntax, tmpObj), lower_expr(idx->object), objType, sideEffects);
        emit_assign(syntax, builder.local_addr(syntax, tmpIdx), lower_expr(idx->index), idxType, sideEffects);
        emit_assign(syntax, builder.local_addr(syntax, tmpRhs), lower_expr(valueExpr), rhsType, sideEffects);

        auto* current = build_call(syntax, targetType, idx->getter, nullptr,
            { read_slot(syntax, tmpObj), read_slot(syntax, tmpIdx) });
        auto* result = apply_bin(syntax, type, binOp, current, read_slot(syntax, tmpRhs));
        emit_assign(syntax, builder.local_addr(syntax, tmpVal), result, type, sideEffects);

        auto* setterCall = build_call(syntax, idx->setter->get_return_type(), idx->setter, nullptr,
            { read_slot(syntax, tmpObj), read_slot(syntax, tmpIdx), read_slot(syntax, tmpVal) });
        sideEffects.push_back(builder.expr_stmt(syntax, setterCall));

        return builder.sequence(syntax, std::move(sideEffects), read_slot(syntax, tmpVal));
    }

    // Every other target is a place. Its address is taken once, so the target's operands run once, and
    // the place is then read, updated, and written back through that pointer.
    auto* tmpPtr = address_temp(targetType);
    sideEffects.push_back(builder.store(syntax, builder.local_addr(syntax, tmpPtr), lower_place(targetExpr)));

    auto* tmpRhs = builder.synthetic_local(currentMethod, "rhs", rhsType);
    emit_assign(syntax, builder.local_addr(syntax, tmpRhs), lower_expr(valueExpr), rhsType, sideEffects);

    auto* current = address_load(syntax, deref(syntax, tmpPtr, targetType), targetType);
    auto* result = apply_bin(syntax, type, binOp, current, read_slot(syntax, tmpRhs));
    emit_assign(syntax, builder.local_addr(syntax, tmpVal), result, type, sideEffects);
    emit_assign(syntax, deref(syntax, tmpPtr, targetType), read_slot(syntax, tmpVal), targetType, sideEffects);

    return builder.sequence(syntax, std::move(sideEffects), read_slot(syntax, tmpVal));
}

FlirExpr* FlirLowerer::lower_cast(FhirCastExpr* expr)
{
    // A widened constant still carries the narrower source type, so folding retypes it before it truncates
    auto* from = expr->operand && expr->operand->type ? expr->operand->type->as<NamedTypeSymbol>() : nullptr;
    auto* to = expr->type ? expr->type->as<NamedTypeSymbol>() : nullptr;
    if (from && to && from->is_integer() && to->is_integer())
    {
        const auto& folded = expr->get_constant();
        if (folded) return builder.constant(expr->syntax, expr->type, *folded);
    }

    auto* operand = lower_expr(expr->operand);
    if (expr->method && !expr->method->is_intrinsic())
        return build_call(expr->syntax, expr->type, expr->method, nullptr, { operand });
    return builder.cast(expr->syntax, expr->type, operand, expr->method);
}

// An index on a place getter reads the element place. A value getter stays a call.
FlirExpr* FlirLowerer::lower_index(FhirIndexExpr* expr)
{
    auto* object = lower_expr(expr->object);
    auto* index = lower_expr(expr->index);

    if (expr->is_place())
        return address_load(expr->syntax, index_place(expr->syntax, expr->getter, expr->type, object, index), expr->type);
    return build_call(expr->syntax, expr->type, expr->getter, nullptr, { object, index });
}

// A value indexer stores through its setter. Every other target is a place and is written directly.
void FlirLowerer::lower_store(FhirExpr* target, FlirExpr* value, BaseSyntax* syntax, std::vector<FlirStmt*>& out)
{
    if (!target) return;

    if (auto* idx = target->as<FhirIndexExpr>(); idx && !idx->is_place())
    {
        if (!idx->setter) return;
        auto* object = lower_expr(idx->object);
        auto* index = lower_expr(idx->index);
        auto* call = build_call(syntax, idx->setter->get_return_type(), idx->setter, nullptr, { object, index, value });
        out.push_back(builder.expr_stmt(syntax, call));
        return;
    }
    emit_assign(syntax, lower_place(target), value, target->type, out);
}

#pragma region Places

// The address of a place expression. The binder guarantees that assignment targets and ref returns are
// places, so anything else here is an aggregate that already evaluates to its address.
FlirExpr* FlirLowerer::lower_place(FhirExpr* expr)
{
    if (!expr) return nullptr;

    if (auto* e = expr->as<FhirLocalRefExpr>())
        return builder.local_addr(e->syntax, flir.lookup_local_symbol(e->symbol));
    if (auto* e = expr->as<FhirParamRefExpr>())
        return builder.local_addr(e->syntax, flir.lookup_param_symbol(e->symbol));
    if (auto* e = expr->as<FhirThisExpr>())
    {
        if (!currentMethod || currentMethod->parameters.empty()) return nullptr;
        return builder.local_addr(e->syntax, currentMethod->parameters[0]);
    }
    if (auto* e = expr->as<FhirFieldRefExpr>())
        return builder.field_addr(e->syntax, lower_expr(e->thisRef), e->symbol);
    if (auto* e = expr->as<FhirIndexExpr>())
        return index_place(e->syntax, e->getter, e->type, lower_expr(e->object), lower_expr(e->index));
    if (auto* e = expr->as<FhirCallExpr>())
        return call_expr(e);

    return lower_expr(expr);
}

// The address of an indexed place. Ptr indexing is the one intrinsic and is element arithmetic, any
// other place getter is a call that yields the address it returns.
FlirExpr* FlirLowerer::index_place(BaseSyntax* syntax, MethodSymbol* getter, TypeSymbol* elemType, FlirExpr* object, FlirExpr* index)
{
    if (getter && getter->intrinsic() == IntrinsicKind::PtrIndex)
        return builder.elem_addr(syntax, object, index, elemType);
    return build_call(syntax, elemType, getter, nullptr, { object, index });
}

// Ptr<pointee>, instantiated and laid out on demand since lowering runs after the layout pass.
TypeSymbol* FlirLowerer::pointer_type(TypeSymbol* pointee)
{
    auto* inst = semantic.symbols.get_or_declare_pointer_type(pointee);
    if (inst) layout.compute(inst);
    return inst;
}

// A frame slot that holds the address of a place, so the place's operands run once and the place can
// be read and written more than once.
FlirLocal* FlirLowerer::address_temp(TypeSymbol* pointee)
{
    return builder.synthetic_local(currentMethod, "ptr", pointer_type(pointee));
}

// The place an address temp points at, spelled as element zero so it stays an address node.
FlirExpr* FlirLowerer::deref(BaseSyntax* syntax, FlirLocal* ptrSlot, TypeSymbol* type)
{
    auto* zero = builder.constant(syntax, semantic.resolve_type_name("i32"), ConstantValue::make_int(0));
    return builder.elem_addr(syntax, read_slot(syntax, ptrSlot), zero, type);
}

#pragma region Builders

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

// Lowers [a, b, c] to an alloc plus constructor, then one element store per element, through the
// element place when the array has a place getter and through the setter otherwise
FlirExpr* FlirLowerer::lower_array_literal(FhirArrayLiteralExpr* expr)
{
    BaseSyntax* syntax = expr->syntax;
    TypeSymbol* type = expr->type;
    TypeSymbol* i32Type = semantic.resolve_type_name("i32");
    int count = static_cast<int>(expr->elements.size());

    auto* tmp = builder.local(currentMethod, "$arr", type);

    std::vector<FlirStmt*> sideEffects;
    auto* countConst = builder.constant(syntax, i32Type, ConstantValue::make_int(count));
    sideEffects.push_back(builder.store(syntax, builder.local_addr(syntax, tmp), build_call(syntax, type, expr->ctor, nullptr, { countConst })));

    for (int i = 0; i < count; ++i)
    {
        auto* indexConst = builder.constant(syntax, i32Type, ConstantValue::make_int(i));
        auto* value = lower_expr(expr->elements[i]);
        if (expr->getter)
        {
            auto* place = index_place(syntax, expr->getter, expr->elementType, read_slot(syntax, tmp), indexConst);
            emit_assign(syntax, place, value, expr->elementType, sideEffects);
        }
        else
        {
            auto* setCall = build_call(syntax, expr->setter->get_return_type(), expr->setter, nullptr,
                { read_slot(syntax, tmp), indexConst, value });
            sideEffects.push_back(builder.expr_stmt(syntax, setCall));
        }
    }

    return builder.sequence(syntax, std::move(sideEffects), read_slot(syntax, tmp));
}

#pragma region Local Helpers

}
