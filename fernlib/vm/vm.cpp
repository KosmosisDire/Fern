#include <vm/vm.hpp>

#include <cstring>
#include <format>

#include <logger.hpp>

#include <ast/ast.hpp>
#include <common/diagnostic.hpp>
#include <common/float16.hpp>
#include <flir/context.hpp>
#include <flir/fmt.hpp>
#include <semantic/context.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

#pragma region Construction

Interpreter::Interpreter(SemanticContext& semantic, FlirContext& flir, Diagnostics& diag, VmConfig config)
    : semantic(semantic)
    , flir(flir)
    , diag(diag)
    , config(config)
    , target()
    , memory(config.stackSize)
{
    i8Type = semantic.resolve_type_name("i8");
    i16Type = semantic.resolve_type_name("i16");
    i32Type = semantic.resolve_type_name("i32");
    i64Type = semantic.resolve_type_name("i64");
    u8Type = semantic.resolve_type_name("u8");
    u16Type = semantic.resolve_type_name("u16");
    u32Type = semantic.resolve_type_name("u32");
    u64Type = semantic.resolve_type_name("u64");
    f16Type = semantic.resolve_type_name("f16");
    f32Type = semantic.resolve_type_name("f32");
    f64Type = semantic.resolve_type_name("f64");
    boolType = semantic.resolve_type_name("bool");
    c8Type = semantic.resolve_type_name("c8");
    stringType = semantic.resolve_type_name("string");
}

#pragma region Entry

FlirMethod* Interpreter::find_main()
{
    // The entry is Program.Main, wherever Program lives, so search all types rather than one namespace.
    Span nearMiss{};
    for (auto* type : semantic.symbols.allTypes)
    {
        if (!type || type->name != "Program") continue;

        for (auto* method : type->methods)
        {
            if (method->name != "Main") continue;
            // Remember a wrong shaped Main so the error can point at it
            if (method->syntax) nearMiss = method->syntax->span;
            if (!has_modifier(method->modifiers, Modifier::Static)) continue;
            if (!method->parameters.empty()) continue;

            auto it = flir.loweredMethods.find(method);
            if (it != flir.loweredMethods.end()) return it->second;
        }
    }

    diag.report(DiagnosticCode::Err_NoEntryPoint, nearMiss);
    return nullptr;
}

RunResult Interpreter::run_main()
{
    FlirMethod* main = find_main();
    if (!main) return RunResult{};
    return run(main);
}

RunResult Interpreter::run(FlirMethod* main)
{
    RunResult result;
    result.returnType = main->symbol ? main->symbol->get_return_type() : nullptr;

    try
    {
        std::vector<Value> noArgs;
        Value value = invoke(main, false, Value{}, noArgs, false, 0);
        result.status = RunResult::Status::Completed;
        if (!is_void(result.returnType))
        {
            result.hasValue = true;
            result.value = value;
        }
    }
    catch (const VmError& error)
    {
        result.status = RunResult::Status::Errored;
        result.errorMessage = error.message;

        Span span = frames.empty() ? Span{} : frames.back().span;
        diag.report(DiagnosticCode::Err_RuntimeError, span, error.message);

        for (auto it = frames.rbegin(); it != frames.rend(); ++it)
        {
            std::string name = it->method && it->method->symbol
                ? it->method->symbol->qualified_name()
                : "?";
            result.backtrace.push_back(BacktraceEntry{std::move(name), it->span});
        }
        frames.clear();
    }
    return result;
}

#pragma region Result Formatting

std::string Interpreter::format_result(const RunResult& result)
{
    if (!result.hasValue) return "";

    const Value& value = result.value;
    if (result.returnType == stringType)
    {
        return read_string(value.as_addr());
    }

    switch (value.kind)
    {
        case Value::Kind::I8:   return std::to_string(value.as_i8());
        case Value::Kind::I16:  return std::to_string(value.as_i16());
        case Value::Kind::I32:  return std::to_string(value.as_i32());
        case Value::Kind::I64:  return std::to_string(value.as_i64());
        case Value::Kind::U8:   return std::to_string(value.as_u8());
        case Value::Kind::U16:  return std::to_string(value.as_u16());
        case Value::Kind::U32:  return std::to_string(value.as_u32());
        case Value::Kind::U64:  return std::to_string(value.as_u64());
        case Value::Kind::F16:  return std::format("{}", f16_to_float(value.as_f16()));
        case Value::Kind::F32:  return std::format("{}", value.as_f32());
        case Value::Kind::F64:  return std::format("{}", value.as_f64());
        case Value::Kind::Bool: return value.as_bool() ? "true" : "false";
        case Value::Kind::C8:   return std::string(1, static_cast<char>(value.as_c8()));
        case Value::Kind::Addr: return std::format("{}", value.as_addr());
    }
    return "";
}

#pragma region Calls

Value Interpreter::invoke(FlirMethod* callee, bool hasThis, Value thisVal,
                          const std::vector<Value>& args, bool hasResultDest, uint64_t resultDest)
{
    uint64_t base = memory.stack_alloc(static_cast<uint64_t>(callee->frameSize));

    frames.push_back(Frame{});
    {
        Frame& frame = frames.back();
        frame.method = callee;
        frame.base = base;

        auto bind = [&](FlirLocal* slot, const Value& value)
        {
            if (!slot) return;
            if (slot->byAddress) frame.byAddress[slot] = value.as_addr();
            else store_scalar(base + slot->offset, value);
        };

        size_t i = 0;
        if (hasThis && i < callee->parameters.size()) bind(callee->parameters[i++], thisVal);
        for (const auto& arg : args)
        {
            if (i >= callee->parameters.size()) break;
            bind(callee->parameters[i++], arg);
        }

        if (callee->sretParam && hasResultDest)
            frame.byAddress[callee->sretParam] = resultDest;
    }

    Control control = exec(callee->body);

    frames.pop_back();
    memory.stack_restore(base);

    if (control.kind == ControlKind::Return && control.hasValue)
        return control.value;
    return Value{};
}

#pragma region Statements

Control Interpreter::exec(FlirStmt* stmt)
{
    if (!stmt) return Control::normal();
    step();
    set_span(stmt);
    trace(stmt);

    if (auto* s = stmt->as<FlirBlock>())    return exec_block(s);
    if (auto* s = stmt->as<FlirStore>())    return exec_store(s);
    if (auto* s = stmt->as<FlirCopy>())     return exec_copy(s);
    if (auto* s = stmt->as<FlirExprStmt>()) return exec_expr_stmt(s);
    if (auto* s = stmt->as<FlirIf>())       return exec_if(s);
    if (auto* s = stmt->as<FlirLoop>())     return exec_loop(s);
    if (stmt->is<FlirBreak>())              return Control::brk();
    if (auto* s = stmt->as<FlirReturn>())   return exec_return(s);
    return Control::normal();
}

Control Interpreter::exec_block(FlirBlock* node)
{
    for (auto* stmt : node->statements)
    {
        Control control = exec(stmt);
        if (control.kind != ControlKind::Normal) return control;
    }
    return Control::normal();
}

Control Interpreter::exec_store(FlirStore* node)
{
    uint64_t addr = eval(node->address).as_addr();
    Value value = eval(node->value);
    store_scalar(addr, value);
    return Control::normal();
}

Control Interpreter::exec_copy(FlirCopy* node)
{
    uint64_t dest = eval(node->dest).as_addr();
    uint64_t src = eval(node->src).as_addr();
    auto* named = node->type ? node->type->as<NamedTypeSymbol>() : nullptr;
    uint64_t size = named ? static_cast<uint64_t>(named->strideInBytes) : 0;
    memory.copy(dest, src, size);
    return Control::normal();
}

Control Interpreter::exec_expr_stmt(FlirExprStmt* node)
{
    eval(node->expression);
    return Control::normal();
}

Control Interpreter::exec_if(FlirIf* node)
{
    if (eval(node->condition).as_bool())
        return exec(node->thenBlock);
    if (node->elseBlock)
        return exec(node->elseBlock);
    return Control::normal();
}

Control Interpreter::exec_loop(FlirLoop* node)
{
    while (true)
    {
        Control control = exec(node->body);
        if (control.kind == ControlKind::Break) return Control::normal();
        if (control.kind == ControlKind::Return) return control;
    }
}

Control Interpreter::exec_return(FlirReturn* node)
{
    if (node->value) return Control::ret(eval(node->value));
    return Control::ret();
}

#pragma region Expressions

Value Interpreter::eval(FlirExpr* expr)
{
    if (!expr) return Value{};
    set_span(expr);

    if (auto* e = expr->as<FlirConst>())     return eval_const(e);
    if (auto* e = expr->as<FlirLocalAddr>()) return eval_local_addr(e);
    if (auto* e = expr->as<FlirFieldAddr>()) return eval_field_addr(e);
    if (auto* e = expr->as<FlirElemAddr>())  return eval_elem_addr(e);
    if (auto* e = expr->as<FlirLoad>())      return eval_load(e);
    if (auto* e = expr->as<FlirCall>())      return eval_call(e);
    if (auto* e = expr->as<FlirIntrinsic>()) return exec_intrinsic(e);
    if (auto* e = expr->as<FlirCast>())      return eval_cast(e);
    if (auto* e = expr->as<FlirAlloc>())     return eval_alloc(e);
    if (auto* e = expr->as<FlirSequence>())  return eval_sequence(e);
    return Value{};
}

Value Interpreter::eval_const(FlirConst* node)
{
    const ConstantValue& cv = node->value;
    switch (type_kind(node->type))
    {
        case Value::Kind::I8:   return Value::make_i8(static_cast<int8_t>(cv.intValue));
        case Value::Kind::I16:  return Value::make_i16(static_cast<int16_t>(cv.intValue));
        case Value::Kind::I32:  return Value::make_i32(static_cast<int32_t>(cv.intValue));
        case Value::Kind::I64:  return Value::make_i64(cv.intValue);
        case Value::Kind::U8:   return Value::make_u8(static_cast<uint8_t>(cv.intValue));
        case Value::Kind::U16:  return Value::make_u16(static_cast<uint16_t>(cv.intValue));
        case Value::Kind::U32:  return Value::make_u32(static_cast<uint32_t>(cv.intValue));
        case Value::Kind::U64:  return Value::make_u64(static_cast<uint64_t>(cv.intValue));
        case Value::Kind::C8:   return Value::make_c8(static_cast<uint8_t>(cv.intValue));
        case Value::Kind::Bool: return Value::make_bool(cv.boolValue);
        case Value::Kind::F16:
            return Value::make_f16(f16_from_double(cv.kind == ConstantValue::Kind::Float
                ? cv.floatValue
                : static_cast<double>(cv.intValue)));
        case Value::Kind::F32:
            return Value::make_f32(cv.kind == ConstantValue::Kind::Float
                ? static_cast<float>(cv.floatValue)
                : static_cast<float>(cv.intValue));
        case Value::Kind::F64:
            return Value::make_f64(cv.kind == ConstantValue::Kind::Float
                ? cv.floatValue
                : static_cast<double>(cv.intValue));
        case Value::Kind::Addr: return Value::make_addr(intern_string(cv.stringValue));
    }
    return Value{};
}

Value Interpreter::eval_local_addr(FlirLocalAddr* node)
{
    FlirLocal* local = node->local;
    Frame& frame = current_frame();
    if (local->byAddress)
    {
        auto it = frame.byAddress.find(local);
        if (it == frame.byAddress.end())
            throw VmError{std::format("by-address slot '{}' has no incoming pointer", local->name)};
        return Value::make_addr(it->second);
    }
    if (local->offset < 0)
        throw VmError{std::format("local '{}' has no frame offset", local->name)};
    return Value::make_addr(frame.base + local->offset);
}

Value Interpreter::eval_field_addr(FlirFieldAddr* node)
{
    uint64_t base = eval(node->base).as_addr();
    return Value::make_addr(base + node->field->offset);
}

Value Interpreter::eval_elem_addr(FlirElemAddr* node)
{
    uint64_t base = eval(node->base).as_addr();
    int64_t index = eval(node->index).as_i32();

    auto* elem = node->elemType ? node->elemType->as<NamedTypeSymbol>() : nullptr;
    int64_t stride = elem ? elem->strideInBytes : 0;
    return Value::make_addr(base + static_cast<uint64_t>(index * stride));
}

Value Interpreter::eval_load(FlirLoad* node)
{
    uint64_t addr = eval(node->address).as_addr();
    return load_scalar(addr, type_kind(node->type));
}

Value Interpreter::eval_call(FlirCall* node)
{
    bool hasThis = node->thisArg != nullptr;
    Value thisVal;
    if (hasThis) thisVal = eval(node->thisArg);

    std::vector<Value> args;
    args.reserve(node->args.size());
    for (auto* arg : node->args)
        args.push_back(eval(arg));

    bool hasResultDest = node->resultDest != nullptr;
    uint64_t resultDest = 0;
    if (hasResultDest) resultDest = eval(node->resultDest).as_addr();

    auto it = flir.loweredMethods.find(node->method);
    if (it == flir.loweredMethods.end() || !it->second)
    {
        std::string_view name = node->method
            ? std::string_view(node->method->name)
            : std::string_view("?");
        throw VmError{std::format("missing body for method '{}'", name)};
    }

    return invoke(it->second, hasThis, thisVal, args, hasResultDest, resultDest);
}

Value Interpreter::eval_alloc(FlirAlloc* node)
{
    auto* named = node->allocType ? node->allocType->as<NamedTypeSymbol>() : nullptr;
    uint64_t size = named ? static_cast<uint64_t>(named->payloadSize) : 0;
    return Value::make_addr(memory.alloc(size, node->allocType));
}

Value Interpreter::eval_sequence(FlirSequence* node)
{
    // Lowered sequences never carry control out of their side effects, so the returned control is ignored.
    for (auto* effect : node->sideEffects)
        exec(effect);
    return eval(node->value);
}

Value Interpreter::eval_cast(FlirCast* node)
{
    Value operand = eval(node->operand);
    IntrinsicKind kind = node->method ? node->method->intrinsic() : IntrinsicKind::None;
    return convert(kind, operand);
}

#pragma region Helpers

Value::Kind Interpreter::type_kind(TypeSymbol* type) const
{
    if (type == i8Type)   return Value::Kind::I8;
    if (type == i16Type)  return Value::Kind::I16;
    if (type == i32Type)  return Value::Kind::I32;
    if (type == i64Type)  return Value::Kind::I64;
    if (type == u8Type)   return Value::Kind::U8;
    if (type == u16Type)  return Value::Kind::U16;
    if (type == u32Type)  return Value::Kind::U32;
    if (type == u64Type)  return Value::Kind::U64;
    if (type == f16Type)  return Value::Kind::F16;
    if (type == f32Type)  return Value::Kind::F32;
    if (type == f64Type)  return Value::Kind::F64;
    if (type == boolType) return Value::Kind::Bool;
    if (type == c8Type)   return Value::Kind::C8;
    // Strings, arrays, and every ref type flow as a one word handle.
    return Value::Kind::Addr;
}

Value Interpreter::load_scalar(uint64_t addr, Value::Kind kind)
{
    switch (kind)
    {
        case Value::Kind::I8:   return Value::make_i8(static_cast<int8_t>(memory.read_u8(addr)));
        case Value::Kind::I16:  return Value::make_i16(static_cast<int16_t>(memory.read_u16(addr)));
        case Value::Kind::I32:  return Value::make_i32(static_cast<int32_t>(memory.read_u32(addr)));
        case Value::Kind::I64:  return Value::make_i64(static_cast<int64_t>(memory.read_u64(addr)));
        case Value::Kind::U8:   return Value::make_u8(memory.read_u8(addr));
        case Value::Kind::U16:  return Value::make_u16(memory.read_u16(addr));
        case Value::Kind::U32:  return Value::make_u32(memory.read_u32(addr));
        case Value::Kind::U64:  return Value::make_u64(memory.read_u64(addr));
        case Value::Kind::F16:  return Value::make_f16(memory.read_u16(addr));
        case Value::Kind::F32:
        {
            uint32_t bits = memory.read_u32(addr);
            float value;
            std::memcpy(&value, &bits, 4);
            return Value::make_f32(value);
        }
        case Value::Kind::F64:
        {
            uint64_t bits = memory.read_u64(addr);
            double value;
            std::memcpy(&value, &bits, 8);
            return Value::make_f64(value);
        }
        case Value::Kind::Bool: return Value::make_bool(memory.read_u8(addr) != 0);
        case Value::Kind::C8:   return Value::make_c8(memory.read_u8(addr));
        case Value::Kind::Addr: return Value::make_addr(memory.read_u64(addr));
    }
    return Value{};
}

void Interpreter::store_scalar(uint64_t addr, Value value)
{
    switch (value.kind)
    {
        case Value::Kind::I8:   memory.write_u8(addr, static_cast<uint8_t>(value.as_i8())); break;
        case Value::Kind::I16:  memory.write_u16(addr, static_cast<uint16_t>(value.as_i16())); break;
        case Value::Kind::I32:  memory.write_u32(addr, static_cast<uint32_t>(value.as_i32())); break;
        case Value::Kind::I64:  memory.write_u64(addr, static_cast<uint64_t>(value.as_i64())); break;
        case Value::Kind::U8:   memory.write_u8(addr, value.as_u8()); break;
        case Value::Kind::U16:  memory.write_u16(addr, value.as_u16()); break;
        case Value::Kind::U32:  memory.write_u32(addr, value.as_u32()); break;
        case Value::Kind::U64:  memory.write_u64(addr, value.as_u64()); break;
        case Value::Kind::F16:  memory.write_u16(addr, value.as_f16()); break;
        case Value::Kind::F32:
        {
            float raw = value.as_f32();
            uint32_t bits;
            std::memcpy(&bits, &raw, 4);
            memory.write_u32(addr, bits);
            break;
        }
        case Value::Kind::F64:
        {
            double raw = value.as_f64();
            uint64_t bits;
            std::memcpy(&bits, &raw, 8);
            memory.write_u64(addr, bits);
            break;
        }
        case Value::Kind::Bool: memory.write_u8(addr, value.as_bool() ? 1 : 0); break;
        case Value::Kind::C8:   memory.write_u8(addr, value.as_c8()); break;
        case Value::Kind::Addr: memory.write_u64(addr, value.as_addr()); break;
    }
}

std::string Interpreter::read_string(uint64_t handle)
{
    uint32_t length = memory.read_u32(handle);
    std::string text(length, '\0');
    if (length > 0)
        std::memcpy(text.data(), memory.host_ptr(handle + target.blockHeaderSize, length), length);
    return text;
}

uint64_t Interpreter::intern_string(std::string_view text)
{
    std::string key(text);
    auto it = stringInterns.find(key);
    if (it != stringInterns.end()) return it->second;

    uint64_t size = static_cast<uint64_t>(target.blockHeaderSize) + text.size() + 1;
    uint64_t addr = memory.alloc(size, stringType);
    memory.write_u32(addr, static_cast<uint32_t>(text.size()));
    if (!text.empty())
        memory.write_bytes(addr + target.blockHeaderSize, text.data(), text.size());
    memory.write_u8(addr + target.blockHeaderSize + text.size(), 0);

    stringInterns[key] = addr;
    return addr;
}

void Interpreter::set_span(FlirNode* node)
{
    if (frames.empty() || !node) return;
    const Span& span = node->span;
    // Synthetic nodes carry a default span, keep the last real location instead.
    if (span.startLine == 0 && span.startColumn == 0 && span.endLine == 0 && span.endColumn == 0) return;
    frames.back().span = span;
}

void Interpreter::trace(FlirStmt* stmt)
{
    if (!config.trace || !stmt) return;
    // Blocks, ifs, and loops are control structure. Tracing only the leaf statements gives a linear
    // record of the operations actually run, in order, without repeating nested bodies.
    if (stmt->is<FlirBlock>() || stmt->is<FlirIf>() || stmt->is<FlirLoop>()) return;
    LOG(LogChannel::Debug) << std::format("[line {}] {}", stmt->span.startLine + 1, FlirPrettyFormatter::format(stmt));
}

void Interpreter::step()
{
    if (++stepCount > config.stepLimit)
        throw VmError{"step limit exceeded"};
}

}
