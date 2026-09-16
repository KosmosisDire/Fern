#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include <flir/flir.hpp>
#include <semantic/intrinsics.hpp>
#include <semantic/layout.hpp>
#include <source/span.hpp>
#include <vm/externs.hpp>
#include <vm/memory.hpp>
#include <vm/value.hpp>

namespace Fern
{

class Diagnostics;
struct FlirContext;
struct SemanticContext;
struct TypeSymbol;

#pragma region Control

// How a statement finished. Break unwinds the nearest loop, Return unwinds the whole method carrying an
// optional value (void returns carry none). Normal falls through to the next statement.
enum class ControlKind { Normal, Break, Return };

struct Control
{
    ControlKind kind = ControlKind::Normal;
    bool hasValue = false;
    Value value;

    static Control normal() { return Control{}; }
    static Control brk() { Control c; c.kind = ControlKind::Break; return c; }
    static Control ret() { Control c; c.kind = ControlKind::Return; return c; }
    static Control ret(Value v) { Control c; c.kind = ControlKind::Return; c.hasValue = true; c.value = v; return c; }
};

#pragma region Frame

// One activation record. base is the frame's stack address. byAddress maps the pointer slots (value
// this, value type params, sret) to the incoming addresses they alias. span tracks the running node so a
// runtime error and the backtrace report the right source location.
struct Frame
{
    FlirMethod* method = nullptr;
    uint64_t base = 0;
    std::unordered_map<const FlirLocal*, uint64_t> byAddress;
    Span span;
};

#pragma region Result

// One call stack level captured when a run errors. The method is the qualified name, the span is the
// node that was running. The path is left to the caller, which owns the file id to path mapping.
struct BacktraceEntry
{
    std::string method;
    Span span;
};

struct RunResult
{
    enum class Status { Completed, Errored, NoEntry };

    Status status = Status::NoEntry;
    TypeSymbol* returnType = nullptr;
    bool hasValue = false;
    Value value;
    std::string errorMessage;
    std::vector<BacktraceEntry> backtrace;
};

#pragma region Config

struct VmConfig
{
    uint64_t stackSize = 1024 * 1024;   // 1 MiB virtual stack
    uint64_t stepLimit = 100'000'000;   // hang protection for tests
    bool trace = false;
};

#pragma region Interpreter

class Interpreter
{
public:
    Interpreter(SemanticContext& semantic, FlirContext& flir, Diagnostics& diag, VmConfig config = {});

    // Finds Program.Main and runs it
    RunResult run_main();

    // Renders a completed result by its return type. Empty for void. Must run while this Interpreter is alive
    std::string format_result(const RunResult& result);

    // How a type flows as a value and how a value moves to and from bytes. Shared with ExternCalls.
    Value::Kind type_kind(TypeSymbol* type) const;
    Value load_scalar(uint64_t addr, Value::Kind kind);
    void store_scalar(uint64_t addr, Value value);

private:
    RunResult run(FlirMethod* main);
    FlirMethod* find_main();
    bool bind_externs();

    Value invoke(FlirMethod* callee, bool hasThis, Value thisVal,
                 const std::vector<Value>& args, bool hasResultDest, uint64_t resultDest);

    Control exec(FlirStmt* stmt);
    Control exec_block(FlirBlock* node);
    Control exec_store(FlirStore* node);
    Control exec_copy(FlirCopy* node);
    Control exec_expr_stmt(FlirExprStmt* node);
    Control exec_if(FlirIf* node);
    Control exec_loop(FlirLoop* node);
    Control exec_return(FlirReturn* node);

    Value eval(FlirExpr* expr);
    Value eval_const(FlirConst* node);
    Value eval_local_addr(FlirLocalAddr* node);
    Value eval_field_addr(FlirFieldAddr* node);
    Value eval_static_addr(FlirStaticAddr* node);
    Value eval_elem_addr(FlirElemAddr* node);
    Value eval_load(FlirLoad* node);
    Value eval_call(FlirCall* node);
    Value eval_alloc(FlirAlloc* node);
    Value eval_sequence(FlirSequence* node);
    Value eval_cast(FlirCast* node);

    // vm/intrinsics.cpp
    Value exec_intrinsic(FlirIntrinsic* node);
    Value convert(IntrinsicKind kind, Value operand);

    uint64_t intern_string(std::string_view text);
    std::string read_string(uint64_t handle);

    Frame& current_frame() { return frames.back(); }
    void set_span(FlirNode* node);
    void trace(FlirStmt* stmt);
    void step();
    bool is_void(TypeSymbol* type) const { return type == nullptr; }

    SemanticContext& semantic;
    FlirContext& flir;
    Diagnostics& diag;
    VmConfig config;
    const TargetInfo& target;
    VmMemory memory;
    ExternCalls externs;
    // The block holding every static field, allocated once when the interpreter is made
    uint64_t staticBase = 0;

    TypeSymbol* i8Type = nullptr;
    TypeSymbol* i16Type = nullptr;
    TypeSymbol* i32Type = nullptr;
    TypeSymbol* i64Type = nullptr;
    TypeSymbol* u8Type = nullptr;
    TypeSymbol* u16Type = nullptr;
    TypeSymbol* u32Type = nullptr;
    TypeSymbol* u64Type = nullptr;
    TypeSymbol* isizeType = nullptr;
    TypeSymbol* usizeType = nullptr;
    TypeSymbol* f16Type = nullptr;
    TypeSymbol* f32Type = nullptr;
    TypeSymbol* f64Type = nullptr;
    TypeSymbol* boolType = nullptr;
    TypeSymbol* c8Type = nullptr;
    TypeSymbol* stringType = nullptr;

    std::vector<Frame> frames;
    std::unordered_map<std::string, uint64_t> stringInterns;
    uint64_t stepCount = 0;
};

}
