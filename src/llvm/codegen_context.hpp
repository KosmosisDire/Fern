// codegen_context.hpp - Unified Code Generation Context
#pragma once

#include "flir/flir.hpp"
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DataLayout.h>
#include <unordered_map>
#include <string>
#include <stack>
#include <vector>

namespace Fern
{

// Loop context for break/continue targeting
struct LoopContext
{
    llvm::BasicBlock* continue_bb;  // Where 'continue' should jump
    llvm::BasicBlock* break_bb;     // Where 'break' should jump
};

// Block context for WASM-style Br/BrIf
struct BlockContext
{
    llvm::BasicBlock* exit_bb;
    bool is_loop;
};

class CodeGenContext
{
private:
    llvm::LLVMContext& context;
    llvm::Module& llvm_module;
    llvm::IRBuilder<>& builder;

    // Module-level caches (persist across functions)
    std::unordered_map<FLIR::IRTypePtr, llvm::Type*> type_cache;
    std::unordered_map<FLIR::IRStruct*, llvm::StructType*> struct_cache;
    std::unordered_map<FLIR::Function*, llvm::Function*> function_cache;

    // Function-level state (cleared per function)
    std::unordered_map<FLIR::Value*, llvm::Value*> value_map;
    FLIR::Function* current_flir_func = nullptr;
    llvm::Function* current_llvm_func = nullptr;

    // Loop stack for break/continue targeting
    std::stack<LoopContext> loop_stack;

    // Block stack for Br/BrIf depth targeting
    std::vector<BlockContext> block_stack;

public:
    CodeGenContext(llvm::LLVMContext& ctx, llvm::Module& mod, llvm::IRBuilder<>& bldr)
        : context(ctx), llvm_module(mod), builder(bldr) {}

    #pragma region Module Setup

    void declare_types(FLIR::Module* flir_module);
    void declare_functions(FLIR::Module* flir_module);

    #pragma region Type Management

    llvm::Type* get_type(FLIR::IRTypePtr type);
    llvm::StructType* get_struct_type(FLIR::IRStruct* ir_struct);

    #pragma region Function Management

    llvm::Function* declare_function(FLIR::Function* flir_func);
    llvm::Function* get_function(FLIR::Function* flir_func);

    void begin_function(FLIR::Function* flir_func, llvm::Function* llvm_func);
    void end_function();

    #pragma region Value Management

    llvm::Value* get_value(FLIR::Value* flir_value);
    void map_value(FLIR::Value* flir_value, llvm::Value* llvm_value);

    void map_parameters();

    #pragma region Loop Stack Management

    void push_loop(llvm::BasicBlock* continue_bb, llvm::BasicBlock* break_bb);
    void pop_loop();
    LoopContext& current_loop();
    bool in_loop() const { return !loop_stack.empty(); }

    #pragma region Block Stack Management

    void push_block(llvm::BasicBlock* exit_bb, bool is_loop = false);
    void pop_block();
    llvm::BasicBlock* get_break_target(uint32_t depth);

    #pragma region IR Generation Helpers

    llvm::BasicBlock* create_block(const std::string& name);
    llvm::Value* create_entry_alloca(llvm::Type* type, const std::string& name = "");
    llvm::Value* create_malloc(llvm::Type* type, const std::string& name = "");
    llvm::Value* create_malloc_bytes(llvm::Value* size, const std::string& name = "");
    void create_free(llvm::Value* ptr);
    llvm::Value* create_global_string(const std::string& str, const std::string& name = ".str");

    #pragma region Accessors

    llvm::LLVMContext& ctx() { return context; }
    llvm::Module& module() { return llvm_module; }
    llvm::IRBuilder<>& ir() { return builder; }

    FLIR::Function* flir_func() { return current_flir_func; }
    llvm::Function* llvm_func() { return current_llvm_func; }

    bool is_float(FLIR::IRTypePtr type) const { return type && type->is_float(); }
    bool is_signed(FLIR::IRTypePtr type) const { return type && type->is_int() && type->is_signed; }
    bool is_integer(FLIR::IRTypePtr type) const { return type && type->is_int(); }
    bool is_pointer(FLIR::IRTypePtr type) const { return type && type->is_pointer(); }

private:
    llvm::FunctionType* get_function_type(FLIR::Function* flir_func);
    std::string format_value_error(FLIR::Value* value, const std::string& message);
};

} // namespace Fern
