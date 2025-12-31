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

namespace Fern
{

class CodeGenContext
{
private:
    llvm::LLVMContext& context;
    llvm::Module& module;
    llvm::IRBuilder<>& builder;

    // Module-level caches (persist across functions)
    std::unordered_map<FLIR::IRTypePtr, llvm::Type*> type_cache;
    std::unordered_map<FLIR::IRStruct*, llvm::StructType*> struct_cache;
    std::unordered_map<FLIR::Function*, llvm::Function*> function_cache;

    // Function-level state (cleared per function)
    std::unordered_map<FLIR::Value*, llvm::Value*> value_map;
    std::unordered_map<FLIR::BasicBlock*, llvm::BasicBlock*> block_map;
    FLIR::Function* current_flir_func = nullptr;
    llvm::Function* current_llvm_func = nullptr;

public:
    CodeGenContext(llvm::LLVMContext& ctx, llvm::Module& mod, llvm::IRBuilder<>& bldr)
        : context(ctx), module(mod), builder(bldr) {}

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

    #pragma region Value/Block Management

    llvm::Value* get_value(FLIR::Value* flir_value);
    void map_value(FLIR::Value* flir_value, llvm::Value* llvm_value);

    llvm::BasicBlock* get_block(FLIR::BasicBlock* flir_block);
    void map_block(FLIR::BasicBlock* flir_block, llvm::BasicBlock* llvm_block);

    void create_all_blocks();
    void map_parameters();

    #pragma region IR Generation Helpers

    llvm::Value* create_entry_alloca(llvm::Type* type, const std::string& name = "");
    llvm::Value* create_malloc(llvm::Type* type, const std::string& name = "");
    llvm::Value* create_malloc_bytes(llvm::Value* size, const std::string& name = "");
    void create_free(llvm::Value* ptr);
    llvm::Value* create_global_string(const std::string& str, const std::string& name = ".str");

    #pragma region Accessors

    llvm::LLVMContext& ctx() { return context; }
    llvm::Module& mod() { return module; }
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
    std::string format_block_error(FLIR::BasicBlock* block, const std::string& message);
};

} // namespace Fern
