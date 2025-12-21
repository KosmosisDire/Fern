// codegen_function.hpp - Function-Level Code Generation Context
#pragma once

#include "codegen_module.hpp"
#include "llvm_ir_builder.hpp"
#include "flir/flir.hpp"
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <unordered_map>
#include <vector>
#include <string>

namespace Fern
{
    /**
     * @brief Manages function-level code generation state
     *
     * Handles value mappings, block mappings, and function-local state.
     * Created and destroyed for each function being lowered.
     */
    class CodeGenFunction
    {
    private:
        CodeGenModule& CGM;
        LLVMIRBuilder& ir_builder;

        FLIR::Function* flir_function;
        llvm::Function* llvm_function;

        // Value and block mappings
        std::unordered_map<FLIR::Value*, llvm::Value*> value_map;
        std::unordered_map<FLIR::BasicBlock*, llvm::BasicBlock*> block_map;

    public:
        CodeGenFunction(CodeGenModule& cgm, LLVMIRBuilder& builder,
                       FLIR::Function* flir_func, llvm::Function* llvm_func)
            : CGM(cgm), ir_builder(builder),
              flir_function(flir_func), llvm_function(llvm_func) {}

        #pragma region Value Management
        llvm::Value* get_value(FLIR::Value* flir_value);
        void map_value(FLIR::Value* flir_value, llvm::Value* llvm_value);
        bool has_value(FLIR::Value* flir_value) const;

        #pragma region Block Management
        llvm::BasicBlock* get_block(FLIR::BasicBlock* flir_block);
        void map_block(FLIR::BasicBlock* flir_block, llvm::BasicBlock* llvm_block);
        bool has_block(FLIR::BasicBlock* flir_block) const;
        void create_all_blocks();

        #pragma region Parameter Mapping
        void map_parameters();

        // Accessors
        CodeGenModule& get_module() { return CGM; }
        LLVMIRBuilder& get_ir_builder() { return ir_builder; }
        FLIR::Function* get_flir_function() { return flir_function; }
        llvm::Function* get_llvm_function() { return llvm_function; }

    private:
        // Error helpers
        std::string format_value_error(FLIR::Value* value, const std::string& message);
        std::string format_block_error(FLIR::BasicBlock* block, const std::string& message);
    };

} // namespace Fern
