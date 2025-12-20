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

        // === Value Management ===

        /**
         * @brief Get LLVM value for FLIR value
         * @throws std::runtime_error if value not found
         */
        llvm::Value* get_value(FLIR::Value* flir_value);

        /**
         * @brief Map FLIR value to LLVM value
         */
        void map_value(FLIR::Value* flir_value, llvm::Value* llvm_value);

        /**
         * @brief Check if value has been mapped
         */
        bool has_value(FLIR::Value* flir_value) const;

        // === Block Management ===

        /**
         * @brief Get LLVM basic block for FLIR block
         * @throws std::runtime_error if block not found
         */
        llvm::BasicBlock* get_block(FLIR::BasicBlock* flir_block);

        /**
         * @brief Map FLIR block to LLVM block
         */
        void map_block(FLIR::BasicBlock* flir_block, llvm::BasicBlock* llvm_block);

        /**
         * @brief Check if block has been mapped
         */
        bool has_block(FLIR::BasicBlock* flir_block) const;

        /**
         * @brief Create all basic blocks for function
         */
        void create_all_blocks();

        // === Parameter Mapping ===

        /**
         * @brief Map function parameters to LLVM arguments
         */
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
