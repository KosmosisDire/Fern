// codegen_function.hpp - Function-Level Code Generation Context
#pragma once

#include "codegen_module.hpp"
#include "llvm_ir_builder.hpp"
#include "fnir/fnir.hpp"
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

        FNIR::Function* fnir_function;
        llvm::Function* llvm_function;

        // Value and block mappings
        std::unordered_map<FNIR::Value*, llvm::Value*> value_map;
        std::unordered_map<FNIR::BasicBlock*, llvm::BasicBlock*> block_map;

    public:
        CodeGenFunction(CodeGenModule& cgm, LLVMIRBuilder& builder,
                       FNIR::Function* fnir_func, llvm::Function* llvm_func)
            : CGM(cgm), ir_builder(builder),
              fnir_function(fnir_func), llvm_function(llvm_func) {}

        // === Value Management ===

        /**
         * @brief Get LLVM value for FNIR value
         * @throws std::runtime_error if value not found
         */
        llvm::Value* get_value(FNIR::Value* fnir_value);

        /**
         * @brief Map FNIR value to LLVM value
         */
        void map_value(FNIR::Value* fnir_value, llvm::Value* llvm_value);

        /**
         * @brief Check if value has been mapped
         */
        bool has_value(FNIR::Value* fnir_value) const;

        // === Block Management ===

        /**
         * @brief Get LLVM basic block for FNIR block
         * @throws std::runtime_error if block not found
         */
        llvm::BasicBlock* get_block(FNIR::BasicBlock* fnir_block);

        /**
         * @brief Map FNIR block to LLVM block
         */
        void map_block(FNIR::BasicBlock* fnir_block, llvm::BasicBlock* llvm_block);

        /**
         * @brief Check if block has been mapped
         */
        bool has_block(FNIR::BasicBlock* fnir_block) const;

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
        FNIR::Function* get_fnir_function() { return fnir_function; }
        llvm::Function* get_llvm_function() { return llvm_function; }

    private:
        // Error helpers
        std::string format_value_error(FNIR::Value* value, const std::string& message);
        std::string format_block_error(FNIR::BasicBlock* block, const std::string& message);
    };

} // namespace Fern
