// codegen_function.hpp - Function-Level Code Generation Context
#pragma once

#include "codegen_module.hpp"
#include "llvm_ir_builder.hpp"
#include "hlir/hlir.hpp"
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

        HLIR::Function* hlir_function;
        llvm::Function* llvm_function;

        // Value and block mappings
        std::unordered_map<HLIR::Value*, llvm::Value*> value_map;
        std::unordered_map<HLIR::BasicBlock*, llvm::BasicBlock*> block_map;

        // Pending phi nodes
        std::vector<std::pair<llvm::PHINode*, HLIR::PhiInst*>> pending_phis;

    public:
        CodeGenFunction(CodeGenModule& cgm, LLVMIRBuilder& builder,
                       HLIR::Function* hlir_func, llvm::Function* llvm_func)
            : CGM(cgm), ir_builder(builder),
              hlir_function(hlir_func), llvm_function(llvm_func) {}

        // === Value Management ===

        /**
         * @brief Get LLVM value for HLIR value
         * @throws std::runtime_error if value not found
         */
        llvm::Value* get_value(HLIR::Value* hlir_value);

        /**
         * @brief Map HLIR value to LLVM value
         */
        void map_value(HLIR::Value* hlir_value, llvm::Value* llvm_value);

        /**
         * @brief Check if value has been mapped
         */
        bool has_value(HLIR::Value* hlir_value) const;

        // === Block Management ===

        /**
         * @brief Get LLVM basic block for HLIR block
         * @throws std::runtime_error if block not found
         */
        llvm::BasicBlock* get_block(HLIR::BasicBlock* hlir_block);

        /**
         * @brief Map HLIR block to LLVM block
         */
        void map_block(HLIR::BasicBlock* hlir_block, llvm::BasicBlock* llvm_block);

        /**
         * @brief Check if block has been mapped
         */
        bool has_block(HLIR::BasicBlock* hlir_block) const;

        /**
         * @brief Create all basic blocks for function
         */
        void create_all_blocks();

        // === Phi Management ===

        /**
         * @brief Register a phi node to be resolved later
         */
        void add_pending_phi(llvm::PHINode* llvm_phi, HLIR::PhiInst* hlir_phi);

        /**
         * @brief Resolve all pending phi nodes
         */
        void resolve_pending_phis();

        // === Parameter Mapping ===

        /**
         * @brief Map function parameters to LLVM arguments
         */
        void map_parameters();

        // Accessors
        CodeGenModule& get_module() { return CGM; }
        LLVMIRBuilder& get_ir_builder() { return ir_builder; }
        HLIR::Function* get_hlir_function() { return hlir_function; }
        llvm::Function* get_llvm_function() { return llvm_function; }

    private:
        // Error helpers
        std::string format_value_error(HLIR::Value* value, const std::string& message);
        std::string format_block_error(HLIR::BasicBlock* block, const std::string& message);
    };

} // namespace Fern
