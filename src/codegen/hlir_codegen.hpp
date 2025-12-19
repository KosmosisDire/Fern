// hlir_codegen.hpp - HLIR to LLVM IR Lowering (Refactored)
#pragma once

#include "codegen_module.hpp"
#include "codegen_function.hpp"
#include "llvm_ir_builder.hpp"
#include "hlir/hlir.hpp"
#include "semantic/type_system.hpp"
#include "common/error.hpp"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <memory>
#include <string>

namespace Fern
{
    /**
     * @brief Main HLIR to LLVM IR lowering implementation
     *
     * Orchestrates the lowering process and contains logic for translating
     * HLIR instructions to LLVM IR using the CodeGenModule, CodeGenFunction,
     * and LLVMIRBuilder abstractions.
     */
    class HLIRCodeGen : public DiagnosticSystem
    {
    private:
        llvm::LLVMContext& context;
        std::unique_ptr<llvm::Module> module;
        std::unique_ptr<llvm::IRBuilder<>> builder;
        TypeSystem* type_system = nullptr;

    public:
        HLIRCodeGen(llvm::LLVMContext& ctx, const std::string& module_name, TypeSystem* types = nullptr)
            : DiagnosticSystem("CodeGen"), context(ctx), type_system(types)
        {
            module = std::make_unique<llvm::Module>(module_name, context);
            builder = std::make_unique<llvm::IRBuilder<>>(context);
        }

        /**
         * @brief Lower entire HLIR module to LLVM IR
         */
        std::unique_ptr<llvm::Module> lower(HLIR::Module* hlir_module);

    private:
        // === Phase 1: Module Setup ===
        void setup_module(CodeGenModule& CGM, HLIR::Module* hlir_module);

        // === Phase 2: Function Body Generation ===
        void generate_function_bodies(CodeGenModule& CGM, HLIR::Module* hlir_module);
        void generate_function_body(CodeGenModule& CGM, HLIR::Function* hlir_func);
        void generate_basic_block(CodeGenFunction& CGF, HLIR::BasicBlock* hlir_block);

        // === Instruction Generation ===
        void generate_instruction(CodeGenFunction& CGF, HLIR::Instruction* inst);

        // === Constant Instructions ===
        void gen_const_int(CodeGenFunction& CGF, HLIR::ConstIntInst* inst);
        void gen_const_float(CodeGenFunction& CGF, HLIR::ConstFloatInst* inst);
        void gen_const_bool(CodeGenFunction& CGF, HLIR::ConstBoolInst* inst);
        void gen_const_string(CodeGenFunction& CGF, HLIR::ConstStringInst* inst);
        void gen_const_null(CodeGenFunction& CGF, HLIR::ConstNullInst* inst);

        // === Memory Instructions ===
        void gen_stack_alloc(CodeGenFunction& CGF, HLIR::StackAllocInst* inst);
        void gen_stack_alloc_bytes(CodeGenFunction& CGF, HLIR::StackAllocBytesInst* inst);
        void gen_heap_alloc(CodeGenFunction& CGF, HLIR::HeapAllocInst* inst);
        void gen_heap_alloc_bytes(CodeGenFunction& CGF, HLIR::HeapAllocBytesInst* inst);
        void gen_heap_free(CodeGenFunction& CGF, HLIR::HeapFreeInst* inst);
        void gen_memcpy(CodeGenFunction& CGF, HLIR::MemCpyInst* inst);
        void gen_memset(CodeGenFunction& CGF, HLIR::MemSetInst* inst);
        void gen_load(CodeGenFunction& CGF, HLIR::LoadInst* inst);
        void gen_store(CodeGenFunction& CGF, HLIR::StoreInst* inst);
        void gen_field_addr(CodeGenFunction& CGF, HLIR::FieldAddrInst* inst);
        void gen_element_addr(CodeGenFunction& CGF, HLIR::ElementAddrInst* inst);

        // Element address helpers
        llvm::Value* gen_fixed_array_element_addr(CodeGenFunction& CGF,
                                                  HLIR::ElementAddrInst* inst);
        llvm::Value* gen_dynamic_array_element_addr(CodeGenFunction& CGF,
                                                    HLIR::ElementAddrInst* inst);
        llvm::Value* gen_pointer_element_addr(CodeGenFunction& CGF,
                                              HLIR::ElementAddrInst* inst);

        // === Arithmetic Instructions ===
        void gen_binary(CodeGenFunction& CGF, HLIR::BinaryInst* inst);
        void gen_unary(CodeGenFunction& CGF, HLIR::UnaryInst* inst);

        // Binary operation helpers
        llvm::Value* gen_arithmetic_op(LLVMIRBuilder& builder, HLIR::Opcode op,
                                      llvm::Value* left, llvm::Value* right,
                                      bool is_float, bool is_signed);
        llvm::Value* gen_comparison_op(LLVMIRBuilder& builder, HLIR::Opcode op,
                                      llvm::Value* left, llvm::Value* right,
                                      bool is_float, bool is_signed);
        llvm::Value* gen_bitwise_op(LLVMIRBuilder& builder, HLIR::Opcode op,
                                   llvm::Value* left, llvm::Value* right,
                                   bool is_signed);

        // === Cast Instruction ===
        void gen_cast(CodeGenFunction& CGF, HLIR::CastInst* inst);

        // === Call Instruction ===
        void gen_call(CodeGenFunction& CGF, HLIR::CallInst* inst);

        // === Control Flow Instructions ===
        void gen_ret(CodeGenFunction& CGF, HLIR::RetInst* inst);
        void gen_br(CodeGenFunction& CGF, HLIR::BrInst* inst);
        void gen_cond_br(CodeGenFunction& CGF, HLIR::CondBrInst* inst);

        // === Validation ===
        void verify_module();
    };

} // namespace Fern
