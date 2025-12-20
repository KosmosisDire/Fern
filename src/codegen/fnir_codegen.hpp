// fnir_codegen.hpp - FNIR to LLVM IR Lowering (Refactored)
#pragma once

#include "codegen_module.hpp"
#include "codegen_function.hpp"
#include "llvm_ir_builder.hpp"
#include "fnir/fnir.hpp"
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
     * @brief Main FNIR to LLVM IR lowering implementation
     *
     * Orchestrates the lowering process and contains logic for translating
     * FNIR instructions to LLVM IR using the CodeGenModule, CodeGenFunction,
     * and LLVMIRBuilder abstractions.
     */
    class FNIRCodeGen : public DiagnosticSystem
    {
    private:
        llvm::LLVMContext& context;
        std::unique_ptr<llvm::Module> module;
        std::unique_ptr<llvm::IRBuilder<>> builder;
        TypeSystem* type_system = nullptr;

    public:
        FNIRCodeGen(llvm::LLVMContext& ctx, const std::string& module_name, TypeSystem* types = nullptr)
            : DiagnosticSystem("CodeGen"), context(ctx), type_system(types)
        {
            module = std::make_unique<llvm::Module>(module_name, context);
            builder = std::make_unique<llvm::IRBuilder<>>(context);
        }

        /**
         * @brief Lower entire FNIR module to LLVM IR
         */
        std::unique_ptr<llvm::Module> lower(FNIR::Module* fnir_module);

    private:
        // === Phase 1: Module Setup ===
        void setup_module(CodeGenModule& CGM, FNIR::Module* fnir_module);

        // === Phase 2: Function Body Generation ===
        void generate_function_bodies(CodeGenModule& CGM, FNIR::Module* fnir_module);
        void generate_function_body(CodeGenModule& CGM, FNIR::Function* fnir_func);
        void generate_basic_block(CodeGenFunction& CGF, FNIR::BasicBlock* fnir_block);

        // === Instruction Generation ===
        void generate_instruction(CodeGenFunction& CGF, FNIR::Instruction* inst);

        // === Constant Instructions ===
        void gen_const_int(CodeGenFunction& CGF, FNIR::ConstIntInst* inst);
        void gen_const_float(CodeGenFunction& CGF, FNIR::ConstFloatInst* inst);
        void gen_const_bool(CodeGenFunction& CGF, FNIR::ConstBoolInst* inst);
        void gen_const_string(CodeGenFunction& CGF, FNIR::ConstStringInst* inst);
        void gen_const_null(CodeGenFunction& CGF, FNIR::ConstNullInst* inst);

        // === Memory Instructions ===
        void gen_stack_alloc(CodeGenFunction& CGF, FNIR::StackAllocInst* inst);
        void gen_stack_alloc_bytes(CodeGenFunction& CGF, FNIR::StackAllocBytesInst* inst);
        void gen_heap_alloc(CodeGenFunction& CGF, FNIR::HeapAllocInst* inst);
        void gen_heap_alloc_bytes(CodeGenFunction& CGF, FNIR::HeapAllocBytesInst* inst);
        void gen_heap_free(CodeGenFunction& CGF, FNIR::HeapFreeInst* inst);
        void gen_memcpy(CodeGenFunction& CGF, FNIR::MemCpyInst* inst);
        void gen_memset(CodeGenFunction& CGF, FNIR::MemSetInst* inst);
        void gen_load(CodeGenFunction& CGF, FNIR::LoadInst* inst);
        void gen_store(CodeGenFunction& CGF, FNIR::StoreInst* inst);
        void gen_field_addr(CodeGenFunction& CGF, FNIR::FieldAddrInst* inst);
        void gen_element_addr(CodeGenFunction& CGF, FNIR::ElementAddrInst* inst);

        // Element address helpers
        llvm::Value* gen_fixed_array_element_addr(CodeGenFunction& CGF,
                                                  FNIR::ElementAddrInst* inst);
        llvm::Value* gen_dynamic_array_element_addr(CodeGenFunction& CGF,
                                                    FNIR::ElementAddrInst* inst);
        llvm::Value* gen_pointer_element_addr(CodeGenFunction& CGF,
                                              FNIR::ElementAddrInst* inst);

        // === Arithmetic Instructions ===
        void gen_binary(CodeGenFunction& CGF, FNIR::BinaryInst* inst);
        void gen_unary(CodeGenFunction& CGF, FNIR::UnaryInst* inst);

        // Binary operation helpers
        llvm::Value* gen_arithmetic_op(LLVMIRBuilder& builder, FNIR::Opcode op,
                                      llvm::Value* left, llvm::Value* right,
                                      bool is_float, bool is_signed);
        llvm::Value* gen_comparison_op(LLVMIRBuilder& builder, FNIR::Opcode op,
                                      llvm::Value* left, llvm::Value* right,
                                      bool is_float, bool is_signed);
        llvm::Value* gen_bitwise_op(LLVMIRBuilder& builder, FNIR::Opcode op,
                                   llvm::Value* left, llvm::Value* right,
                                   bool is_signed);

        // === Cast Instruction ===
        void gen_cast(CodeGenFunction& CGF, FNIR::CastInst* inst);

        // === Call Instruction ===
        void gen_call(CodeGenFunction& CGF, FNIR::CallInst* inst);

        // === Control Flow Instructions ===
        void gen_ret(CodeGenFunction& CGF, FNIR::RetInst* inst);
        void gen_br(CodeGenFunction& CGF, FNIR::BrInst* inst);
        void gen_cond_br(CodeGenFunction& CGF, FNIR::CondBrInst* inst);

        // === Validation ===
        void verify_module();
    };

} // namespace Fern
