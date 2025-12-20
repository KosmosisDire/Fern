// flir_codegen.hpp - FLIR to LLVM IR Lowering (Refactored)
#pragma once

#include "codegen_module.hpp"
#include "codegen_function.hpp"
#include "llvm_ir_builder.hpp"
#include "flir/flir.hpp"
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
     * @brief Main FLIR to LLVM IR lowering implementation
     *
     * Orchestrates the lowering process and contains logic for translating
     * FLIR instructions to LLVM IR using the CodeGenModule, CodeGenFunction,
     * and LLVMIRBuilder abstractions.
     */
    class FLIRCodeGen : public DiagnosticSystem
    {
    private:
        llvm::LLVMContext& context;
        std::unique_ptr<llvm::Module> module;
        std::unique_ptr<llvm::IRBuilder<>> builder;
        TypeSystem* type_system = nullptr;

    public:
        FLIRCodeGen(llvm::LLVMContext& ctx, const std::string& module_name, TypeSystem* types = nullptr)
            : DiagnosticSystem("CodeGen"), context(ctx), type_system(types)
        {
            module = std::make_unique<llvm::Module>(module_name, context);
            builder = std::make_unique<llvm::IRBuilder<>>(context);
        }

        /**
         * @brief Lower entire FLIR module to LLVM IR
         */
        std::unique_ptr<llvm::Module> lower(FLIR::Module* flir_module);

    private:
        // === Phase 1: Module Setup ===
        void setup_module(CodeGenModule& CGM, FLIR::Module* flir_module);

        // === Phase 2: Function Body Generation ===
        void generate_function_bodies(CodeGenModule& CGM, FLIR::Module* flir_module);
        void generate_function_body(CodeGenModule& CGM, FLIR::Function* flir_func);
        void generate_basic_block(CodeGenFunction& CGF, FLIR::BasicBlock* flir_block);

        // === Instruction Generation ===
        void generate_instruction(CodeGenFunction& CGF, FLIR::Instruction* inst);

        // === Constant Instructions ===
        void gen_const_int(CodeGenFunction& CGF, FLIR::ConstIntInst* inst);
        void gen_const_float(CodeGenFunction& CGF, FLIR::ConstFloatInst* inst);
        void gen_const_bool(CodeGenFunction& CGF, FLIR::ConstBoolInst* inst);
        void gen_const_string(CodeGenFunction& CGF, FLIR::ConstStringInst* inst);
        void gen_const_null(CodeGenFunction& CGF, FLIR::ConstNullInst* inst);

        // === Memory Instructions ===
        void gen_stack_alloc(CodeGenFunction& CGF, FLIR::StackAllocInst* inst);
        void gen_stack_alloc_bytes(CodeGenFunction& CGF, FLIR::StackAllocBytesInst* inst);
        void gen_heap_alloc(CodeGenFunction& CGF, FLIR::HeapAllocInst* inst);
        void gen_heap_alloc_bytes(CodeGenFunction& CGF, FLIR::HeapAllocBytesInst* inst);
        void gen_heap_free(CodeGenFunction& CGF, FLIR::HeapFreeInst* inst);
        void gen_memcpy(CodeGenFunction& CGF, FLIR::MemCpyInst* inst);
        void gen_memset(CodeGenFunction& CGF, FLIR::MemSetInst* inst);
        void gen_load(CodeGenFunction& CGF, FLIR::LoadInst* inst);
        void gen_store(CodeGenFunction& CGF, FLIR::StoreInst* inst);
        void gen_field_addr(CodeGenFunction& CGF, FLIR::FieldAddrInst* inst);
        void gen_element_addr(CodeGenFunction& CGF, FLIR::ElementAddrInst* inst);

        // Element address helpers
        llvm::Value* gen_fixed_array_element_addr(CodeGenFunction& CGF,
                                                  FLIR::ElementAddrInst* inst);
        llvm::Value* gen_dynamic_array_element_addr(CodeGenFunction& CGF,
                                                    FLIR::ElementAddrInst* inst);
        llvm::Value* gen_pointer_element_addr(CodeGenFunction& CGF,
                                              FLIR::ElementAddrInst* inst);

        // === Arithmetic Instructions ===
        void gen_binary(CodeGenFunction& CGF, FLIR::BinaryInst* inst);
        void gen_unary(CodeGenFunction& CGF, FLIR::UnaryInst* inst);

        // Binary operation helpers
        llvm::Value* gen_arithmetic_op(LLVMIRBuilder& builder, FLIR::Opcode op,
                                      llvm::Value* left, llvm::Value* right,
                                      bool is_float, bool is_signed);
        llvm::Value* gen_comparison_op(LLVMIRBuilder& builder, FLIR::Opcode op,
                                      llvm::Value* left, llvm::Value* right,
                                      bool is_float, bool is_signed);
        llvm::Value* gen_bitwise_op(LLVMIRBuilder& builder, FLIR::Opcode op,
                                   llvm::Value* left, llvm::Value* right,
                                   bool is_signed);

        // === Cast Instruction ===
        void gen_cast(CodeGenFunction& CGF, FLIR::CastInst* inst);

        // === Call Instruction ===
        void gen_call(CodeGenFunction& CGF, FLIR::CallInst* inst);

        // === Control Flow Instructions ===
        void gen_ret(CodeGenFunction& CGF, FLIR::RetInst* inst);
        void gen_br(CodeGenFunction& CGF, FLIR::BrInst* inst);
        void gen_cond_br(CodeGenFunction& CGF, FLIR::CondBrInst* inst);

        // === Validation ===
        void verify_module();
    };

} // namespace Fern
