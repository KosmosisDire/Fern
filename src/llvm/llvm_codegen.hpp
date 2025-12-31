// llvm_codegen.hpp - FLIR to LLVM IR Lowering
#pragma once

#include "codegen_context.hpp"
#include "flir/flir.hpp"
#include "common/error.hpp"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <memory>
#include <string>

namespace Fern
{

class FLIRCodeGen : public DiagnosticSystem
{
private:
    llvm::LLVMContext& context;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> builder;

public:
    FLIRCodeGen(llvm::LLVMContext& ctx, const std::string& module_name)
        : DiagnosticSystem("CodeGen"), context(ctx)
    {
        module = std::make_unique<llvm::Module>(module_name, context);
        builder = std::make_unique<llvm::IRBuilder<>>(context);
    }

    std::unique_ptr<llvm::Module> lower(FLIR::Module* flir_module);

private:
    #pragma region High Level

    void generate_function_bodies(CodeGenContext& ctx, FLIR::Module* flir_module);
    void generate_function_body(CodeGenContext& ctx, FLIR::Function* flir_func);
    void generate_basic_block(CodeGenContext& ctx, FLIR::BasicBlock* flir_block);

    #pragma region Instruction Generation

    void generate_instruction(CodeGenContext& ctx, FLIR::Instruction* inst);

    void gen_const_int(CodeGenContext& ctx, FLIR::ConstIntInst* inst);
    void gen_const_float(CodeGenContext& ctx, FLIR::ConstFloatInst* inst);
    void gen_const_bool(CodeGenContext& ctx, FLIR::ConstBoolInst* inst);
    void gen_const_string(CodeGenContext& ctx, FLIR::ConstStringInst* inst);
    void gen_const_null(CodeGenContext& ctx, FLIR::ConstNullInst* inst);

    void gen_stack_alloc(CodeGenContext& ctx, FLIR::StackAllocInst* inst);
    void gen_stack_alloc_bytes(CodeGenContext& ctx, FLIR::StackAllocBytesInst* inst);
    void gen_heap_alloc(CodeGenContext& ctx, FLIR::HeapAllocInst* inst);
    void gen_heap_alloc_bytes(CodeGenContext& ctx, FLIR::HeapAllocBytesInst* inst);
    void gen_heap_free(CodeGenContext& ctx, FLIR::HeapFreeInst* inst);
    void gen_memcpy(CodeGenContext& ctx, FLIR::MemCpyInst* inst);
    void gen_memset(CodeGenContext& ctx, FLIR::MemSetInst* inst);
    void gen_load(CodeGenContext& ctx, FLIR::LoadInst* inst);
    void gen_store(CodeGenContext& ctx, FLIR::StoreInst* inst);
    void gen_field_addr(CodeGenContext& ctx, FLIR::FieldAddrInst* inst);
    void gen_element_addr(CodeGenContext& ctx, FLIR::ElementAddrInst* inst);

    llvm::Value* gen_fixed_array_element_addr(CodeGenContext& ctx, FLIR::ElementAddrInst* inst);
    llvm::Value* gen_dynamic_array_element_addr(CodeGenContext& ctx, FLIR::ElementAddrInst* inst);
    llvm::Value* gen_pointer_element_addr(CodeGenContext& ctx, FLIR::ElementAddrInst* inst);

    void gen_binary(CodeGenContext& ctx, FLIR::BinaryInst* inst);
    void gen_unary(CodeGenContext& ctx, FLIR::UnaryInst* inst);
    void gen_cast(CodeGenContext& ctx, FLIR::CastInst* inst);
    void gen_call(CodeGenContext& ctx, FLIR::CallInst* inst);
    void gen_ret(CodeGenContext& ctx, FLIR::RetInst* inst);
    void gen_br(CodeGenContext& ctx, FLIR::BrInst* inst);
    void gen_cond_br(CodeGenContext& ctx, FLIR::CondBrInst* inst);

    #pragma region Helpers

    llvm::Value* gen_arithmetic_op(CodeGenContext& ctx, FLIR::Opcode op,
                                   llvm::Value* left, llvm::Value* right,
                                   bool is_float, bool is_signed);
    llvm::Value* gen_comparison_op(CodeGenContext& ctx, FLIR::Opcode op,
                                   llvm::Value* left, llvm::Value* right,
                                   bool is_float, bool is_signed);
    llvm::Value* gen_bitwise_op(CodeGenContext& ctx, FLIR::Opcode op,
                                llvm::Value* left, llvm::Value* right,
                                bool is_signed);

    void verify_module();
};

} // namespace Fern
