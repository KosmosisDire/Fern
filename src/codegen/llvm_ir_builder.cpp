// llvm_ir_builder.cpp - LLVM IR Building Utilities Implementation
#include "llvm_ir_builder.hpp"
#include <llvm/IR/DataLayout.h>
#include <iostream>

namespace Fern
{
    // === Constants ===

    llvm::Value* LLVMIRBuilder::i1_constant(bool value)
    {
        return llvm::ConstantInt::get(i1_type(), value ? 1 : 0);
    }

    llvm::Value* LLVMIRBuilder::i8_constant(int8_t value)
    {
        return llvm::ConstantInt::get(i8_type(), value, true);
    }

    llvm::Value* LLVMIRBuilder::i32_constant(int32_t value)
    {
        return llvm::ConstantInt::get(i32_type(), value, true);
    }

    llvm::Value* LLVMIRBuilder::i64_constant(int64_t value)
    {
        return llvm::ConstantInt::get(i64_type(), value, true);
    }

    llvm::Value* LLVMIRBuilder::f32_constant(float value)
    {
        return llvm::ConstantFP::get(f32_type(), value);
    }

    llvm::Value* LLVMIRBuilder::f64_constant(double value)
    {
        return llvm::ConstantFP::get(f64_type(), value);
    }

    // === Types ===

    llvm::Type* LLVMIRBuilder::void_type()
    {
        return llvm::Type::getVoidTy(context);
    }

    llvm::Type* LLVMIRBuilder::i1_type()
    {
        return llvm::Type::getInt1Ty(context);
    }

    llvm::Type* LLVMIRBuilder::i8_type()
    {
        return llvm::Type::getInt8Ty(context);
    }

    llvm::Type* LLVMIRBuilder::i32_type()
    {
        return llvm::Type::getInt32Ty(context);
    }

    llvm::Type* LLVMIRBuilder::i64_type()
    {
        return llvm::Type::getInt64Ty(context);
    }

    llvm::Type* LLVMIRBuilder::f32_type()
    {
        return llvm::Type::getFloatTy(context);
    }

    llvm::Type* LLVMIRBuilder::f64_type()
    {
        return llvm::Type::getDoubleTy(context);
    }

    llvm::Type* LLVMIRBuilder::ptr_type()
    {
        return llvm::PointerType::get(context, 0);
    }

    // === Memory Operations ===

    llvm::Value* LLVMIRBuilder::create_alloca(llvm::Type* type, const std::string& name)
    {
        return builder.CreateAlloca(type, nullptr, name);
    }

    llvm::Value* LLVMIRBuilder::create_malloc(llvm::Type* type, const std::string& name)
    {
        llvm::Value* size = i64_constant(module->getDataLayout().getTypeAllocSize(type));

        // Declare/get malloc function
        llvm::FunctionType* malloc_type = llvm::FunctionType::get(
            ptr_type(),
            {i64_type()},
            false);
        llvm::FunctionCallee malloc_func = module->getOrInsertFunction("malloc", malloc_type);

        return builder.CreateCall(malloc_func, {size}, name);
    }

    llvm::Value* LLVMIRBuilder::create_load(llvm::Type* type, llvm::Value* ptr, const std::string& name)
    {
        return builder.CreateLoad(type, ptr, name);
    }

    void LLVMIRBuilder::create_store(llvm::Value* value, llvm::Value* ptr)
    {
        // Validate pointer argument
        if (!ptr) {
            std::cerr << "ERROR: Null pointer passed to create_store\n";
            return;
        }
        
        if (!ptr->getType()->isPointerTy()) {
            std::cerr << "ERROR: Non-pointer type passed as pointer to create_store\n";
            std::cerr << "  Got type: ";
            ptr->getType()->print(llvm::errs());
            std::cerr << "\n";
            return;
        }

        // Validate value argument
        if (!value) {
            std::cerr << "ERROR: Null value passed to create_store\n";
            return;
        }

        // Note: Type validation removed for opaque pointers (LLVM 15+)
        // Opaque pointers don't have getPointerElementType()
        // LLVM will validate the store operation internally

        builder.CreateStore(value, ptr);
    }

    // === GEP Operations ===

    llvm::Value* LLVMIRBuilder::create_struct_gep(llvm::Type* struct_type, llvm::Value* ptr,
                                                   uint32_t index, const std::string& name)
    {
        return builder.CreateStructGEP(struct_type, ptr, index, name);
    }

    llvm::Value* LLVMIRBuilder::create_gep(llvm::Type* type, llvm::Value* ptr,
                                           llvm::Value* index, const std::string& name)
    {
        return builder.CreateGEP(type, ptr, index, name);
    }

    llvm::Value* LLVMIRBuilder::create_gep(llvm::Type* type, llvm::Value* ptr,
                                           std::initializer_list<llvm::Value*> indices,
                                           const std::string& name)
    {
        return builder.CreateGEP(type, ptr, std::vector<llvm::Value*>(indices), name);
    }

    llvm::Value* LLVMIRBuilder::create_inbounds_gep(llvm::Type* type, llvm::Value* ptr,
                                                    std::initializer_list<llvm::Value*> indices,
                                                    const std::string& name)
    {
        return builder.CreateInBoundsGEP(type, ptr, std::vector<llvm::Value*>(indices), name);
    }

    // === Arithmetic Operations ===

    llvm::Value* LLVMIRBuilder::create_add(llvm::Value* left, llvm::Value* right, bool is_float,
                                          const std::string& name)
    {
        return is_float ? builder.CreateFAdd(left, right, name)
                        : builder.CreateAdd(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_sub(llvm::Value* left, llvm::Value* right, bool is_float,
                                          const std::string& name)
    {
        return is_float ? builder.CreateFSub(left, right, name)
                        : builder.CreateSub(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_mul(llvm::Value* left, llvm::Value* right, bool is_float,
                                          const std::string& name)
    {
        return is_float ? builder.CreateFMul(left, right, name)
                        : builder.CreateMul(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_div(llvm::Value* left, llvm::Value* right,
                                          bool is_float, bool is_signed, const std::string& name)
    {
        if (is_float)
            return builder.CreateFDiv(left, right, name);
        else if (is_signed)
            return builder.CreateSDiv(left, right, name);
        else
            return builder.CreateUDiv(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_rem(llvm::Value* left, llvm::Value* right,
                                          bool is_float, bool is_signed, const std::string& name)
    {
        if (is_float)
            return builder.CreateFRem(left, right, name);
        else if (is_signed)
            return builder.CreateSRem(left, right, name);
        else
            return builder.CreateURem(left, right, name);
    }

    // === Comparison Operations ===

    llvm::Value* LLVMIRBuilder::create_eq(llvm::Value* left, llvm::Value* right, bool is_float,
                                         const std::string& name)
    {
        return is_float ? builder.CreateFCmpOEQ(left, right, name)
                        : builder.CreateICmpEQ(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_ne(llvm::Value* left, llvm::Value* right, bool is_float,
                                         const std::string& name)
    {
        return is_float ? builder.CreateFCmpONE(left, right, name)
                        : builder.CreateICmpNE(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_lt(llvm::Value* left, llvm::Value* right,
                                         bool is_float, bool is_signed, const std::string& name)
    {
        if (is_float)
            return builder.CreateFCmpOLT(left, right, name);
        else if (is_signed)
            return builder.CreateICmpSLT(left, right, name);
        else
            return builder.CreateICmpULT(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_le(llvm::Value* left, llvm::Value* right,
                                         bool is_float, bool is_signed, const std::string& name)
    {
        if (is_float)
            return builder.CreateFCmpOLE(left, right, name);
        else if (is_signed)
            return builder.CreateICmpSLE(left, right, name);
        else
            return builder.CreateICmpULE(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_gt(llvm::Value* left, llvm::Value* right,
                                         bool is_float, bool is_signed, const std::string& name)
    {
        if (is_float)
            return builder.CreateFCmpOGT(left, right, name);
        else if (is_signed)
            return builder.CreateICmpSGT(left, right, name);
        else
            return builder.CreateICmpUGT(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_ge(llvm::Value* left, llvm::Value* right,
                                         bool is_float, bool is_signed, const std::string& name)
    {
        if (is_float)
            return builder.CreateFCmpOGE(left, right, name);
        else if (is_signed)
            return builder.CreateICmpSGE(left, right, name);
        else
            return builder.CreateICmpUGE(left, right, name);
    }

    // === Bitwise Operations ===

    llvm::Value* LLVMIRBuilder::create_and(llvm::Value* left, llvm::Value* right,
                                          const std::string& name)
    {
        return builder.CreateAnd(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_or(llvm::Value* left, llvm::Value* right,
                                         const std::string& name)
    {
        return builder.CreateOr(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_xor(llvm::Value* left, llvm::Value* right,
                                          const std::string& name)
    {
        return builder.CreateXor(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_shl(llvm::Value* left, llvm::Value* right,
                                          const std::string& name)
    {
        return builder.CreateShl(left, right, name);
    }

    llvm::Value* LLVMIRBuilder::create_shr(llvm::Value* left, llvm::Value* right, bool is_signed,
                                          const std::string& name)
    {
        return is_signed ? builder.CreateAShr(left, right, name)
                         : builder.CreateLShr(left, right, name);
    }

    // === Unary Operations ===

    llvm::Value* LLVMIRBuilder::create_neg(llvm::Value* operand, bool is_float,
                                          const std::string& name)
    {
        return is_float ? builder.CreateFNeg(operand, name)
                        : builder.CreateNeg(operand, name);
    }

    llvm::Value* LLVMIRBuilder::create_not(llvm::Value* operand, const std::string& name)
    {
        return builder.CreateNot(operand, name);
    }

    // === Cast Operations ===

    llvm::Value* LLVMIRBuilder::create_int_cast(llvm::Value* value, llvm::Type* target_type,
                                               bool is_signed, const std::string& name)
    {
        return builder.CreateIntCast(value, target_type, is_signed, name);
    }

    llvm::Value* LLVMIRBuilder::create_float_cast(llvm::Value* value, llvm::Type* target_type,
                                                 const std::string& name)
    {
        return builder.CreateFPCast(value, target_type, name);
    }

    llvm::Value* LLVMIRBuilder::create_int_to_float(llvm::Value* value, llvm::Type* target_type,
                                                   bool is_signed, const std::string& name)
    {
        return is_signed ? builder.CreateSIToFP(value, target_type, name)
                         : builder.CreateUIToFP(value, target_type, name);
    }

    llvm::Value* LLVMIRBuilder::create_float_to_int(llvm::Value* value, llvm::Type* target_type,
                                                   bool is_signed, const std::string& name)
    {
        return is_signed ? builder.CreateFPToSI(value, target_type, name)
                         : builder.CreateFPToUI(value, target_type, name);
    }

    // === Control Flow ===

    void LLVMIRBuilder::create_ret(llvm::Value* value)
    {
        if (value)
            builder.CreateRet(value);
        else
            builder.CreateRetVoid();
    }

    void LLVMIRBuilder::create_br(llvm::BasicBlock* target)
    {
        builder.CreateBr(target);
    }

    void LLVMIRBuilder::create_cond_br(llvm::Value* condition, llvm::BasicBlock* true_block,
                                      llvm::BasicBlock* false_block)
    {
        builder.CreateCondBr(condition, true_block, false_block);
    }

    llvm::PHINode* LLVMIRBuilder::create_phi(llvm::Type* type, unsigned num_incoming,
                                            const std::string& name)
    {
        return builder.CreatePHI(type, num_incoming, name);
    }

    // === Function Calls ===

    llvm::Value* LLVMIRBuilder::create_call(llvm::Function* callee,
                                           std::vector<llvm::Value*> args,
                                           const std::string& name)
    {
        // Validate arguments before calling
        llvm::FunctionType* func_type = callee->getFunctionType();
        if (args.size() != func_type->getNumParams()) {
            std::cerr << "ERROR: Argument count mismatch for function " << callee->getName().str() << "\n";
            std::cerr << "  Expected " << func_type->getNumParams() << " arguments, got " << args.size() << "\n";
        }

        for (size_t i = 0; i < args.size(); ++i) {
            if (i < func_type->getNumParams()) {
                llvm::Type* expected_type = func_type->getParamType(i);
                llvm::Type* actual_type = args[i]->getType();
                if (expected_type != actual_type) {
                    std::cerr << "ERROR: Type mismatch in call to " << callee->getName().str() << "\n";
                    std::cerr << "  Argument " << i << ":\n";
                    std::cerr << "    Expected: ";
                    expected_type->print(llvm::errs());
                    std::cerr << "\n    Got: ";
                    actual_type->print(llvm::errs());
                    std::cerr << "\n";
                }
            }
        }

        return builder.CreateCall(callee, args, name);
    }

    // === String Constants ===

    llvm::Value* LLVMIRBuilder::create_global_string(const std::string& str,
                                                     const std::string& name)
    {
        llvm::Constant* str_const = llvm::ConstantDataArray::getString(context, str);
        llvm::GlobalVariable* global_str = new llvm::GlobalVariable(
            *module,
            str_const->getType(),
            true,
            llvm::GlobalValue::PrivateLinkage,
            str_const,
            name);

        llvm::Value* indices[] = {
            i32_constant(0),
            i32_constant(0)
        };

        return builder.CreateInBoundsGEP(
            str_const->getType(),
            global_str,
            indices,
            "str");
    }

    // === Helper Queries ===

    size_t LLVMIRBuilder::get_type_size(llvm::Type* type)
    {
        return module->getDataLayout().getTypeAllocSize(type);
    }

} // namespace Fern
