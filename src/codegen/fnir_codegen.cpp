// fnir_codegen.cpp - FNIR to LLVM IR Lowering Implementation (Refactored)
#include "fnir_codegen.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>
#include <iostream>

namespace Fern
{

    #pragma region Entry

    std::unique_ptr<llvm::Module> FNIRCodeGen::lower(FNIR::Module* fnir_module)
    {
        if (!fnir_module)
        {
            throw std::runtime_error("Cannot lower null FNIR module");
        }

        CodeGenModule CGM(context, *module);
        setup_module(CGM, fnir_module);
        generate_function_bodies(CGM, fnir_module);
        verify_module();

        return std::move(module);
    }

    #pragma region High Level

    void FNIRCodeGen::setup_module(CodeGenModule& CGM, FNIR::Module* fnir_module)
    {
        // Declare all types
        CGM.declare_types(fnir_module);

        // Declare all functions
        CGM.declare_functions(fnir_module);
    }

    void FNIRCodeGen::generate_function_bodies(CodeGenModule& CGM, FNIR::Module* fnir_module)
    {
        for (const auto& fnir_func : fnir_module->functions)
        {
            if (!fnir_func->is_external && fnir_func->entry)
            {
                generate_function_body(CGM, fnir_func.get());
            }
        }
    }

    void FNIRCodeGen::generate_function_body(CodeGenModule& CGM, FNIR::Function* fnir_func)
    {
        llvm::Function* llvm_func = CGM.get_function(fnir_func);
        if (!llvm_func)
        {
            throw std::runtime_error("Function not declared: " + fnir_func->name());
        }

        // Create IR builder wrapper
        LLVMIRBuilder ir_builder(context, *builder, module.get());

        // Create function-level context
        CodeGenFunction CGF(CGM, ir_builder, fnir_func, llvm_func);

        // Map parameters to LLVM arguments
        CGF.map_parameters();

        // Create all basic blocks upfront
        CGF.create_all_blocks();

        // Generate code for each basic block
        for (const auto& fnir_block : fnir_func->blocks)
        {
            generate_basic_block(CGF, fnir_block.get());
        }
    }

    void FNIRCodeGen::generate_basic_block(CodeGenFunction& CGF, FNIR::BasicBlock* fnir_block)
    {
        llvm::BasicBlock* llvm_block = CGF.get_block(fnir_block);
        CGF.get_ir_builder().get_builder().SetInsertPoint(llvm_block);

        // Generate all instructions
        for (const auto& inst : fnir_block->instructions)
        {
            generate_instruction(CGF, inst.get());
        }
    }

    #pragma region Instruction Gen

    void FNIRCodeGen::generate_instruction(CodeGenFunction& CGF, FNIR::Instruction* inst)
    {
        switch (inst->op)
        {
        case FNIR::Opcode::ConstInt:
            gen_const_int(CGF, static_cast<FNIR::ConstIntInst*>(inst));
            break;
        case FNIR::Opcode::ConstFloat:
            gen_const_float(CGF, static_cast<FNIR::ConstFloatInst*>(inst));
            break;
        case FNIR::Opcode::ConstBool:
            gen_const_bool(CGF, static_cast<FNIR::ConstBoolInst*>(inst));
            break;
        case FNIR::Opcode::ConstString:
            gen_const_string(CGF, static_cast<FNIR::ConstStringInst*>(inst));
            break;
        case FNIR::Opcode::ConstNull:
            gen_const_null(CGF, static_cast<FNIR::ConstNullInst*>(inst));
            break;
        case FNIR::Opcode::StackAlloc:
            gen_stack_alloc(CGF, static_cast<FNIR::StackAllocInst*>(inst));
            break;
        case FNIR::Opcode::StackAllocBytes:
            gen_stack_alloc_bytes(CGF, static_cast<FNIR::StackAllocBytesInst*>(inst));
            break;
        case FNIR::Opcode::HeapAlloc:
            gen_heap_alloc(CGF, static_cast<FNIR::HeapAllocInst*>(inst));
            break;
        case FNIR::Opcode::HeapAllocBytes:
            gen_heap_alloc_bytes(CGF, static_cast<FNIR::HeapAllocBytesInst*>(inst));
            break;
        case FNIR::Opcode::HeapFree:
            gen_heap_free(CGF, static_cast<FNIR::HeapFreeInst*>(inst));
            break;
        case FNIR::Opcode::Load:
            gen_load(CGF, static_cast<FNIR::LoadInst*>(inst));
            break;
        case FNIR::Opcode::Store:
            gen_store(CGF, static_cast<FNIR::StoreInst*>(inst));
            break;
        case FNIR::Opcode::FieldAddr:
            gen_field_addr(CGF, static_cast<FNIR::FieldAddrInst*>(inst));
            break;
        case FNIR::Opcode::ElementAddr:
            gen_element_addr(CGF, static_cast<FNIR::ElementAddrInst*>(inst));
            break;
        case FNIR::Opcode::MemCpy:
            gen_memcpy(CGF, static_cast<FNIR::MemCpyInst*>(inst));
            break;
        case FNIR::Opcode::MemSet:
            gen_memset(CGF, static_cast<FNIR::MemSetInst*>(inst));
            break;
        case FNIR::Opcode::Add:
        case FNIR::Opcode::Sub:
        case FNIR::Opcode::Mul:
        case FNIR::Opcode::Div:
        case FNIR::Opcode::Rem:
        case FNIR::Opcode::Eq:
        case FNIR::Opcode::Ne:
        case FNIR::Opcode::Lt:
        case FNIR::Opcode::Le:
        case FNIR::Opcode::Gt:
        case FNIR::Opcode::Ge:
        case FNIR::Opcode::And:
        case FNIR::Opcode::Or:
        case FNIR::Opcode::BitAnd:
        case FNIR::Opcode::BitOr:
        case FNIR::Opcode::BitXor:
        case FNIR::Opcode::ShiftL:
        case FNIR::Opcode::ShiftR:
            gen_binary(CGF, static_cast<FNIR::BinaryInst*>(inst));
            break;
        case FNIR::Opcode::Neg:
        case FNIR::Opcode::Not:
        case FNIR::Opcode::BitNot:
            gen_unary(CGF, static_cast<FNIR::UnaryInst*>(inst));
            break;
        case FNIR::Opcode::Cast:
            gen_cast(CGF, static_cast<FNIR::CastInst*>(inst));
            break;
        case FNIR::Opcode::Call:
            gen_call(CGF, static_cast<FNIR::CallInst*>(inst));
            break;
        case FNIR::Opcode::Ret:
            gen_ret(CGF, static_cast<FNIR::RetInst*>(inst));
            break;
        case FNIR::Opcode::Br:
            gen_br(CGF, static_cast<FNIR::BrInst*>(inst));
            break;
        case FNIR::Opcode::CondBr:
            gen_cond_br(CGF, static_cast<FNIR::CondBrInst*>(inst));
            break;
        default:
            throw std::runtime_error("Unsupported FNIR opcode: " +
                Fern::FNIR::to_string(inst->op));
        }
    }

    #pragma region Constant Gen

    void FNIRCodeGen::gen_const_int(CodeGenFunction& CGF, FNIR::ConstIntInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Type* type = CGF.get_module().get_or_create_type(inst->result->type);

        // If type is void or invalid for integer constants, default to i32
        if (!type->isIntegerTy())
        {
            type = ir.i32_type();
        }

        llvm::Value* const_val = llvm::ConstantInt::get(type, inst->value, true);
        CGF.map_value(inst->result, const_val);
    }

    void FNIRCodeGen::gen_const_float(CodeGenFunction& CGF, FNIR::ConstFloatInst* inst)
    {
        llvm::Type* type = CGF.get_module().get_or_create_type(inst->result->type);
        llvm::Value* const_val = llvm::ConstantFP::get(type, inst->value);
        CGF.map_value(inst->result, const_val);
    }

    void FNIRCodeGen::gen_const_bool(CodeGenFunction& CGF, FNIR::ConstBoolInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* const_val = ir.i1_constant(inst->value);
        CGF.map_value(inst->result, const_val);
    }

    void FNIRCodeGen::gen_const_string(CodeGenFunction& CGF, FNIR::ConstStringInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* str_ptr = ir.create_global_string(inst->value, ".str");
        CGF.map_value(inst->result, str_ptr);
    }

    void FNIRCodeGen::gen_const_null(CodeGenFunction& CGF, FNIR::ConstNullInst* inst)
    {
        llvm::Type* type = CGF.get_module().get_or_create_type(inst->null_type);
        llvm::Value* null_val = llvm::Constant::getNullValue(type);
        CGF.map_value(inst->result, null_val);
    }

    #pragma region Memory Gen

    void FNIRCodeGen::gen_stack_alloc(CodeGenFunction& CGF, FNIR::StackAllocInst* inst)
    {
        auto& ir = CGF.get_ir_builder();

        llvm::Type* alloc_type;

        // Special handling for array types - allocate the data, not a pointer
        if (inst->alloc_type->is_array() && inst->alloc_type->array_size >= 0)
        {
            llvm::Type* elem_type = CGF.get_module().get_or_create_type(inst->alloc_type->element);
            alloc_type = llvm::ArrayType::get(elem_type, inst->alloc_type->array_size);
        }
        else
        {
            alloc_type = CGF.get_module().get_or_create_type(inst->alloc_type);
        }

        std::string name = inst->result->debug_name.empty() ? "stack_alloc" : inst->result->debug_name;
        llvm::Value* ptr = ir.create_alloca(alloc_type, name);
        CGF.map_value(inst->result, ptr);
    }

    void FNIRCodeGen::gen_stack_alloc_bytes(CodeGenFunction& CGF, FNIR::StackAllocBytesInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* size_val = CGF.get_value(inst->size);

        std::string name = inst->result->debug_name.empty() ? "stack_bytes" : inst->result->debug_name;
        llvm::Type* i8_type = llvm::Type::getInt8Ty(context);
        llvm::Value* ptr = ir.get_builder().CreateAlloca(i8_type, size_val, name);
        CGF.map_value(inst->result, ptr);
    }

    void FNIRCodeGen::gen_heap_alloc(CodeGenFunction& CGF, FNIR::HeapAllocInst* inst)
    {
        auto& ir = CGF.get_ir_builder();

        llvm::Type* alloc_type;

        // Special handling for array types
        if (inst->alloc_type->is_array() && inst->alloc_type->array_size >= 0)
        {
            llvm::Type* elem_type = CGF.get_module().get_or_create_type(inst->alloc_type->element);
            alloc_type = llvm::ArrayType::get(elem_type, inst->alloc_type->array_size);
        }
        else
        {
            alloc_type = CGF.get_module().get_or_create_type(inst->alloc_type);
        }

        std::string name = inst->result->debug_name.empty() ? "heap_alloc" : inst->result->debug_name;
        llvm::Value* ptr = ir.create_malloc(alloc_type, name);
        CGF.map_value(inst->result, ptr);
    }

    void FNIRCodeGen::gen_heap_alloc_bytes(CodeGenFunction& CGF, FNIR::HeapAllocBytesInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* size_val = CGF.get_value(inst->size);

        std::string name = inst->result->debug_name.empty() ? "heap_bytes" : inst->result->debug_name;
        llvm::Value* ptr = ir.create_malloc_bytes(size_val, name);
        CGF.map_value(inst->result, ptr);
    }

    void FNIRCodeGen::gen_heap_free(CodeGenFunction& CGF, FNIR::HeapFreeInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* ptr_val = CGF.get_value(inst->ptr);

        // Call free
        ir.create_free(ptr_val);
    }

    void FNIRCodeGen::gen_memcpy(CodeGenFunction& CGF, FNIR::MemCpyInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* dest = CGF.get_value(inst->dest);
        llvm::Value* src = CGF.get_value(inst->src);
        llvm::Value* size = CGF.get_value(inst->size);

        // Call llvm.memcpy intrinsic
        ir.create_memcpy(dest, src, size, inst->is_volatile);
    }

    void FNIRCodeGen::gen_memset(CodeGenFunction& CGF, FNIR::MemSetInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* dest = CGF.get_value(inst->dest);
        llvm::Value* value = CGF.get_value(inst->value);
        llvm::Value* size = CGF.get_value(inst->size);

        // Call llvm.memset intrinsic
        ir.create_memset(dest, value, size, inst->is_volatile);
    }

    void FNIRCodeGen::gen_load(CodeGenFunction& CGF, FNIR::LoadInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* addr = CGF.get_value(inst->address);

        // Validate that we're loading from a pointer
        if (!addr->getType()->isPointerTy())
        {
            std::string error = "Load instruction expects pointer, but got: ";
            llvm::raw_string_ostream os(error);
            addr->getType()->print(os);
            os << "\n  Address FNIR value: %" << inst->address->id;
            if (!inst->address->debug_name.empty())
            {
                os << " <" << inst->address->debug_name << ">";
            }
            os << " : " << inst->address->type->get_name();
            throw std::runtime_error(os.str());
        }

        llvm::Type* load_type = CGF.get_module().get_or_create_type(inst->result->type);
        std::string name = inst->result->debug_name.empty() ? "load" : inst->result->debug_name;
        llvm::Value* loaded = ir.create_load(load_type, addr, name);
        CGF.map_value(inst->result, loaded);
    }

    void FNIRCodeGen::gen_store(CodeGenFunction& CGF, FNIR::StoreInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* val = CGF.get_value(inst->value);
        llvm::Value* addr = CGF.get_value(inst->address);
        ir.create_store(val, addr);
    }

    void FNIRCodeGen::gen_field_addr(CodeGenFunction& CGF, FNIR::FieldAddrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* obj = CGF.get_value(inst->object);

        // Get the struct type we're accessing
        llvm::Type* struct_type = CGF.get_module().get_or_create_type(inst->object->type);

        // If the FNIR type is a pointer, get the pointee type for GEP
        if (inst->object->type->is_pointer() && inst->object->type->pointee)
        {
            struct_type = CGF.get_module().get_or_create_type(inst->object->type->pointee);
        }

        // GEP to get field address
        std::string name = inst->result->debug_name.empty() ? "field_addr" : inst->result->debug_name;
        llvm::Value* field_ptr = ir.create_struct_gep(
            struct_type,
            obj,
            inst->field_index,
            name);

        CGF.map_value(inst->result, field_ptr);
    }

    void FNIRCodeGen::gen_element_addr(CodeGenFunction& CGF, FNIR::ElementAddrInst* inst)
    {
        // Determine the underlying type we're indexing into
        FNIR::IRTypePtr fnir_type = inst->array->type;

        // Check if this is a pointer type - if so, use pointer arithmetic
        if (fnir_type->is_pointer()) {
            CGF.map_value(inst->result, gen_pointer_element_addr(CGF, inst));
            return;
        }

        // Check the FNIR type for arrays
        if (fnir_type->is_array()) {
            // If the array value came from a load instruction, it's actually a pointer
            // in LLVM representation (e.g., loaded nested array), so use pointer arithmetic
            if (inst->array->def && inst->array->def->op == FNIR::Opcode::Load) {
                CGF.map_value(inst->result, gen_pointer_element_addr(CGF, inst));
                return;
            }

            if (fnir_type->array_size < 0) {
                CGF.map_value(inst->result, gen_dynamic_array_element_addr(CGF, inst));
            } else {
                CGF.map_value(inst->result, gen_fixed_array_element_addr(CGF, inst));
            }
            return;
        }

        // Fallback: direct pointer arithmetic
        CGF.map_value(inst->result, gen_pointer_element_addr(CGF, inst));
    }

    llvm::Value* FNIRCodeGen::gen_fixed_array_element_addr(CodeGenFunction& CGF,
                                                           FNIR::ElementAddrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* array = CGF.get_value(inst->array);
        llvm::Value* index = CGF.get_value(inst->index);

        // Get the array type (unwrap pointer if needed)
        FNIR::IRTypePtr fnir_type = inst->array->type;
        if (fnir_type->is_pointer() && fnir_type->pointee) {
            fnir_type = fnir_type->pointee;
        }
        llvm::Type* array_llvm_type = CGF.get_module().get_or_create_type(fnir_type);

        // For fixed arrays, we need GEP with two indices: [0, index]
        llvm::Value* zero = ir.i32_constant(0);
        return ir.create_inbounds_gep(array_llvm_type, array, {zero, index}, "elem_addr");
    }

    llvm::Value* FNIRCodeGen::gen_dynamic_array_element_addr(CodeGenFunction& CGF,
                                                             FNIR::ElementAddrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* array = CGF.get_value(inst->array);
        llvm::Value* index = CGF.get_value(inst->index);
        llvm::Type* array_llvm_type = CGF.get_module().get_or_create_type(inst->array->type);

        // Get element type
        llvm::Type* elem_type = CGF.get_module().get_or_create_type(inst->result->type);
        if (inst->result->type->is_pointer() && inst->result->type->pointee)
        {
            elem_type = CGF.get_module().get_or_create_type(inst->result->type->pointee);
        }

        // Load the data pointer (second field of the array struct)
        llvm::Value* data_ptr_addr = ir.create_struct_gep(
            array_llvm_type,
            array,
            1,
            "data_ptr_addr");

        llvm::Value* data_ptr = ir.create_load(
            ir.ptr_type(),
            data_ptr_addr,
            "data_ptr");

        // GEP into the data pointer
        return ir.create_gep(elem_type, data_ptr, index, "elem_addr");
    }

    llvm::Value* FNIRCodeGen::gen_pointer_element_addr(CodeGenFunction& CGF, FNIR::ElementAddrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* array = CGF.get_value(inst->array);
        llvm::Value* index = CGF.get_value(inst->index);

        // Determine the element type
        FNIR::IRTypePtr fnir_type = inst->array->type;
        llvm::Type* elem_type = nullptr;

        // If it's a pointer, unwrap to get the pointee type
        if (fnir_type->is_pointer() && fnir_type->pointee)
        {
            fnir_type = fnir_type->pointee;
        }

        // Check if the pointee is an array type (e.g., for pointer to array)
        if (fnir_type->is_array() && fnir_type->element)
        {
            elem_type = CGF.get_module().get_or_create_type(fnir_type->element);
        }
        else
        {
            // Direct pointer arithmetic (e.g., char* + 5)
            // The element type is just the pointee type itself
            // Special case: void* is treated as i8* for pointer arithmetic (like C)
            if (fnir_type->is_void())
            {
                elem_type = llvm::Type::getInt8Ty(context);
            }
            else
            {
                elem_type = CGF.get_module().get_or_create_type(fnir_type);
            }
        }

        if (!elem_type)
        {
            throw std::runtime_error("Cannot determine element type for pointer GEP");
        }

        auto* result = ir.create_gep(elem_type, array, index, "elem_addr");

        return result;
    }

    #pragma region Binary Gen

    void FNIRCodeGen::gen_binary(CodeGenFunction& CGF, FNIR::BinaryInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* left = CGF.get_value(inst->left);
        llvm::Value* right = CGF.get_value(inst->right);

        auto props = CGF.get_module().get_type_properties(inst->left->type);

        llvm::Value* result = nullptr;

        // Determine operation category and dispatch
        switch (inst->op)
        {
        case FNIR::Opcode::Add:
        case FNIR::Opcode::Sub:
        case FNIR::Opcode::Mul:
        case FNIR::Opcode::Div:
        case FNIR::Opcode::Rem:
            result = gen_arithmetic_op(ir, inst->op, left, right, props.is_float, props.is_signed);
            break;

        case FNIR::Opcode::Eq:
        case FNIR::Opcode::Ne:
        case FNIR::Opcode::Lt:
        case FNIR::Opcode::Le:
        case FNIR::Opcode::Gt:
        case FNIR::Opcode::Ge:
            result = gen_comparison_op(ir, inst->op, left, right, props.is_float, props.is_signed);
            break;

        case FNIR::Opcode::And:
        case FNIR::Opcode::Or:
        case FNIR::Opcode::BitAnd:
        case FNIR::Opcode::BitOr:
        case FNIR::Opcode::BitXor:
        case FNIR::Opcode::ShiftL:
        case FNIR::Opcode::ShiftR:
            result = gen_bitwise_op(ir, inst->op, left, right, props.is_signed);
            break;

        default:
            throw std::runtime_error("Unsupported binary operation");
        }

        CGF.map_value(inst->result, result);
    }

    llvm::Value* FNIRCodeGen::gen_arithmetic_op(LLVMIRBuilder& ir, FNIR::Opcode op,
                                               llvm::Value* left, llvm::Value* right,
                                               bool is_float, bool is_signed)
    {
        switch (op)
        {
        case FNIR::Opcode::Add:
            return ir.create_add(left, right, is_float, "add");
        case FNIR::Opcode::Sub:
            return ir.create_sub(left, right, is_float, "sub");
        case FNIR::Opcode::Mul:
            return ir.create_mul(left, right, is_float, "mul");
        case FNIR::Opcode::Div:
            return ir.create_div(left, right, is_float, is_signed, "div");
        case FNIR::Opcode::Rem:
            return ir.create_rem(left, right, is_float, is_signed, "rem");
        default:
            throw std::runtime_error("Invalid arithmetic operation");
        }
    }

    llvm::Value* FNIRCodeGen::gen_comparison_op(LLVMIRBuilder& ir, FNIR::Opcode op,
                                               llvm::Value* left, llvm::Value* right,
                                               bool is_float, bool is_signed)
    {
        switch (op)
        {
        case FNIR::Opcode::Eq:
            return ir.create_eq(left, right, is_float, "eq");
        case FNIR::Opcode::Ne:
            return ir.create_ne(left, right, is_float, "ne");
        case FNIR::Opcode::Lt:
            return ir.create_lt(left, right, is_float, is_signed, "lt");
        case FNIR::Opcode::Le:
            return ir.create_le(left, right, is_float, is_signed, "le");
        case FNIR::Opcode::Gt:
            return ir.create_gt(left, right, is_float, is_signed, "gt");
        case FNIR::Opcode::Ge:
            return ir.create_ge(left, right, is_float, is_signed, "ge");
        default:
            throw std::runtime_error("Invalid comparison operation");
        }
    }

    llvm::Value* FNIRCodeGen::gen_bitwise_op(LLVMIRBuilder& ir, FNIR::Opcode op,
                                            llvm::Value* left, llvm::Value* right,
                                            bool is_signed)
    {
        switch (op)
        {
        case FNIR::Opcode::And:
        case FNIR::Opcode::BitAnd:
            return ir.create_and(left, right, "and");
        case FNIR::Opcode::Or:
        case FNIR::Opcode::BitOr:
            return ir.create_or(left, right, "or");
        case FNIR::Opcode::BitXor:
            return ir.create_xor(left, right, "xor");
        case FNIR::Opcode::ShiftL:
            return ir.create_shl(left, right, "shl");
        case FNIR::Opcode::ShiftR:
            return ir.create_shr(left, right, is_signed, "shr");
        default:
            throw std::runtime_error("Invalid bitwise operation");
        }
    }

    #pragma region Unary Gen

    void FNIRCodeGen::gen_unary(CodeGenFunction& CGF, FNIR::UnaryInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* operand = CGF.get_value(inst->operand);

        auto props = CGF.get_module().get_type_properties(inst->operand->type);

        llvm::Value* result = nullptr;

        switch (inst->op)
        {
        case FNIR::Opcode::Neg:
            result = ir.create_neg(operand, props.is_float, "neg");
            break;
        case FNIR::Opcode::Not:
        case FNIR::Opcode::BitNot:
            result = ir.create_not(operand, "not");
            break;
        default:
            throw std::runtime_error("Unsupported unary operation");
        }

        CGF.map_value(inst->result, result);
    }

    #pragma region Cast Gen

    void FNIRCodeGen::gen_cast(CodeGenFunction& CGF, FNIR::CastInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* value = CGF.get_value(inst->value);
        llvm::Type* target_type = CGF.get_module().get_or_create_type(inst->target_type);

        auto src_props = CGF.get_module().get_type_properties(inst->value->type);
        auto dst_props = CGF.get_module().get_type_properties(inst->target_type);

        llvm::Value* result = nullptr;

        // Handle pointer-to-integer cast
        if (src_props.is_pointer && dst_props.is_integer)
        {
            // Use ptrtoint instruction
            result = ir.create_ptr_to_int(value, target_type, "ptrtoint");
        }
        // Handle integer-to-pointer cast
        else if (src_props.is_integer && dst_props.is_pointer)
        {
            // Use inttoptr instruction
            result = ir.create_int_to_ptr(value, target_type, "inttoptr");
        }
        // Handle pointer-to-pointer cast (bitcast)
        else if (src_props.is_pointer && dst_props.is_pointer)
        {
            // Pointer to pointer cast (bitcast)
            result = ir.create_bitcast(value, target_type, "ptrcast");
        }
        else if (src_props.is_float && dst_props.is_float)
        {
            // Float to float
            result = ir.create_float_cast(value, target_type, "cast");
        }
        else if (src_props.is_float && !dst_props.is_float)
        {
            // Float to int
            result = ir.create_float_to_int(value, target_type, dst_props.is_signed, "cast");
        }
        else if (!src_props.is_float && dst_props.is_float)
        {
            // Int to float
            result = ir.create_int_to_float(value, target_type, src_props.is_signed, "cast");
        }
        else
        {
            // Int to int
            result = ir.create_int_cast(value, target_type, src_props.is_signed, "cast");
        }

        CGF.map_value(inst->result, result);
    }

    #pragma region Call Gen

    void FNIRCodeGen::gen_call(CodeGenFunction& CGF, FNIR::CallInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Function* callee = CGF.get_module().get_function(inst->callee);
        if (!callee)
        {
            throw std::runtime_error("Function not declared: " + inst->callee->name());
        }

        // Collect arguments
        std::vector<llvm::Value*> args;
        for (FNIR::Value* arg : inst->args)
        {
            args.push_back(CGF.get_value(arg));
        }

        // Create call - only name the result if it's not void
        std::string call_name = "";
        if (!callee->getReturnType()->isVoidTy())
        {
            call_name = (inst->result && !inst->result->debug_name.empty())
                        ? inst->result->debug_name : "call";
        }
        llvm::Value* call_result = ir.create_call(callee, args, call_name);

        // Map result if not void
        if (inst->result)
        {
            CGF.map_value(inst->result, call_result);
        }
    }

    #pragma region Control Flow Gen

    void FNIRCodeGen::gen_ret(CodeGenFunction& CGF, FNIR::RetInst* inst)
    {
        auto& ir = CGF.get_ir_builder();

        if (inst->value)
        {
            llvm::Value* ret_val = CGF.get_value(inst->value);
            ir.create_ret(ret_val);
        }
        else
        {
            ir.create_ret(nullptr);
        }
    }

    void FNIRCodeGen::gen_br(CodeGenFunction& CGF, FNIR::BrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::BasicBlock* target = CGF.get_block(inst->target);
        ir.create_br(target);
    }

    void FNIRCodeGen::gen_cond_br(CodeGenFunction& CGF, FNIR::CondBrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* cond = CGF.get_value(inst->condition);
        llvm::BasicBlock* true_block = CGF.get_block(inst->true_block);
        llvm::BasicBlock* false_block = CGF.get_block(inst->false_block);
        ir.create_cond_br(cond, true_block, false_block);
    }

    #pragma region Verification

    void FNIRCodeGen::verify_module()
    {
        std::string error_msg;
        llvm::raw_string_ostream error_stream(error_msg);
        if (llvm::verifyModule(*module, &error_stream))
        {
            std::cerr << "LLVM Module verification failed:\n"
                      << error_msg << std::endl;
            module->print(llvm::errs(), nullptr);
            throw std::runtime_error("Invalid LLVM module generated");
        }
    }

} // namespace Fern
