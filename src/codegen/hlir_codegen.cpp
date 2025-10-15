// hlir_codegen.cpp - HLIR to LLVM IR Lowering Implementation (Refactored)
#include "hlir_codegen.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>
#include <iostream>

namespace Fern
{

    #pragma region Entry

    std::unique_ptr<llvm::Module> HLIRCodeGen::lower(HLIR::Module* hlir_module)
    {
        if (!hlir_module)
        {
            throw std::runtime_error("Cannot lower null HLIR module");
        }

        CodeGenModule CGM(context, *module);
        setup_module(CGM, hlir_module);
        generate_function_bodies(CGM, hlir_module);
        verify_module();

        return std::move(module);
    }

    #pragma region High Level

    void HLIRCodeGen::setup_module(CodeGenModule& CGM, HLIR::Module* hlir_module)
    {
        // Declare all types
        CGM.declare_types(hlir_module);

        // Declare all functions
        CGM.declare_functions(hlir_module);
    }

    void HLIRCodeGen::generate_function_bodies(CodeGenModule& CGM, HLIR::Module* hlir_module)
    {
        for (const auto& hlir_func : hlir_module->functions)
        {
            if (!hlir_func->is_external && hlir_func->entry)
            {
                generate_function_body(CGM, hlir_func.get());
            }
        }
    }

    void HLIRCodeGen::generate_function_body(CodeGenModule& CGM, HLIR::Function* hlir_func)
    {
        llvm::Function* llvm_func = CGM.get_function(hlir_func);
        if (!llvm_func)
        {
            throw std::runtime_error("Function not declared: " + hlir_func->name());
        }

        // Create IR builder wrapper
        LLVMIRBuilder ir_builder(context, *builder, module.get());

        // Create function-level context
        CodeGenFunction CGF(CGM, ir_builder, hlir_func, llvm_func);

        // Map parameters to LLVM arguments
        CGF.map_parameters();

        // Create all basic blocks upfront
        CGF.create_all_blocks();

        // Generate code for each basic block
        for (const auto& hlir_block : hlir_func->blocks)
        {
            generate_basic_block(CGF, hlir_block.get());
        }

        // Resolve pending phi nodes
        CGF.resolve_pending_phis();
    }

    void HLIRCodeGen::generate_basic_block(CodeGenFunction& CGF, HLIR::BasicBlock* hlir_block)
    {
        llvm::BasicBlock* llvm_block = CGF.get_block(hlir_block);
        CGF.get_ir_builder().get_builder().SetInsertPoint(llvm_block);

        // Generate all instructions
        for (const auto& inst : hlir_block->instructions)
        {
            generate_instruction(CGF, inst.get());
        }
    }

    #pragma region Instruction Gen

    void HLIRCodeGen::generate_instruction(CodeGenFunction& CGF, HLIR::Instruction* inst)
    {
        switch (inst->op)
        {
        case HLIR::Opcode::ConstInt:
            gen_const_int(CGF, static_cast<HLIR::ConstIntInst*>(inst));
            break;
        case HLIR::Opcode::ConstFloat:
            gen_const_float(CGF, static_cast<HLIR::ConstFloatInst*>(inst));
            break;
        case HLIR::Opcode::ConstBool:
            gen_const_bool(CGF, static_cast<HLIR::ConstBoolInst*>(inst));
            break;
        case HLIR::Opcode::ConstString:
            gen_const_string(CGF, static_cast<HLIR::ConstStringInst*>(inst));
            break;
        case HLIR::Opcode::Alloc:
            gen_alloc(CGF, static_cast<HLIR::AllocInst*>(inst));
            break;
        case HLIR::Opcode::Load:
            gen_load(CGF, static_cast<HLIR::LoadInst*>(inst));
            break;
        case HLIR::Opcode::Store:
            gen_store(CGF, static_cast<HLIR::StoreInst*>(inst));
            break;
        case HLIR::Opcode::FieldAddr:
            gen_field_addr(CGF, static_cast<HLIR::FieldAddrInst*>(inst));
            break;
        case HLIR::Opcode::ElementAddr:
            gen_element_addr(CGF, static_cast<HLIR::ElementAddrInst*>(inst));
            break;
        case HLIR::Opcode::Add:
        case HLIR::Opcode::Sub:
        case HLIR::Opcode::Mul:
        case HLIR::Opcode::Div:
        case HLIR::Opcode::Rem:
        case HLIR::Opcode::Eq:
        case HLIR::Opcode::Ne:
        case HLIR::Opcode::Lt:
        case HLIR::Opcode::Le:
        case HLIR::Opcode::Gt:
        case HLIR::Opcode::Ge:
        case HLIR::Opcode::And:
        case HLIR::Opcode::Or:
        case HLIR::Opcode::BitAnd:
        case HLIR::Opcode::BitOr:
        case HLIR::Opcode::BitXor:
        case HLIR::Opcode::Shl:
        case HLIR::Opcode::Shr:
            gen_binary(CGF, static_cast<HLIR::BinaryInst*>(inst));
            break;
        case HLIR::Opcode::Neg:
        case HLIR::Opcode::Not:
        case HLIR::Opcode::BitNot:
            gen_unary(CGF, static_cast<HLIR::UnaryInst*>(inst));
            break;
        case HLIR::Opcode::Cast:
            gen_cast(CGF, static_cast<HLIR::CastInst*>(inst));
            break;
        case HLIR::Opcode::Call:
            gen_call(CGF, static_cast<HLIR::CallInst*>(inst));
            break;
        case HLIR::Opcode::Ret:
            gen_ret(CGF, static_cast<HLIR::RetInst*>(inst));
            break;
        case HLIR::Opcode::Br:
            gen_br(CGF, static_cast<HLIR::BrInst*>(inst));
            break;
        case HLIR::Opcode::CondBr:
            gen_cond_br(CGF, static_cast<HLIR::CondBrInst*>(inst));
            break;
        case HLIR::Opcode::Phi:
            gen_phi(CGF, static_cast<HLIR::PhiInst*>(inst));
            break;
        default:
            throw std::runtime_error("Unsupported HLIR opcode: " +
                std::to_string(static_cast<int>(inst->op)));
        }
    }

    #pragma region Constant Gen

    void HLIRCodeGen::gen_const_int(CodeGenFunction& CGF, HLIR::ConstIntInst* inst)
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

    void HLIRCodeGen::gen_const_float(CodeGenFunction& CGF, HLIR::ConstFloatInst* inst)
    {
        llvm::Type* type = CGF.get_module().get_or_create_type(inst->result->type);
        llvm::Value* const_val = llvm::ConstantFP::get(type, inst->value);
        CGF.map_value(inst->result, const_val);
    }

    void HLIRCodeGen::gen_const_bool(CodeGenFunction& CGF, HLIR::ConstBoolInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* const_val = ir.i1_constant(inst->value);
        CGF.map_value(inst->result, const_val);
    }

    void HLIRCodeGen::gen_const_string(CodeGenFunction& CGF, HLIR::ConstStringInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* str_ptr = ir.create_global_string(inst->value, ".str");
        CGF.map_value(inst->result, str_ptr);
    }

    #pragma region Memory Gen

    void HLIRCodeGen::gen_alloc(CodeGenFunction& CGF, HLIR::AllocInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        
        llvm::Type* alloc_type;
        
        // Special handling for array types - allocate the data, not a pointer
        if (auto* array_type = inst->alloc_type->as<ArrayType>())
        {
            llvm::Type* elem_type = CGF.get_module().get_or_create_type(array_type->element);
            alloc_type = llvm::ArrayType::get(elem_type, array_type->size);
        }
        else
        {
            alloc_type = CGF.get_module().get_or_create_type(inst->alloc_type);
        }

        llvm::Value* ptr;
        if (inst->on_stack)
        {
            ptr = ir.create_alloca(alloc_type, "alloc");
        }
        else
        {
            ptr = ir.create_malloc(alloc_type, "heap_alloc");
        }

        CGF.map_value(inst->result, ptr);
    }

    void HLIRCodeGen::gen_load(CodeGenFunction& CGF, HLIR::LoadInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* addr = CGF.get_value(inst->address);

        // Validate that we're loading from a pointer
        if (!addr->getType()->isPointerTy())
        {
            std::string error = "Load instruction expects pointer, but got: ";
            llvm::raw_string_ostream os(error);
            addr->getType()->print(os);
            os << "\n  Address HLIR value: %" << inst->address->id;
            if (!inst->address->debug_name.empty())
            {
                os << " <" << inst->address->debug_name << ">";
            }
            os << " : " << inst->address->type->get_name();
            throw std::runtime_error(os.str());
        }

        llvm::Type* load_type = CGF.get_module().get_or_create_type(inst->result->type);
        llvm::Value* loaded = ir.create_load(load_type, addr, "load");
        CGF.map_value(inst->result, loaded);
    }

    void HLIRCodeGen::gen_store(CodeGenFunction& CGF, HLIR::StoreInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* val = CGF.get_value(inst->value);
        llvm::Value* addr = CGF.get_value(inst->address);
        ir.create_store(val, addr);
    }

    void HLIRCodeGen::gen_field_addr(CodeGenFunction& CGF, HLIR::FieldAddrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* obj = CGF.get_value(inst->object);

        // Get the struct type we're accessing
        llvm::Type* struct_type = CGF.get_module().get_or_create_type(inst->object->type);

        // If the HLIR type is a pointer, get the pointee type for GEP
        if (auto* ptr_type = inst->object->type->as<PointerType>())
        {
            struct_type = CGF.get_module().get_or_create_type(ptr_type->pointee);
        }

        // GEP to get field address
        llvm::Value* field_ptr = ir.create_struct_gep(
            struct_type,
            obj,
            inst->field_index,
            "field_addr");

        CGF.map_value(inst->result, field_ptr);
    }

    void HLIRCodeGen::gen_element_addr(CodeGenFunction& CGF, HLIR::ElementAddrInst* inst)
    {
        // Determine the underlying type we're indexing into
        TypePtr hlir_type = inst->array->type;

        // If it's a pointer, get the pointee type
        if (auto ptr_type = hlir_type->as<PointerType>()) {
            hlir_type = ptr_type->pointee;
        }

        llvm::Type* array_llvm_type = CGF.get_module().get_or_create_type(hlir_type);

        llvm::Value* elem_ptr;

        // Check if this is a fixed-size array [N x T]
        if (array_llvm_type->isArrayTy())
        {
            elem_ptr = gen_fixed_array_element_addr(CGF, inst);
        }
        // Check if this is a dynamic array (struct { i32, ptr })
        else if (array_llvm_type->isStructTy())
        {
            elem_ptr = gen_dynamic_array_element_addr(CGF, inst);
        }
        else
        {
            // Direct pointer - simple GEP
            elem_ptr = gen_pointer_element_addr(CGF, inst);
        }

        CGF.map_value(inst->result, elem_ptr);
    }

    llvm::Value* HLIRCodeGen::gen_fixed_array_element_addr(CodeGenFunction& CGF,
                                                           HLIR::ElementAddrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* array = CGF.get_value(inst->array);
        llvm::Value* index = CGF.get_value(inst->index);

        // Get the array type (unwrap pointer if needed)
        TypePtr hlir_type = inst->array->type;
        if (auto ptr_type = hlir_type->as<PointerType>()) {
            hlir_type = ptr_type->pointee;
        }
        llvm::Type* array_llvm_type = CGF.get_module().get_or_create_type(hlir_type);

        // For fixed arrays, we need GEP with two indices: [0, index]
        llvm::Value* zero = ir.i32_constant(0);
        return ir.create_inbounds_gep(array_llvm_type, array, {zero, index}, "elem_addr");
    }

    llvm::Value* HLIRCodeGen::gen_dynamic_array_element_addr(CodeGenFunction& CGF,
                                                             HLIR::ElementAddrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* array = CGF.get_value(inst->array);
        llvm::Value* index = CGF.get_value(inst->index);
        llvm::Type* array_llvm_type = CGF.get_module().get_or_create_type(inst->array->type);

        // Get element type
        llvm::Type* elem_type = CGF.get_module().get_or_create_type(inst->result->type);
        if (auto* ptr_type = inst->result->type->as<PointerType>())
        {
            elem_type = CGF.get_module().get_or_create_type(ptr_type->pointee);
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

    llvm::Value* HLIRCodeGen::gen_pointer_element_addr(CodeGenFunction& CGF, HLIR::ElementAddrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* array = CGF.get_value(inst->array);
        llvm::Value* index = CGF.get_value(inst->index);

        // Debug: Print GEP operation details
        std::cout << "\n=== Pointer GEP Operation Debug ===" << std::endl;
        std::cout << "Array value: ";
        array->print(llvm::outs());
        std::cout << "\nArray value type: ";
        array->getType()->print(llvm::outs());
        std::cout << "\nIndex value: ";
        index->print(llvm::outs());
        std::cout << "\nIndex value type: ";
        index->getType()->print(llvm::outs());
        std::cout << "\nHLIR array type: " << inst->array->type->get_name() << std::endl;

        // Determine the element type
        TypePtr hlir_type = inst->array->type;
        llvm::Type* elem_type = nullptr;

        // If it's a pointer to an array, unwrap it
        if (auto* ptr_type = hlir_type->as<PointerType>())
        {
            std::cout << "Pointer pointee type: " << ptr_type->pointee->get_name() << std::endl;
            hlir_type = ptr_type->pointee;
        }

        // Now hlir_type should be an array type
        if (auto* array_type = hlir_type->as<ArrayType>())
        {
            std::cout << "Array element type: " << array_type->element->get_name() << std::endl;
            elem_type = CGF.get_module().get_or_create_type(array_type->element);
            std::cout << "LLVM element type: ";
            elem_type->print(llvm::outs());
            std::cout << std::endl;
        }

        if (!elem_type)
        {
            throw std::runtime_error("Cannot determine element type for pointer GEP - type is neither pointer nor array");
        }

        auto* result = ir.create_gep(elem_type, array, index, "elem_addr");
        std::cout << "GEP result: ";
        result->print(llvm::outs());
        std::cout << "\nGEP result type: ";
        result->getType()->print(llvm::outs());
        std::cout << "\n=== End Pointer GEP Debug ===\n" << std::endl;

        return result;
    }

    #pragma region Binary Gen

    void HLIRCodeGen::gen_binary(CodeGenFunction& CGF, HLIR::BinaryInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* left = CGF.get_value(inst->left);
        llvm::Value* right = CGF.get_value(inst->right);

        auto props = CGF.get_module().get_type_properties(inst->left->type);

        llvm::Value* result = nullptr;

        // Determine operation category and dispatch
        switch (inst->op)
        {
        case HLIR::Opcode::Add:
        case HLIR::Opcode::Sub:
        case HLIR::Opcode::Mul:
        case HLIR::Opcode::Div:
        case HLIR::Opcode::Rem:
            result = gen_arithmetic_op(ir, inst->op, left, right, props.is_float, props.is_signed);
            break;

        case HLIR::Opcode::Eq:
        case HLIR::Opcode::Ne:
        case HLIR::Opcode::Lt:
        case HLIR::Opcode::Le:
        case HLIR::Opcode::Gt:
        case HLIR::Opcode::Ge:
            result = gen_comparison_op(ir, inst->op, left, right, props.is_float, props.is_signed);
            break;

        case HLIR::Opcode::And:
        case HLIR::Opcode::Or:
        case HLIR::Opcode::BitAnd:
        case HLIR::Opcode::BitOr:
        case HLIR::Opcode::BitXor:
        case HLIR::Opcode::Shl:
        case HLIR::Opcode::Shr:
            result = gen_bitwise_op(ir, inst->op, left, right, props.is_signed);
            break;

        default:
            throw std::runtime_error("Unsupported binary operation");
        }

        CGF.map_value(inst->result, result);
    }

    llvm::Value* HLIRCodeGen::gen_arithmetic_op(LLVMIRBuilder& ir, HLIR::Opcode op,
                                               llvm::Value* left, llvm::Value* right,
                                               bool is_float, bool is_signed)
    {
        switch (op)
        {
        case HLIR::Opcode::Add:
            return ir.create_add(left, right, is_float, "add");
        case HLIR::Opcode::Sub:
            return ir.create_sub(left, right, is_float, "sub");
        case HLIR::Opcode::Mul:
            return ir.create_mul(left, right, is_float, "mul");
        case HLIR::Opcode::Div:
            return ir.create_div(left, right, is_float, is_signed, "div");
        case HLIR::Opcode::Rem:
            return ir.create_rem(left, right, is_float, is_signed, "rem");
        default:
            throw std::runtime_error("Invalid arithmetic operation");
        }
    }

    llvm::Value* HLIRCodeGen::gen_comparison_op(LLVMIRBuilder& ir, HLIR::Opcode op,
                                               llvm::Value* left, llvm::Value* right,
                                               bool is_float, bool is_signed)
    {
        switch (op)
        {
        case HLIR::Opcode::Eq:
            return ir.create_eq(left, right, is_float, "eq");
        case HLIR::Opcode::Ne:
            return ir.create_ne(left, right, is_float, "ne");
        case HLIR::Opcode::Lt:
            return ir.create_lt(left, right, is_float, is_signed, "lt");
        case HLIR::Opcode::Le:
            return ir.create_le(left, right, is_float, is_signed, "le");
        case HLIR::Opcode::Gt:
            return ir.create_gt(left, right, is_float, is_signed, "gt");
        case HLIR::Opcode::Ge:
            return ir.create_ge(left, right, is_float, is_signed, "ge");
        default:
            throw std::runtime_error("Invalid comparison operation");
        }
    }

    llvm::Value* HLIRCodeGen::gen_bitwise_op(LLVMIRBuilder& ir, HLIR::Opcode op,
                                            llvm::Value* left, llvm::Value* right,
                                            bool is_signed)
    {
        switch (op)
        {
        case HLIR::Opcode::And:
        case HLIR::Opcode::BitAnd:
            return ir.create_and(left, right, "and");
        case HLIR::Opcode::Or:
        case HLIR::Opcode::BitOr:
            return ir.create_or(left, right, "or");
        case HLIR::Opcode::BitXor:
            return ir.create_xor(left, right, "xor");
        case HLIR::Opcode::Shl:
            return ir.create_shl(left, right, "shl");
        case HLIR::Opcode::Shr:
            return ir.create_shr(left, right, is_signed, "shr");
        default:
            throw std::runtime_error("Invalid bitwise operation");
        }
    }

    #pragma region Unary Gen

    void HLIRCodeGen::gen_unary(CodeGenFunction& CGF, HLIR::UnaryInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* operand = CGF.get_value(inst->operand);

        auto props = CGF.get_module().get_type_properties(inst->operand->type);

        llvm::Value* result = nullptr;

        switch (inst->op)
        {
        case HLIR::Opcode::Neg:
            result = ir.create_neg(operand, props.is_float, "neg");
            break;
        case HLIR::Opcode::Not:
        case HLIR::Opcode::BitNot:
            result = ir.create_not(operand, "not");
            break;
        default:
            throw std::runtime_error("Unsupported unary operation");
        }

        CGF.map_value(inst->result, result);
    }

    #pragma region Cast Gen

    void HLIRCodeGen::gen_cast(CodeGenFunction& CGF, HLIR::CastInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* value = CGF.get_value(inst->value);
        llvm::Type* target_type = CGF.get_module().get_or_create_type(inst->target_type);

        auto src_props = CGF.get_module().get_type_properties(inst->value->type);
        auto dst_props = CGF.get_module().get_type_properties(inst->target_type);

        llvm::Value* result = nullptr;

        if (src_props.is_float && dst_props.is_float)
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

    void HLIRCodeGen::gen_call(CodeGenFunction& CGF, HLIR::CallInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Function* callee = CGF.get_module().get_function(inst->callee);
        if (!callee)
        {
            throw std::runtime_error("Function not declared: " + inst->callee->name());
        }

        // Collect arguments
        std::vector<llvm::Value*> args;
        for (HLIR::Value* arg : inst->args)
        {
            args.push_back(CGF.get_value(arg));
        }

        // Create call - only name the result if it's not void
        std::string call_name = callee->getReturnType()->isVoidTy() ? "" : "call";
        llvm::Value* call_result = ir.create_call(callee, args, call_name);

        // Map result if not void
        if (inst->result)
        {
            CGF.map_value(inst->result, call_result);
        }
    }

    #pragma region Control Flow Gen

    void HLIRCodeGen::gen_ret(CodeGenFunction& CGF, HLIR::RetInst* inst)
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

    void HLIRCodeGen::gen_br(CodeGenFunction& CGF, HLIR::BrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::BasicBlock* target = CGF.get_block(inst->target);
        ir.create_br(target);
    }

    void HLIRCodeGen::gen_cond_br(CodeGenFunction& CGF, HLIR::CondBrInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Value* cond = CGF.get_value(inst->condition);
        llvm::BasicBlock* true_block = CGF.get_block(inst->true_block);
        llvm::BasicBlock* false_block = CGF.get_block(inst->false_block);
        ir.create_cond_br(cond, true_block, false_block);
    }

    void HLIRCodeGen::gen_phi(CodeGenFunction& CGF, HLIR::PhiInst* inst)
    {
        auto& ir = CGF.get_ir_builder();
        llvm::Type* phi_type = CGF.get_module().get_or_create_type(inst->result->type);
        llvm::PHINode* phi = ir.create_phi(phi_type, inst->incoming.size(), "phi");

        // Register the phi node result immediately so it can be referenced
        CGF.map_value(inst->result, phi);

        // Defer adding incoming values until all blocks are processed
        CGF.add_pending_phi(phi, inst);
    }

    #pragma region Verification

    void HLIRCodeGen::verify_module()
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
