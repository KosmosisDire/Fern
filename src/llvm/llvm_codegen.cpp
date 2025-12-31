// llvm_codegen.cpp - FLIR to LLVM IR Lowering Implementation
#include "llvm_codegen.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Constants.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>
#include <iostream>

namespace Fern
{

#pragma region Entry

std::unique_ptr<llvm::Module> FLIRCodeGen::lower(FLIR::Module* flir_module)
{
    if (!flir_module)
    {
        throw std::runtime_error("Cannot lower null FLIR module");
    }

    CodeGenContext ctx(context, *module, *builder);
    ctx.declare_types(flir_module);
    ctx.declare_functions(flir_module);
    generate_function_bodies(ctx, flir_module);
    verify_module();

    return std::move(module);
}

#pragma region High Level

void FLIRCodeGen::generate_function_bodies(CodeGenContext& ctx, FLIR::Module* flir_module)
{
    for (const auto& flir_func : flir_module->functions)
    {
        if (!flir_func->has_valid_symbol())
            continue;

        if (!flir_func->is_external && flir_func->entry)
        {
            generate_function_body(ctx, flir_func.get());
        }
    }
}

void FLIRCodeGen::generate_function_body(CodeGenContext& ctx, FLIR::Function* flir_func)
{
    llvm::Function* llvm_func = ctx.get_function(flir_func);
    if (!llvm_func)
    {
        throw std::runtime_error("Function not declared: " + flir_func->name());
    }

    ctx.begin_function(flir_func, llvm_func);
    ctx.map_parameters();
    ctx.create_all_blocks();

    for (const auto& flir_block : flir_func->blocks)
    {
        generate_basic_block(ctx, flir_block.get());
    }

    ctx.end_function();
}

void FLIRCodeGen::generate_basic_block(CodeGenContext& ctx, FLIR::BasicBlock* flir_block)
{
    llvm::BasicBlock* llvm_block = ctx.get_block(flir_block);
    ctx.ir().SetInsertPoint(llvm_block);

    for (const auto& inst : flir_block->instructions)
    {
        generate_instruction(ctx, inst.get());
    }
}

#pragma region Instruction Gen

void FLIRCodeGen::generate_instruction(CodeGenContext& ctx, FLIR::Instruction* inst)
{
    switch (inst->op)
    {
    case FLIR::Opcode::ConstInt:
        gen_const_int(ctx, static_cast<FLIR::ConstIntInst*>(inst));
        break;
    case FLIR::Opcode::ConstFloat:
        gen_const_float(ctx, static_cast<FLIR::ConstFloatInst*>(inst));
        break;
    case FLIR::Opcode::ConstBool:
        gen_const_bool(ctx, static_cast<FLIR::ConstBoolInst*>(inst));
        break;
    case FLIR::Opcode::ConstString:
        gen_const_string(ctx, static_cast<FLIR::ConstStringInst*>(inst));
        break;
    case FLIR::Opcode::ConstNull:
        gen_const_null(ctx, static_cast<FLIR::ConstNullInst*>(inst));
        break;
    case FLIR::Opcode::StackAlloc:
        gen_stack_alloc(ctx, static_cast<FLIR::StackAllocInst*>(inst));
        break;
    case FLIR::Opcode::StackAllocBytes:
        gen_stack_alloc_bytes(ctx, static_cast<FLIR::StackAllocBytesInst*>(inst));
        break;
    case FLIR::Opcode::HeapAlloc:
        gen_heap_alloc(ctx, static_cast<FLIR::HeapAllocInst*>(inst));
        break;
    case FLIR::Opcode::HeapAllocBytes:
        gen_heap_alloc_bytes(ctx, static_cast<FLIR::HeapAllocBytesInst*>(inst));
        break;
    case FLIR::Opcode::HeapFree:
        gen_heap_free(ctx, static_cast<FLIR::HeapFreeInst*>(inst));
        break;
    case FLIR::Opcode::Load:
        gen_load(ctx, static_cast<FLIR::LoadInst*>(inst));
        break;
    case FLIR::Opcode::Store:
        gen_store(ctx, static_cast<FLIR::StoreInst*>(inst));
        break;
    case FLIR::Opcode::FieldAddr:
        gen_field_addr(ctx, static_cast<FLIR::FieldAddrInst*>(inst));
        break;
    case FLIR::Opcode::ElementAddr:
        gen_element_addr(ctx, static_cast<FLIR::ElementAddrInst*>(inst));
        break;
    case FLIR::Opcode::MemCpy:
        gen_memcpy(ctx, static_cast<FLIR::MemCpyInst*>(inst));
        break;
    case FLIR::Opcode::MemSet:
        gen_memset(ctx, static_cast<FLIR::MemSetInst*>(inst));
        break;
    case FLIR::Opcode::Add:
    case FLIR::Opcode::Sub:
    case FLIR::Opcode::Mul:
    case FLIR::Opcode::Div:
    case FLIR::Opcode::Rem:
    case FLIR::Opcode::Eq:
    case FLIR::Opcode::Ne:
    case FLIR::Opcode::Lt:
    case FLIR::Opcode::Le:
    case FLIR::Opcode::Gt:
    case FLIR::Opcode::Ge:
    case FLIR::Opcode::And:
    case FLIR::Opcode::Or:
    case FLIR::Opcode::BitAnd:
    case FLIR::Opcode::BitOr:
    case FLIR::Opcode::BitXor:
    case FLIR::Opcode::ShiftL:
    case FLIR::Opcode::ShiftR:
        gen_binary(ctx, static_cast<FLIR::BinaryInst*>(inst));
        break;
    case FLIR::Opcode::Neg:
    case FLIR::Opcode::Not:
    case FLIR::Opcode::BitNot:
        gen_unary(ctx, static_cast<FLIR::UnaryInst*>(inst));
        break;
    case FLIR::Opcode::Cast:
        gen_cast(ctx, static_cast<FLIR::CastInst*>(inst));
        break;
    case FLIR::Opcode::Call:
        gen_call(ctx, static_cast<FLIR::CallInst*>(inst));
        break;
    case FLIR::Opcode::Ret:
        gen_ret(ctx, static_cast<FLIR::RetInst*>(inst));
        break;
    case FLIR::Opcode::Br:
        gen_br(ctx, static_cast<FLIR::BrInst*>(inst));
        break;
    case FLIR::Opcode::CondBr:
        gen_cond_br(ctx, static_cast<FLIR::CondBrInst*>(inst));
        break;
    default:
        throw std::runtime_error("Unsupported FLIR opcode: " +
            Fern::FLIR::to_string(inst->op));
    }
}

#pragma region Constant Gen

void FLIRCodeGen::gen_const_int(CodeGenContext& ctx, FLIR::ConstIntInst* inst)
{
    llvm::Type* type = ctx.get_type(inst->result->type);

    if (!type->isIntegerTy())
    {
        type = llvm::Type::getInt32Ty(context);
    }

    llvm::Value* const_val = llvm::ConstantInt::get(type, inst->value, true);
    ctx.map_value(inst->result, const_val);
}

void FLIRCodeGen::gen_const_float(CodeGenContext& ctx, FLIR::ConstFloatInst* inst)
{
    llvm::Type* type = ctx.get_type(inst->result->type);
    llvm::Value* const_val = llvm::ConstantFP::get(type, inst->value);
    ctx.map_value(inst->result, const_val);
}

void FLIRCodeGen::gen_const_bool(CodeGenContext& ctx, FLIR::ConstBoolInst* inst)
{
    llvm::Value* const_val = llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), inst->value ? 1 : 0);
    ctx.map_value(inst->result, const_val);
}

void FLIRCodeGen::gen_const_string(CodeGenContext& ctx, FLIR::ConstStringInst* inst)
{
    llvm::Value* str_ptr = ctx.create_global_string(inst->value, ".str");
    ctx.map_value(inst->result, str_ptr);
}

void FLIRCodeGen::gen_const_null(CodeGenContext& ctx, FLIR::ConstNullInst* inst)
{
    llvm::Type* type = ctx.get_type(inst->null_type);
    llvm::Value* null_val = llvm::Constant::getNullValue(type);
    ctx.map_value(inst->result, null_val);
}

#pragma region Memory Gen

void FLIRCodeGen::gen_stack_alloc(CodeGenContext& ctx, FLIR::StackAllocInst* inst)
{
    llvm::Type* alloc_type;

    if (inst->alloc_type->is_array() && inst->alloc_type->array_size >= 0)
    {
        llvm::Type* elem_type = ctx.get_type(inst->alloc_type->element);
        alloc_type = llvm::ArrayType::get(elem_type, inst->alloc_type->array_size);
    }
    else
    {
        alloc_type = ctx.get_type(inst->alloc_type);
    }

    std::string name = inst->result->debug_name.empty() ? "stack_alloc" : inst->result->debug_name;
    llvm::Value* ptr = ctx.create_entry_alloca(alloc_type, name);

    llvm::Constant* zero = llvm::Constant::getNullValue(alloc_type);
    ctx.ir().CreateStore(zero, ptr);

    ctx.map_value(inst->result, ptr);
}

void FLIRCodeGen::gen_stack_alloc_bytes(CodeGenContext& ctx, FLIR::StackAllocBytesInst* inst)
{
    llvm::Value* size_val = ctx.get_value(inst->size);

    std::string name = inst->result->debug_name.empty() ? "stack_bytes" : inst->result->debug_name;
    llvm::Type* i8_type = llvm::Type::getInt8Ty(context);
    llvm::Value* ptr = ctx.ir().CreateAlloca(i8_type, size_val, name);
    ctx.map_value(inst->result, ptr);
}

void FLIRCodeGen::gen_heap_alloc(CodeGenContext& ctx, FLIR::HeapAllocInst* inst)
{
    llvm::Type* alloc_type;

    if (inst->alloc_type->is_array() && inst->alloc_type->array_size >= 0)
    {
        llvm::Type* elem_type = ctx.get_type(inst->alloc_type->element);
        alloc_type = llvm::ArrayType::get(elem_type, inst->alloc_type->array_size);
    }
    else
    {
        alloc_type = ctx.get_type(inst->alloc_type);
    }

    std::string name = inst->result->debug_name.empty() ? "heap_alloc" : inst->result->debug_name;
    llvm::Value* ptr = ctx.create_malloc(alloc_type, name);
    ctx.map_value(inst->result, ptr);
}

void FLIRCodeGen::gen_heap_alloc_bytes(CodeGenContext& ctx, FLIR::HeapAllocBytesInst* inst)
{
    llvm::Value* size_val = ctx.get_value(inst->size);

    std::string name = inst->result->debug_name.empty() ? "heap_bytes" : inst->result->debug_name;
    llvm::Value* ptr = ctx.create_malloc_bytes(size_val, name);
    ctx.map_value(inst->result, ptr);
}

void FLIRCodeGen::gen_heap_free(CodeGenContext& ctx, FLIR::HeapFreeInst* inst)
{
    llvm::Value* ptr_val = ctx.get_value(inst->ptr);
    ctx.create_free(ptr_val);
}

void FLIRCodeGen::gen_memcpy(CodeGenContext& ctx, FLIR::MemCpyInst* inst)
{
    llvm::Value* dest = ctx.get_value(inst->dest);
    llvm::Value* src = ctx.get_value(inst->src);
    llvm::Value* size = ctx.get_value(inst->size);

    ctx.ir().CreateMemCpy(dest, llvm::MaybeAlign(), src, llvm::MaybeAlign(), size, inst->is_volatile);
}

void FLIRCodeGen::gen_memset(CodeGenContext& ctx, FLIR::MemSetInst* inst)
{
    llvm::Value* dest = ctx.get_value(inst->dest);
    llvm::Value* value = ctx.get_value(inst->value);
    llvm::Value* size = ctx.get_value(inst->size);

    ctx.ir().CreateMemSet(dest, value, size, llvm::MaybeAlign(), inst->is_volatile);
}

void FLIRCodeGen::gen_load(CodeGenContext& ctx, FLIR::LoadInst* inst)
{
    llvm::Value* addr = ctx.get_value(inst->address);

    if (!addr->getType()->isPointerTy())
    {
        std::string err = "Load instruction expects pointer, but got: ";
        llvm::raw_string_ostream os(err);
        addr->getType()->print(os);
        os << "\n  Address FLIR value: %" << inst->address->id;
        throw std::runtime_error(err);
    }

    llvm::Type* load_type = ctx.get_type(inst->result->type);
    std::string name = inst->result->debug_name.empty() ? "load" : inst->result->debug_name;
    llvm::Value* loaded = ctx.ir().CreateLoad(load_type, addr, name);
    ctx.map_value(inst->result, loaded);
}

void FLIRCodeGen::gen_store(CodeGenContext& ctx, FLIR::StoreInst* inst)
{
    llvm::Value* val = ctx.get_value(inst->value);
    llvm::Value* addr = ctx.get_value(inst->address);

    if (!addr->getType()->isPointerTy())
    {
        std::string err = "Store instruction expects pointer address, but got: ";
        llvm::raw_string_ostream os(err);
        addr->getType()->print(os);
        throw std::runtime_error(err);
    }

    ctx.ir().CreateStore(val, addr);
}

void FLIRCodeGen::gen_field_addr(CodeGenContext& ctx, FLIR::FieldAddrInst* inst)
{
    llvm::Value* obj_ptr = ctx.get_value(inst->object);

    if (!obj_ptr->getType()->isPointerTy())
    {
        throw std::runtime_error("FieldAddr requires pointer to struct");
    }

    FLIR::IRTypePtr obj_type = inst->object->type;
    if (!obj_type->is_pointer())
    {
        throw std::runtime_error("FieldAddr object must be pointer type in FLIR");
    }

    FLIR::IRTypePtr pointee_type = obj_type->pointee;
    if (!pointee_type || !pointee_type->struct_def)
    {
        throw std::runtime_error("FieldAddr pointee must be struct type");
    }

    llvm::Type* struct_type = ctx.get_struct_type(pointee_type->struct_def);

    llvm::Value* field_ptr = ctx.ir().CreateStructGEP(
        struct_type, obj_ptr, inst->field_index,
        inst->result->debug_name.empty() ? "field_addr" : inst->result->debug_name);

    ctx.map_value(inst->result, field_ptr);
}

void FLIRCodeGen::gen_element_addr(CodeGenContext& ctx, FLIR::ElementAddrInst* inst)
{
    FLIR::IRTypePtr array_type = inst->array->type;

    if (array_type->is_pointer())
    {
        FLIR::IRTypePtr pointee = array_type->pointee;
        if (pointee && pointee->is_array())
        {
            llvm::Value* result = gen_fixed_array_element_addr(ctx, inst);
            ctx.map_value(inst->result, result);
        }
        else
        {
            llvm::Value* result = gen_pointer_element_addr(ctx, inst);
            ctx.map_value(inst->result, result);
        }
    }
    else if (array_type->is_array())
    {
        llvm::Value* result = gen_fixed_array_element_addr(ctx, inst);
        ctx.map_value(inst->result, result);
    }
    else
    {
        throw std::runtime_error("ElementAddr requires array or pointer type");
    }
}

llvm::Value* FLIRCodeGen::gen_fixed_array_element_addr(CodeGenContext& ctx, FLIR::ElementAddrInst* inst)
{
    llvm::Value* array_ptr = ctx.get_value(inst->array);
    llvm::Value* index = ctx.get_value(inst->index);

    FLIR::IRTypePtr array_type = inst->array->type;
    FLIR::IRTypePtr pointee = array_type->is_pointer() ? array_type->pointee : array_type;

    llvm::Type* elem_type = ctx.get_type(pointee->element);
    llvm::Type* arr_type = llvm::ArrayType::get(elem_type, pointee->array_size);

    llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 0);
    std::string name = inst->result->debug_name.empty() ? "elem_addr" : inst->result->debug_name;

    return ctx.ir().CreateGEP(arr_type, array_ptr, {zero, index}, name);
}

llvm::Value* FLIRCodeGen::gen_dynamic_array_element_addr(CodeGenContext& ctx, FLIR::ElementAddrInst* inst)
{
    llvm::Value* array_ptr = ctx.get_value(inst->array);
    llvm::Value* index = ctx.get_value(inst->index);

    FLIR::IRTypePtr array_type = inst->array->type;
    FLIR::IRTypePtr elem_flir_type = array_type->element;
    llvm::Type* elem_type = ctx.get_type(elem_flir_type);

    std::string name = inst->result->debug_name.empty() ? "dyn_elem_addr" : inst->result->debug_name;
    return ctx.ir().CreateGEP(elem_type, array_ptr, index, name);
}

llvm::Value* FLIRCodeGen::gen_pointer_element_addr(CodeGenContext& ctx, FLIR::ElementAddrInst* inst)
{
    llvm::Value* ptr = ctx.get_value(inst->array);
    llvm::Value* index = ctx.get_value(inst->index);

    FLIR::IRTypePtr ptr_type = inst->array->type;
    FLIR::IRTypePtr elem_flir_type = ptr_type->pointee;
    llvm::Type* elem_type = ctx.get_type(elem_flir_type);

    std::string name = inst->result->debug_name.empty() ? "ptr_elem" : inst->result->debug_name;
    return ctx.ir().CreateGEP(elem_type, ptr, index, name);
}

#pragma region Binary Gen

void FLIRCodeGen::gen_binary(CodeGenContext& ctx, FLIR::BinaryInst* inst)
{
    llvm::Value* left = ctx.get_value(inst->left);
    llvm::Value* right = ctx.get_value(inst->right);

    FLIR::IRTypePtr operand_type = inst->left->type;
    bool is_float = ctx.is_float(operand_type);
    bool is_signed = ctx.is_signed(operand_type);

    llvm::Value* result = nullptr;

    switch (inst->op)
    {
    case FLIR::Opcode::Add:
    case FLIR::Opcode::Sub:
    case FLIR::Opcode::Mul:
    case FLIR::Opcode::Div:
    case FLIR::Opcode::Rem:
        result = gen_arithmetic_op(ctx, inst->op, left, right, is_float, is_signed);
        break;

    case FLIR::Opcode::Eq:
    case FLIR::Opcode::Ne:
    case FLIR::Opcode::Lt:
    case FLIR::Opcode::Le:
    case FLIR::Opcode::Gt:
    case FLIR::Opcode::Ge:
        result = gen_comparison_op(ctx, inst->op, left, right, is_float, is_signed);
        break;

    case FLIR::Opcode::And:
        result = ctx.ir().CreateAnd(left, right, "and");
        break;
    case FLIR::Opcode::Or:
        result = ctx.ir().CreateOr(left, right, "or");
        break;

    case FLIR::Opcode::BitAnd:
    case FLIR::Opcode::BitOr:
    case FLIR::Opcode::BitXor:
    case FLIR::Opcode::ShiftL:
    case FLIR::Opcode::ShiftR:
        result = gen_bitwise_op(ctx, inst->op, left, right, is_signed);
        break;

    default:
        throw std::runtime_error("Unsupported binary opcode: " + FLIR::to_string(inst->op));
    }

    ctx.map_value(inst->result, result);
}

llvm::Value* FLIRCodeGen::gen_arithmetic_op(CodeGenContext& ctx, FLIR::Opcode op,
                                            llvm::Value* left, llvm::Value* right,
                                            bool is_float, bool is_signed)
{
    switch (op)
    {
    case FLIR::Opcode::Add:
        return is_float ? ctx.ir().CreateFAdd(left, right, "fadd")
                        : ctx.ir().CreateAdd(left, right, "add");
    case FLIR::Opcode::Sub:
        return is_float ? ctx.ir().CreateFSub(left, right, "fsub")
                        : ctx.ir().CreateSub(left, right, "sub");
    case FLIR::Opcode::Mul:
        return is_float ? ctx.ir().CreateFMul(left, right, "fmul")
                        : ctx.ir().CreateMul(left, right, "mul");
    case FLIR::Opcode::Div:
        if (is_float)
            return ctx.ir().CreateFDiv(left, right, "fdiv");
        return is_signed ? ctx.ir().CreateSDiv(left, right, "sdiv")
                         : ctx.ir().CreateUDiv(left, right, "udiv");
    case FLIR::Opcode::Rem:
        if (is_float)
            return ctx.ir().CreateFRem(left, right, "frem");
        return is_signed ? ctx.ir().CreateSRem(left, right, "srem")
                         : ctx.ir().CreateURem(left, right, "urem");
    default:
        throw std::runtime_error("Invalid arithmetic opcode");
    }
}

llvm::Value* FLIRCodeGen::gen_comparison_op(CodeGenContext& ctx, FLIR::Opcode op,
                                            llvm::Value* left, llvm::Value* right,
                                            bool is_float, bool is_signed)
{
    if (is_float)
    {
        switch (op)
        {
        case FLIR::Opcode::Eq: return ctx.ir().CreateFCmpOEQ(left, right, "feq");
        case FLIR::Opcode::Ne: return ctx.ir().CreateFCmpONE(left, right, "fne");
        case FLIR::Opcode::Lt: return ctx.ir().CreateFCmpOLT(left, right, "flt");
        case FLIR::Opcode::Le: return ctx.ir().CreateFCmpOLE(left, right, "fle");
        case FLIR::Opcode::Gt: return ctx.ir().CreateFCmpOGT(left, right, "fgt");
        case FLIR::Opcode::Ge: return ctx.ir().CreateFCmpOGE(left, right, "fge");
        default: throw std::runtime_error("Invalid float comparison opcode");
        }
    }
    else if (is_signed)
    {
        switch (op)
        {
        case FLIR::Opcode::Eq: return ctx.ir().CreateICmpEQ(left, right, "eq");
        case FLIR::Opcode::Ne: return ctx.ir().CreateICmpNE(left, right, "ne");
        case FLIR::Opcode::Lt: return ctx.ir().CreateICmpSLT(left, right, "slt");
        case FLIR::Opcode::Le: return ctx.ir().CreateICmpSLE(left, right, "sle");
        case FLIR::Opcode::Gt: return ctx.ir().CreateICmpSGT(left, right, "sgt");
        case FLIR::Opcode::Ge: return ctx.ir().CreateICmpSGE(left, right, "sge");
        default: throw std::runtime_error("Invalid signed comparison opcode");
        }
    }
    else
    {
        switch (op)
        {
        case FLIR::Opcode::Eq: return ctx.ir().CreateICmpEQ(left, right, "eq");
        case FLIR::Opcode::Ne: return ctx.ir().CreateICmpNE(left, right, "ne");
        case FLIR::Opcode::Lt: return ctx.ir().CreateICmpULT(left, right, "ult");
        case FLIR::Opcode::Le: return ctx.ir().CreateICmpULE(left, right, "ule");
        case FLIR::Opcode::Gt: return ctx.ir().CreateICmpUGT(left, right, "ugt");
        case FLIR::Opcode::Ge: return ctx.ir().CreateICmpUGE(left, right, "uge");
        default: throw std::runtime_error("Invalid unsigned comparison opcode");
        }
    }
}

llvm::Value* FLIRCodeGen::gen_bitwise_op(CodeGenContext& ctx, FLIR::Opcode op,
                                         llvm::Value* left, llvm::Value* right,
                                         bool is_signed)
{
    switch (op)
    {
    case FLIR::Opcode::BitAnd:
        return ctx.ir().CreateAnd(left, right, "bitand");
    case FLIR::Opcode::BitOr:
        return ctx.ir().CreateOr(left, right, "bitor");
    case FLIR::Opcode::BitXor:
        return ctx.ir().CreateXor(left, right, "bitxor");
    case FLIR::Opcode::ShiftL:
        return ctx.ir().CreateShl(left, right, "shl");
    case FLIR::Opcode::ShiftR:
        return is_signed ? ctx.ir().CreateAShr(left, right, "ashr")
                         : ctx.ir().CreateLShr(left, right, "lshr");
    default:
        throw std::runtime_error("Invalid bitwise opcode");
    }
}

#pragma region Unary Gen

void FLIRCodeGen::gen_unary(CodeGenContext& ctx, FLIR::UnaryInst* inst)
{
    llvm::Value* operand = ctx.get_value(inst->operand);
    llvm::Value* result = nullptr;

    FLIR::IRTypePtr operand_type = inst->operand->type;
    bool is_float = ctx.is_float(operand_type);

    switch (inst->op)
    {
    case FLIR::Opcode::Neg:
        result = is_float ? ctx.ir().CreateFNeg(operand, "fneg")
                          : ctx.ir().CreateNeg(operand, "neg");
        break;
    case FLIR::Opcode::Not:
        result = ctx.ir().CreateNot(operand, "not");
        break;
    case FLIR::Opcode::BitNot:
        result = ctx.ir().CreateNot(operand, "bitnot");
        break;
    default:
        throw std::runtime_error("Unsupported unary opcode: " + FLIR::to_string(inst->op));
    }

    ctx.map_value(inst->result, result);
}

#pragma region Cast Gen

void FLIRCodeGen::gen_cast(CodeGenContext& ctx, FLIR::CastInst* inst)
{
    llvm::Value* val = ctx.get_value(inst->value);
    llvm::Type* target_type = ctx.get_type(inst->target_type);

    FLIR::IRTypePtr src_type = inst->value->type;
    FLIR::IRTypePtr dst_type = inst->target_type;

    bool src_float = ctx.is_float(src_type);
    bool dst_float = ctx.is_float(dst_type);
    bool src_signed = ctx.is_signed(src_type);
    bool dst_signed = ctx.is_signed(dst_type);
    bool src_ptr = ctx.is_pointer(src_type);
    bool dst_ptr = ctx.is_pointer(dst_type);
    bool src_int = ctx.is_integer(src_type);
    bool dst_int = ctx.is_integer(dst_type);

    llvm::Value* result = nullptr;
    std::string name = inst->result->debug_name.empty() ? "cast" : inst->result->debug_name;

    if (src_ptr && dst_ptr)
    {
        result = ctx.ir().CreateBitCast(val, target_type, name);
    }
    else if (src_ptr && dst_int)
    {
        result = ctx.ir().CreatePtrToInt(val, target_type, name);
    }
    else if (src_int && dst_ptr)
    {
        result = ctx.ir().CreateIntToPtr(val, target_type, name);
    }
    else if (src_float && dst_float)
    {
        unsigned src_bits = val->getType()->getPrimitiveSizeInBits();
        unsigned dst_bits = target_type->getPrimitiveSizeInBits();
        result = (dst_bits > src_bits) ? ctx.ir().CreateFPExt(val, target_type, name)
                                       : ctx.ir().CreateFPTrunc(val, target_type, name);
    }
    else if (src_float && dst_int)
    {
        result = dst_signed ? ctx.ir().CreateFPToSI(val, target_type, name)
                            : ctx.ir().CreateFPToUI(val, target_type, name);
    }
    else if (src_int && dst_float)
    {
        result = src_signed ? ctx.ir().CreateSIToFP(val, target_type, name)
                            : ctx.ir().CreateUIToFP(val, target_type, name);
    }
    else if (src_int && dst_int)
    {
        unsigned src_bits = val->getType()->getIntegerBitWidth();
        unsigned dst_bits = target_type->getIntegerBitWidth();

        if (dst_bits > src_bits)
        {
            result = src_signed ? ctx.ir().CreateSExt(val, target_type, name)
                                : ctx.ir().CreateZExt(val, target_type, name);
        }
        else if (dst_bits < src_bits)
        {
            result = ctx.ir().CreateTrunc(val, target_type, name);
        }
        else
        {
            result = val;
        }
    }
    else
    {
        result = ctx.ir().CreateBitCast(val, target_type, name);
    }

    ctx.map_value(inst->result, result);
}

#pragma region Call Gen

void FLIRCodeGen::gen_call(CodeGenContext& ctx, FLIR::CallInst* inst)
{
    llvm::Function* callee = ctx.get_function(inst->callee);
    if (!callee)
    {
        throw std::runtime_error("Function not found: " + inst->callee->name());
    }

    std::vector<llvm::Value*> args;
    args.reserve(inst->args.size());

    for (FLIR::Value* arg : inst->args)
    {
        args.push_back(ctx.get_value(arg));
    }

    std::string name = "";
    if (inst->result && !callee->getReturnType()->isVoidTy())
    {
        name = inst->result->debug_name.empty() ? "call" : inst->result->debug_name;
    }

    llvm::Value* result = ctx.ir().CreateCall(callee, args, name);

    if (inst->result)
    {
        ctx.map_value(inst->result, result);
    }
}

#pragma region Control Flow Gen

void FLIRCodeGen::gen_ret(CodeGenContext& ctx, FLIR::RetInst* inst)
{
    if (inst->value)
    {
        llvm::Value* ret_val = ctx.get_value(inst->value);
        ctx.ir().CreateRet(ret_val);
    }
    else
    {
        ctx.ir().CreateRetVoid();
    }
}

void FLIRCodeGen::gen_br(CodeGenContext& ctx, FLIR::BrInst* inst)
{
    llvm::BasicBlock* target = ctx.get_block(inst->target);
    ctx.ir().CreateBr(target);
}

void FLIRCodeGen::gen_cond_br(CodeGenContext& ctx, FLIR::CondBrInst* inst)
{
    llvm::Value* cond = ctx.get_value(inst->condition);
    llvm::BasicBlock* true_bb = ctx.get_block(inst->true_block);
    llvm::BasicBlock* false_bb = ctx.get_block(inst->false_block);

    ctx.ir().CreateCondBr(cond, true_bb, false_bb);
}

#pragma region Verification

void FLIRCodeGen::verify_module()
{
    std::string error_str;
    llvm::raw_string_ostream error_stream(error_str);

    if (llvm::verifyModule(*module, &error_stream))
    {
        error("LLVM module verification failed:\n" + error_str, SourceRange());
    }
}

} // namespace Fern
