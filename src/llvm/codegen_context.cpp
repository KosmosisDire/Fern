// codegen_context.cpp - Unified Code Generation Context Implementation
#include "codegen_context.hpp"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <sstream>
#include <stdexcept>

namespace Fern
{

#pragma region Module Setup

void CodeGenContext::declare_types(FLIR::Module* flir_module)
{
    // First pass: create opaque struct types
    for (const auto& ir_struct_ptr : flir_module->ir_types.get_all_structs())
    {
        FLIR::IRStruct* ir_struct = ir_struct_ptr.get();
        auto* struct_type = llvm::StructType::create(context, ir_struct->name);
        struct_cache[ir_struct] = struct_type;
    }

    // Second pass: define struct bodies
    for (const auto& ir_struct_ptr : flir_module->ir_types.get_all_structs())
    {
        FLIR::IRStruct* ir_struct = ir_struct_ptr.get();
        auto* struct_type = struct_cache[ir_struct];

        std::vector<llvm::Type*> field_types;
        for (const auto& field : ir_struct->fields)
        {
            field_types.push_back(get_type(field.type));
        }

        if (!field_types.empty())
        {
            struct_type->setBody(field_types);
        }
    }
}

void CodeGenContext::declare_functions(FLIR::Module* flir_module)
{
    for (const auto& flir_func : flir_module->functions)
    {
        declare_function(flir_func.get());
    }
}

#pragma region Type Management

llvm::Type* CodeGenContext::get_type(FLIR::IRTypePtr type)
{
    if (!type)
        return llvm::Type::getVoidTy(context);

    auto it = type_cache.find(type);
    if (it != type_cache.end())
        return it->second;

    llvm::Type* llvm_type = nullptr;

    switch (type->kind)
    {
    case FLIR::IRTypeKind::Void:
        llvm_type = llvm::Type::getVoidTy(context);
        break;

    case FLIR::IRTypeKind::Bool:
        llvm_type = llvm::Type::getInt1Ty(context);
        break;

    case FLIR::IRTypeKind::Int:
        switch (type->bit_width)
        {
        case 8:  llvm_type = llvm::Type::getInt8Ty(context); break;
        case 16: llvm_type = llvm::Type::getInt16Ty(context); break;
        case 32: llvm_type = llvm::Type::getInt32Ty(context); break;
        case 64: llvm_type = llvm::Type::getInt64Ty(context); break;
        default: llvm_type = llvm::Type::getIntNTy(context, type->bit_width); break;
        }
        break;

    case FLIR::IRTypeKind::Float:
        if (type->bit_width == 16)
            llvm_type = llvm::Type::getHalfTy(context);
        else if (type->bit_width == 32)
            llvm_type = llvm::Type::getFloatTy(context);
        else if (type->bit_width == 64)
            llvm_type = llvm::Type::getDoubleTy(context);
        else
            throw std::runtime_error("Unsupported float bit width: " + std::to_string(type->bit_width));
        break;

    case FLIR::IRTypeKind::Pointer:
        llvm_type = llvm::PointerType::get(context, 0);
        break;

    case FLIR::IRTypeKind::Array:
        if (type->array_size >= 0)
        {
            llvm::Type* elem_type = get_type(type->element);
            llvm_type = llvm::ArrayType::get(elem_type, type->array_size);
        }
        else
        {
            llvm_type = llvm::PointerType::get(context, 0);
        }
        break;

    case FLIR::IRTypeKind::Struct:
        if (type->struct_def)
        {
            auto sit = struct_cache.find(type->struct_def);
            if (sit != struct_cache.end())
                llvm_type = sit->second;
            else
                throw std::runtime_error("Struct not declared: " + type->struct_def->name);
        }
        else
        {
            throw std::runtime_error("Struct type with null definition");
        }
        break;

    default:
        throw std::runtime_error("Unknown IR type kind");
    }

    type_cache[type] = llvm_type;
    return llvm_type;
}

llvm::StructType* CodeGenContext::get_struct_type(FLIR::IRStruct* ir_struct)
{
    auto it = struct_cache.find(ir_struct);
    return (it != struct_cache.end()) ? it->second : nullptr;
}

#pragma region Function Management

llvm::Function* CodeGenContext::declare_function(FLIR::Function* flir_func)
{
    if (!flir_func || !flir_func->has_valid_symbol())
        return nullptr;

    auto it = function_cache.find(flir_func);
    if (it != function_cache.end())
        return it->second;

    llvm::FunctionType* func_type = get_function_type(flir_func);
    llvm::Function* llvm_func = llvm::Function::Create(
        func_type,
        llvm::Function::ExternalLinkage,
        flir_func->name(),
        &llvm_module);

    size_t param_idx = 0;
    for (auto& arg : llvm_func->args())
    {
        if (param_idx < flir_func->params.size())
            arg.setName(flir_func->params[param_idx]->debug_name);
        param_idx++;
    }

    function_cache[flir_func] = llvm_func;
    return llvm_func;
}

llvm::Function* CodeGenContext::get_function(FLIR::Function* flir_func)
{
    auto it = function_cache.find(flir_func);
    return (it != function_cache.end()) ? it->second : nullptr;
}

llvm::FunctionType* CodeGenContext::get_function_type(FLIR::Function* flir_func)
{
    llvm::Type* ret_type = get_type(flir_func->return_type);

    std::vector<llvm::Type*> param_types;
    for (FLIR::Value* param : flir_func->params)
    {
        param_types.push_back(get_type(param->type));
    }

    return llvm::FunctionType::get(ret_type, param_types, false);
}

void CodeGenContext::begin_function(FLIR::Function* flir_func, llvm::Function* llvm_func)
{
    value_map.clear();
    while (!loop_stack.empty()) loop_stack.pop();
    block_stack.clear();
    current_flir_func = flir_func;
    current_llvm_func = llvm_func;
}

void CodeGenContext::end_function()
{
    value_map.clear();
    while (!loop_stack.empty()) loop_stack.pop();
    block_stack.clear();
    current_flir_func = nullptr;
    current_llvm_func = nullptr;
}

#pragma region Value Management

llvm::Value* CodeGenContext::get_value(FLIR::Value* flir_value)
{
    auto it = value_map.find(flir_value);
    if (it == value_map.end())
        throw std::runtime_error(format_value_error(flir_value, "FLIR value not found"));
    return it->second;
}

void CodeGenContext::map_value(FLIR::Value* flir_value, llvm::Value* llvm_value)
{
    value_map[flir_value] = llvm_value;
}

void CodeGenContext::map_parameters()
{
    size_t arg_idx = 0;
    for (auto& arg : current_llvm_func->args())
    {
        if (arg_idx < current_flir_func->params.size())
            map_value(current_flir_func->params[arg_idx], &arg);
        arg_idx++;
    }
}

#pragma region Loop Stack Management

void CodeGenContext::push_loop(llvm::BasicBlock* continue_bb, llvm::BasicBlock* break_bb)
{
    loop_stack.push({continue_bb, break_bb});
}

void CodeGenContext::pop_loop()
{
    if (!loop_stack.empty())
        loop_stack.pop();
}

LoopContext& CodeGenContext::current_loop()
{
    if (loop_stack.empty())
        throw std::runtime_error("No active loop for break/continue");
    return loop_stack.top();
}

#pragma region Block Stack Management

void CodeGenContext::push_block(llvm::BasicBlock* exit_bb, bool is_loop)
{
    block_stack.push_back({exit_bb, is_loop});
}

void CodeGenContext::pop_block()
{
    if (!block_stack.empty())
        block_stack.pop_back();
}

llvm::BasicBlock* CodeGenContext::get_break_target(uint32_t depth)
{
    if (depth >= block_stack.size())
        return nullptr;

    size_t index = block_stack.size() - 1 - depth;
    return block_stack[index].exit_bb;
}

#pragma region IR Generation Helpers

llvm::BasicBlock* CodeGenContext::create_block(const std::string& name)
{
    return llvm::BasicBlock::Create(context, name, current_llvm_func);
}

llvm::Value* CodeGenContext::create_entry_alloca(llvm::Type* type, const std::string& name)
{
    llvm::BasicBlock* entry_block = &current_llvm_func->getEntryBlock();
    auto saved_point = builder.saveIP();

    builder.SetInsertPoint(entry_block, entry_block->getFirstInsertionPt());

    llvm::Align align = llvm_module.getDataLayout().getABITypeAlign(type);
    llvm::AllocaInst* alloca = builder.CreateAlloca(type, nullptr, name);
    alloca->setAlignment(align);

    builder.restoreIP(saved_point);
    return alloca;
}

llvm::Value* CodeGenContext::create_malloc(llvm::Type* type, const std::string& name)
{
    llvm::Type* i64 = llvm::Type::getInt64Ty(context);
    llvm::Type* ptr = llvm::PointerType::get(context, 0);

    llvm::Value* count = llvm::ConstantInt::get(i64, 1);
    llvm::Value* size = llvm::ConstantInt::get(i64, llvm_module.getDataLayout().getTypeAllocSize(type));

    llvm::FunctionType* calloc_type = llvm::FunctionType::get(ptr, {i64, i64}, false);
    llvm::FunctionCallee calloc_func = llvm_module.getOrInsertFunction("calloc", calloc_type);

    return builder.CreateCall(calloc_func, {count, size}, name);
}

llvm::Value* CodeGenContext::create_malloc_bytes(llvm::Value* size, const std::string& name)
{
    llvm::Type* i64 = llvm::Type::getInt64Ty(context);
    llvm::Type* ptr = llvm::PointerType::get(context, 0);

    if (size->getType() != i64)
        size = builder.CreateZExtOrTrunc(size, i64, "size_ext");

    llvm::Value* count = llvm::ConstantInt::get(i64, 1);

    llvm::FunctionType* calloc_type = llvm::FunctionType::get(ptr, {i64, i64}, false);
    llvm::FunctionCallee calloc_func = llvm_module.getOrInsertFunction("calloc", calloc_type);

    return builder.CreateCall(calloc_func, {count, size}, name);
}

void CodeGenContext::create_free(llvm::Value* ptr)
{
    llvm::Type* void_ty = llvm::Type::getVoidTy(context);
    llvm::Type* ptr_ty = llvm::PointerType::get(context, 0);

    llvm::FunctionType* free_type = llvm::FunctionType::get(void_ty, {ptr_ty}, false);
    llvm::FunctionCallee free_func = llvm_module.getOrInsertFunction("free", free_type);

    builder.CreateCall(free_func, {ptr});
}

llvm::Value* CodeGenContext::create_global_string(const std::string& str, const std::string& name)
{
    llvm::Constant* str_const = llvm::ConstantDataArray::getString(context, str);
    llvm::GlobalVariable* global_str = new llvm::GlobalVariable(
        llvm_module,
        str_const->getType(),
        true,
        llvm::GlobalValue::PrivateLinkage,
        str_const,
        name);

    llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
    llvm::Value* indices[] = {zero, zero};

    return builder.CreateInBoundsGEP(str_const->getType(), global_str, indices, "str");
}

#pragma region Error Helpers

std::string CodeGenContext::format_value_error(FLIR::Value* value, const std::string& message)
{
    std::stringstream ss;
    ss << message << ": %" << value->id;
    if (!value->debug_name.empty())
        ss << " <" << value->debug_name << ">";
    ss << " : " << (value->type ? value->type->get_name() : "null");
    return ss.str();
}

} // namespace Fern
