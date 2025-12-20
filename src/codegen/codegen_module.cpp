// codegen_module.cpp - Module-Level Code Generation Context Implementation
#include "codegen_module.hpp"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <stdexcept>

namespace Fern
{
    // === Type Management ===

    void CodeGenModule::declare_types(FNIR::Module* fnir_module)
    {
        // Declare all struct types from IR type system
        // First pass: create opaque struct types
        for (const auto& ir_struct_ptr : fnir_module->ir_types.get_all_structs())
        {
            FNIR::IRStruct* ir_struct = ir_struct_ptr.get();
            auto* struct_type = llvm::StructType::create(context, ir_struct->name);
            struct_cache[ir_struct] = struct_type;
        }

        // Second pass: define struct bodies
        for (const auto& ir_struct_ptr : fnir_module->ir_types.get_all_structs())
        {
            FNIR::IRStruct* ir_struct = ir_struct_ptr.get();
            auto* struct_type = struct_cache[ir_struct];

            // Collect field types
            std::vector<llvm::Type*> field_types;
            for (const auto& field : ir_struct->fields)
            {
                llvm::Type* field_type = get_or_create_type(field.type);
                field_types.push_back(field_type);
            }

            // Set the struct body
            if (!field_types.empty())
            {
                struct_type->setBody(field_types);
            }
        }
    }

    llvm::Type* CodeGenModule::get_or_create_type(FNIR::IRTypePtr type)
    {
        if (!type)
        {
            return llvm::Type::getVoidTy(context);
        }

        // Check cache first
        auto it = type_cache.find(type);
        if (it != type_cache.end())
        {
            return it->second;
        }

        llvm::Type* llvm_type = nullptr;

        switch (type->kind)
        {
        case FNIR::IRTypeKind::Void:
            llvm_type = llvm::Type::getVoidTy(context);
            break;

        case FNIR::IRTypeKind::Bool:
            llvm_type = llvm::Type::getInt1Ty(context);
            break;

        case FNIR::IRTypeKind::Int:
            switch (type->bit_width)
            {
            case 8:
                llvm_type = llvm::Type::getInt8Ty(context);
                break;
            case 16:
                llvm_type = llvm::Type::getInt16Ty(context);
                break;
            case 32:
                llvm_type = llvm::Type::getInt32Ty(context);
                break;
            case 64:
                llvm_type = llvm::Type::getInt64Ty(context);
                break;
            default:
                llvm_type = llvm::Type::getIntNTy(context, type->bit_width);
                break;
            }
            break;

        case FNIR::IRTypeKind::Float:
            if (type->bit_width == 32)
            {
                llvm_type = llvm::Type::getFloatTy(context);
            }
            else if (type->bit_width == 64)
            {
                llvm_type = llvm::Type::getDoubleTy(context);
            }
            else
            {
                throw std::runtime_error("Unsupported float bit width");
            }
            break;

        case FNIR::IRTypeKind::Pointer:
            // LLVM 19+ uses opaque pointers
            llvm_type = llvm::PointerType::get(context, 0);
            break;

        case FNIR::IRTypeKind::Array:
            if (type->array_size >= 0)
            {
                // Fixed-size array
                llvm::Type* elem_type = get_or_create_type(type->element);
                llvm_type = llvm::ArrayType::get(elem_type, type->array_size);
            }
            else
            {
                // Dynamic array - represent as pointer for now
                llvm_type = llvm::PointerType::get(context, 0);
            }
            break;

        case FNIR::IRTypeKind::Struct:
            if (type->struct_def)
            {
                auto sit = struct_cache.find(type->struct_def);
                if (sit != struct_cache.end())
                {
                    llvm_type = sit->second;
                }
                else
                {
                    throw std::runtime_error("Struct not declared: " + type->struct_def->name);
                }
            }
            else
            {
                throw std::runtime_error("Struct type with null definition");
            }
            break;

        default:
            throw std::runtime_error("Unknown IR type kind");
        }

        // Cache and return
        type_cache[type] = llvm_type;
        return llvm_type;
    }

    llvm::StructType* CodeGenModule::get_struct_type(FNIR::IRStruct* ir_struct)
    {
        auto it = struct_cache.find(ir_struct);
        if (it != struct_cache.end())
        {
            return it->second;
        }
        return nullptr;
    }

    bool CodeGenModule::has_type(FNIR::IRTypePtr type) const
    {
        return type_cache.find(type) != type_cache.end();
    }

    // === Function Management ===

    void CodeGenModule::declare_functions(FNIR::Module* fnir_module)
    {
        for (const auto& fnir_func : fnir_module->functions)
        {
            declare_function(fnir_func.get());
        }
    }

    llvm::Function* CodeGenModule::declare_function(FNIR::Function* fnir_func)
    {
        // Check if already declared
        auto it = function_cache.find(fnir_func);
        if (it != function_cache.end())
        {
            return it->second;
        }

        // Get function type
        llvm::FunctionType* func_type = get_function_type(fnir_func);

        std::string func_name = fnir_func->name();

        // Create function
        llvm::Function* llvm_func = llvm::Function::Create(
            func_type,
            llvm::Function::ExternalLinkage,
            func_name,
            &module);

        // Set parameter names
        size_t param_idx = 0;
        for (auto& arg : llvm_func->args())
        {
            if (param_idx < fnir_func->params.size())
            {
                FNIR::Value* param_value = fnir_func->params[param_idx];
                arg.setName(param_value->debug_name);
            }
            param_idx++;
        }

        // Store mapping
        function_cache[fnir_func] = llvm_func;
        return llvm_func;
    }

    llvm::Function* CodeGenModule::get_function(FNIR::Function* fnir_func)
    {
        auto it = function_cache.find(fnir_func);
        if (it != function_cache.end())
        {
            return it->second;
        }
        return nullptr;
    }

    bool CodeGenModule::has_function(FNIR::Function* fnir_func) const
    {
        return function_cache.find(fnir_func) != function_cache.end();
    }

    llvm::FunctionType* CodeGenModule::get_function_type(FNIR::Function* fnir_func)
    {
        // Return type
        llvm::Type* ret_type = get_or_create_type(fnir_func->return_type);

        // Parameter types - FNIR already includes 'this' parameter explicitly for member functions
        std::vector<llvm::Type*> param_types;
        for (FNIR::Value* param : fnir_func->params)
        {
            llvm::Type* param_type = get_or_create_type(param->type);
            param_types.push_back(param_type);
        }

        return llvm::FunctionType::get(ret_type, param_types, false);
    }

    // === Type Utilities ===

    CodeGenModule::TypeProperties CodeGenModule::get_type_properties(FNIR::IRTypePtr type) const
    {
        TypeProperties props = {};
        if (!type) return props;

        props.is_float = type->is_float();
        props.is_signed = type->is_int() && type->is_signed;
        props.is_integer = type->is_int();
        props.is_pointer = type->is_pointer();

        return props;
    }

    llvm::Type* CodeGenModule::get_pointee_type_or_self(FNIR::IRTypePtr type)
    {
        if (type && type->is_pointer() && type->pointee)
        {
            return get_or_create_type(type->pointee);
        }
        return get_or_create_type(type);
    }

    bool CodeGenModule::is_signed_int(FNIR::IRTypePtr type) const
    {
        return type && type->is_int() && type->is_signed;
    }

    bool CodeGenModule::is_unsigned_int(FNIR::IRTypePtr type) const
    {
        return type && type->is_int() && !type->is_signed;
    }

    bool CodeGenModule::is_float(FNIR::IRTypePtr type) const
    {
        return type && type->is_float();
    }

} // namespace Fern
