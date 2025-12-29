// codegen_module.cpp - Module-Level Code Generation Context Implementation
#include "codegen_module.hpp"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <stdexcept>

namespace Fern
{
    #pragma region Type Management

    void CodeGenModule::declare_types(FLIR::Module* flir_module)
    {
        // Declare all struct types from IR type system
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

    llvm::Type* CodeGenModule::get_or_create_type(FLIR::IRTypePtr type)
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
        case FLIR::IRTypeKind::Void:
            llvm_type = llvm::Type::getVoidTy(context);
            break;

        case FLIR::IRTypeKind::Bool:
            llvm_type = llvm::Type::getInt1Ty(context);
            break;

        case FLIR::IRTypeKind::Int:
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

        case FLIR::IRTypeKind::Float:
            if (type->bit_width == 16)
            {
                llvm_type = llvm::Type::getHalfTy(context);
            }
            else if (type->bit_width == 32)
            {
                llvm_type = llvm::Type::getFloatTy(context);
            }
            else if (type->bit_width == 64)
            {
                llvm_type = llvm::Type::getDoubleTy(context);
            }
            else
            {
                throw std::runtime_error("Unsupported float bit width: " + std::to_string(type->bit_width));
            }
            break;

        case FLIR::IRTypeKind::Pointer:
            // LLVM 19+ uses opaque pointers
            llvm_type = llvm::PointerType::get(context, 0);
            break;

        case FLIR::IRTypeKind::Array:
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

        case FLIR::IRTypeKind::Struct:
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

    llvm::StructType* CodeGenModule::get_struct_type(FLIR::IRStruct* ir_struct)
    {
        auto it = struct_cache.find(ir_struct);
        if (it != struct_cache.end())
        {
            return it->second;
        }
        return nullptr;
    }

    bool CodeGenModule::has_type(FLIR::IRTypePtr type) const
    {
        return type_cache.find(type) != type_cache.end();
    }

    #pragma region Function Management

    void CodeGenModule::declare_functions(FLIR::Module* flir_module)
    {
        for (const auto& flir_func : flir_module->functions)
        {
            declare_function(flir_func.get());
        }
    }

    llvm::Function* CodeGenModule::declare_function(FLIR::Function* flir_func)
    {
        // Check if already declared
        auto it = function_cache.find(flir_func);
        if (it != function_cache.end())
        {
            return it->second;
        }

        // Get function type
        llvm::FunctionType* func_type = get_function_type(flir_func);

        std::string func_name = flir_func->name();

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
            if (param_idx < flir_func->params.size())
            {
                FLIR::Value* param_value = flir_func->params[param_idx];
                arg.setName(param_value->debug_name);
            }
            param_idx++;
        }

        // Store mapping
        function_cache[flir_func] = llvm_func;
        return llvm_func;
    }

    llvm::Function* CodeGenModule::get_function(FLIR::Function* flir_func)
    {
        auto it = function_cache.find(flir_func);
        if (it != function_cache.end())
        {
            return it->second;
        }
        return nullptr;
    }

    bool CodeGenModule::has_function(FLIR::Function* flir_func) const
    {
        return function_cache.find(flir_func) != function_cache.end();
    }

    llvm::FunctionType* CodeGenModule::get_function_type(FLIR::Function* flir_func)
    {
        // Return type
        llvm::Type* ret_type = get_or_create_type(flir_func->return_type);

        // Parameter types - FLIR already includes 'this' parameter explicitly for member functions
        std::vector<llvm::Type*> param_types;
        for (FLIR::Value* param : flir_func->params)
        {
            llvm::Type* param_type = get_or_create_type(param->type);
            param_types.push_back(param_type);
        }

        return llvm::FunctionType::get(ret_type, param_types, false);
    }

    #pragma region Type Utilities

    CodeGenModule::TypeProperties CodeGenModule::get_type_properties(FLIR::IRTypePtr type) const
    {
        TypeProperties props = {};
        if (!type) return props;

        props.is_float = type->is_float();
        props.is_signed = type->is_int() && type->is_signed;
        props.is_integer = type->is_int();
        props.is_pointer = type->is_pointer();

        return props;
    }

    llvm::Type* CodeGenModule::get_pointee_type_or_self(FLIR::IRTypePtr type)
    {
        if (type && type->is_pointer() && type->pointee)
        {
            return get_or_create_type(type->pointee);
        }
        return get_or_create_type(type);
    }

    bool CodeGenModule::is_signed_int(FLIR::IRTypePtr type) const
    {
        return type && type->is_int() && type->is_signed;
    }

    bool CodeGenModule::is_unsigned_int(FLIR::IRTypePtr type) const
    {
        return type && type->is_int() && !type->is_signed;
    }

    bool CodeGenModule::is_float(FLIR::IRTypePtr type) const
    {
        return type && type->is_float();
    }

} // namespace Fern
