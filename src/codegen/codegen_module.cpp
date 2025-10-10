// codegen_module.cpp - Module-Level Code Generation Context Implementation
#include "codegen_module.hpp"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <stdexcept>

namespace Fern
{
    // === Type Management ===

    void CodeGenModule::declare_types(HLIR::Module* hlir_module)
    {
        // Declare all struct types first (opaque)
        for (const auto& type_def : hlir_module->types)
        {
            std::string type_name = type_def->symbol->get_qualified_name();
            auto* struct_type = llvm::StructType::create(context, type_name);
            struct_cache[type_def.get()] = struct_type;

            // Also map the TypePtr (shared_ptr) to the struct type
            type_cache[type_def->symbol->type] = struct_type;
        }

        // Now define the struct bodies
        for (const auto& type_def : hlir_module->types)
        {
            auto* struct_type = struct_cache[type_def.get()];

            // Collect field types
            std::vector<llvm::Type*> field_types;
            for (const auto& member : type_def->symbol->member_order)
            {
                if (auto* var_sym = member->as<VariableSymbol>())
                {
                    llvm::Type* field_type = get_or_create_type(var_sym->type);
                    field_types.push_back(field_type);
                }
            }

            // Set the struct body
            if (!field_types.empty())
            {
                struct_type->setBody(field_types);
            }
        }
    }

    llvm::Type* CodeGenModule::get_or_create_type(TypePtr type)
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

        if (auto* prim_type = type->as<PrimitiveType>())
        {
            switch (prim_type->kind)
            {
            case PrimitiveKind::Void:
                llvm_type = llvm::Type::getVoidTy(context);
                break;
            case PrimitiveKind::Bool:
                llvm_type = llvm::Type::getInt1Ty(context);
                break;
            case PrimitiveKind::Char:
            case PrimitiveKind::I8:
            case PrimitiveKind::U8:
                llvm_type = llvm::Type::getInt8Ty(context);
                break;
            case PrimitiveKind::I16:
            case PrimitiveKind::U16:
                llvm_type = llvm::Type::getInt16Ty(context);
                break;
            case PrimitiveKind::I32:
            case PrimitiveKind::U32:
                llvm_type = llvm::Type::getInt32Ty(context);
                break;
            case PrimitiveKind::I64:
            case PrimitiveKind::U64:
                llvm_type = llvm::Type::getInt64Ty(context);
                break;
            case PrimitiveKind::F32:
                llvm_type = llvm::Type::getFloatTy(context);
                break;
            case PrimitiveKind::F64:
                llvm_type = llvm::Type::getDoubleTy(context);
                break;
            default:
                throw std::runtime_error("Unknown primitive type");
            }
        }
        else if (auto* ptr_type = type->as<PointerType>())
        {
            // LLVM 19+ uses opaque pointers - just use ptr type
            llvm_type = llvm::PointerType::get(context, 0);
        }
        else if (auto* array_type = type->as<ArrayType>())
        {
            llvm::Type* elem = get_or_create_type(array_type->element);

            if (array_type->size >= 0)
            {
                // Fixed-size array: [N x T] (stack allocated)
                llvm_type = llvm::ArrayType::get(elem, array_type->size);
            }
            else
            {
                // Dynamic arrays are represented as a struct { i32 length, ptr data }
                std::vector<llvm::Type*> fields = {
                    llvm::Type::getInt32Ty(context),           // length
                    llvm::PointerType::get(context, 0)         // data pointer (opaque)
                };
                llvm_type = llvm::StructType::create(context, fields, "array");
            }
        }
        else if (auto* named_type = type->as<NamedType>())
        {
            // Named types should already be in the map from declare_types
            auto it = type_cache.find(type);
            if (it != type_cache.end())
            {
                llvm_type = it->second;
            }
            else
            {
                throw std::runtime_error("Named type not declared: " + type->get_name());
            }
        }
        else
        {
            throw std::runtime_error("Cannot convert type to LLVM: " + type->get_name());
        }

        // Cache and return
        type_cache[type] = llvm_type;
        return llvm_type;
    }

    llvm::StructType* CodeGenModule::get_struct_type(HLIR::TypeDefinition* type_def)
    {
        auto it = struct_cache.find(type_def);
        if (it != struct_cache.end())
        {
            return it->second;
        }
        return nullptr;
    }

    bool CodeGenModule::has_type(TypePtr type) const
    {
        return type_cache.find(type) != type_cache.end();
    }

    llvm::StructType* CodeGenModule::declare_struct_type(HLIR::TypeDefinition* type_def)
    {
        auto it = struct_cache.find(type_def);
        if (it != struct_cache.end())
        {
            return it->second;
        }

        std::string type_name = type_def->symbol->get_qualified_name();
        auto* struct_type = llvm::StructType::create(context, type_name);
        struct_cache[type_def] = struct_type;
        return struct_type;
    }

    // === Function Management ===

    void CodeGenModule::declare_functions(HLIR::Module* hlir_module)
    {
        for (const auto& hlir_func : hlir_module->functions)
        {
            declare_function(hlir_func.get());
        }
    }

    llvm::Function* CodeGenModule::declare_function(HLIR::Function* hlir_func)
    {
        // Check if already declared
        auto it = function_cache.find(hlir_func);
        if (it != function_cache.end())
        {
            return it->second;
        }

        // Get function type
        llvm::FunctionType* func_type = get_function_type(hlir_func);

        // For external functions, use the simple name (not mangled)
        // For regular functions, use the fully qualified name
        std::string func_name;
        if (hlir_func->is_external && hlir_func->symbol)
        {
            func_name = hlir_func->symbol->name; // Simple name for external linkage
        }
        else
        {
            func_name = hlir_func->name(); // Qualified name for Fern functions
        }

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
            if (param_idx < hlir_func->params.size())
            {
                HLIR::Value* param_value = hlir_func->params[param_idx];
                arg.setName(param_value->debug_name);
            }
            param_idx++;
        }

        // Store mapping
        function_cache[hlir_func] = llvm_func;
        return llvm_func;
    }

    llvm::Function* CodeGenModule::get_function(HLIR::Function* hlir_func)
    {
        auto it = function_cache.find(hlir_func);
        if (it != function_cache.end())
        {
            return it->second;
        }
        return nullptr;
    }

    bool CodeGenModule::has_function(HLIR::Function* hlir_func) const
    {
        return function_cache.find(hlir_func) != function_cache.end();
    }

    llvm::FunctionType* CodeGenModule::get_function_type(HLIR::Function* hlir_func)
    {
        // Return type
        llvm::Type* ret_type = get_or_create_type(hlir_func->return_type());

        // Parameter types - HLIR already includes 'this' parameter explicitly for member functions
        std::vector<llvm::Type*> param_types;
        for (HLIR::Value* param : hlir_func->params)
        {
            llvm::Type* param_type = get_or_create_type(param->type);
            param_types.push_back(param_type);
        }

        return llvm::FunctionType::get(ret_type, param_types, false);
    }

    // === Type Utilities ===

    CodeGenModule::TypeProperties CodeGenModule::get_type_properties(TypePtr type) const
    {
        TypeProperties props;
        props.is_float = is_float(type);
        props.is_signed = is_signed_int(type);
        props.is_integer = is_signed_int(type) || is_unsigned_int(type);
        props.is_pointer = type && type->is<PointerType>();
        return props;
    }

    llvm::Type* CodeGenModule::get_pointee_type_or_self(TypePtr type)
    {
        if (auto* ptr_type = type->as<PointerType>())
        {
            return get_or_create_type(ptr_type->pointee);
        }
        return get_or_create_type(type);
    }

    bool CodeGenModule::is_signed_int(TypePtr type) const
    {
        if (auto* prim = type->as<PrimitiveType>())
        {
            return prim->kind == PrimitiveKind::I8 ||
                   prim->kind == PrimitiveKind::I16 ||
                   prim->kind == PrimitiveKind::I32 ||
                   prim->kind == PrimitiveKind::I64;
        }
        return false;
    }

    bool CodeGenModule::is_unsigned_int(TypePtr type) const
    {
        if (auto* prim = type->as<PrimitiveType>())
        {
            return prim->kind == PrimitiveKind::U8 ||
                   prim->kind == PrimitiveKind::U16 ||
                   prim->kind == PrimitiveKind::U32 ||
                   prim->kind == PrimitiveKind::U64;
        }
        return false;
    }

    bool CodeGenModule::is_float(TypePtr type) const
    {
        if (auto* prim = type->as<PrimitiveType>())
        {
            return prim->kind == PrimitiveKind::F32 ||
                   prim->kind == PrimitiveKind::F64;
        }
        return false;
    }

} // namespace Fern
