// codegen_module.hpp - Module-Level Code Generation Context
#pragma once

#include "flir/flir.hpp"
#include "semantic/type.hpp"
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <unordered_map>
#include <string>

namespace Fern
{
    /**
     * @brief Manages module-level code generation state
     *
     * Handles type declarations, function declarations, and module-wide mappings.
     * Lives for the duration of module lowering.
     */
    class CodeGenModule
    {
    private:
        llvm::LLVMContext& context;
        llvm::Module& module;

        // Type mappings - keyed by IRTypePtr for IR type to LLVM type mapping
        std::unordered_map<FLIR::IRTypePtr, llvm::Type*> type_cache;
        std::unordered_map<FLIR::IRStruct*, llvm::StructType*> struct_cache;

        // Function mappings
        std::unordered_map<FLIR::Function*, llvm::Function*> function_cache;

        // Global variables (if needed later)
        std::unordered_map<std::string, llvm::GlobalVariable*> global_cache;

    public:
        CodeGenModule(llvm::LLVMContext& ctx, llvm::Module& mod)
            : context(ctx), module(mod) {}

        #pragma region Type Management
        void declare_types(FLIR::Module* flir_module);
        llvm::Type* get_or_create_type(FLIR::IRTypePtr type);
        llvm::StructType* get_struct_type(FLIR::IRStruct* ir_struct);
        bool has_type(FLIR::IRTypePtr type) const;

        #pragma region Function Management
        void declare_functions(FLIR::Module* flir_module);
        llvm::Function* declare_function(FLIR::Function* flir_func);
        llvm::Function* get_function(FLIR::Function* flir_func);
        bool has_function(FLIR::Function* flir_func) const;

        #pragma region Type Utilities

        struct TypeProperties
        {
            bool is_float;
            bool is_signed;
            bool is_integer;
            bool is_pointer;
        };

        /**
         * @brief Analyze type properties from IRType
         */
        TypeProperties get_type_properties(FLIR::IRTypePtr type) const;

        /**
         * @brief Get pointee type if pointer, otherwise return type
         */
        llvm::Type* get_pointee_type_or_self(FLIR::IRTypePtr type);

        // Accessors
        llvm::LLVMContext& get_context() { return context; }
        llvm::Module& get_module() { return module; }

    private:
        // Internal helpers
        llvm::FunctionType* get_function_type(FLIR::Function* flir_func);

        // Type property helpers
        bool is_signed_int(FLIR::IRTypePtr type) const;
        bool is_unsigned_int(FLIR::IRTypePtr type) const;
        bool is_float(FLIR::IRTypePtr type) const;
    };

} // namespace Fern
