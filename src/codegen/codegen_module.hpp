// codegen_module.hpp - Module-Level Code Generation Context
#pragma once

#include "hlir/hlir.hpp"
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

        // Type mappings
        std::unordered_map<TypePtr, llvm::Type*> type_cache;
        std::unordered_map<HLIR::TypeDefinition*, llvm::StructType*> struct_cache;

        // Function mappings
        std::unordered_map<HLIR::Function*, llvm::Function*> function_cache;

        // Global variables (if needed later)
        std::unordered_map<std::string, llvm::GlobalVariable*> global_cache;

    public:
        CodeGenModule(llvm::LLVMContext& ctx, llvm::Module& mod)
            : context(ctx), module(mod) {}

        // === Type Management ===

        /**
         * @brief Declare all struct types from HLIR module
         */
        void declare_types(HLIR::Module* hlir_module);

        /**
         * @brief Get or create LLVM type for HLIR type
         */
        llvm::Type* get_or_create_type(TypePtr type);

        /**
         * @brief Get struct type for type definition
         */
        llvm::StructType* get_struct_type(HLIR::TypeDefinition* type_def);

        /**
         * @brief Check if type has been declared
         */
        bool has_type(TypePtr type) const;

        // === Function Management ===

        /**
         * @brief Declare all functions from HLIR module
         */
        void declare_functions(HLIR::Module* hlir_module);

        /**
         * @brief Declare a single function
         */
        llvm::Function* declare_function(HLIR::Function* hlir_func);

        /**
         * @brief Get declared LLVM function
         */
        llvm::Function* get_function(HLIR::Function* hlir_func);

        /**
         * @brief Check if function has been declared
         */
        bool has_function(HLIR::Function* hlir_func) const;

        // === Type Utilities ===

        struct TypeProperties
        {
            bool is_float;
            bool is_signed;
            bool is_integer;
            bool is_pointer;
        };

        /**
         * @brief Analyze type properties
         */
        TypeProperties get_type_properties(TypePtr type) const;

        /**
         * @brief Get pointee type if pointer, otherwise return type
         */
        llvm::Type* get_pointee_type_or_self(TypePtr type);

        // Accessors
        llvm::LLVMContext& get_context() { return context; }
        llvm::Module& get_module() { return module; }

    private:
        // Internal helpers
        llvm::FunctionType* get_function_type(HLIR::Function* hlir_func);
        llvm::StructType* declare_struct_type(HLIR::TypeDefinition* type_def);

        // Type property helpers
        bool is_signed_int(TypePtr type) const;
        bool is_unsigned_int(TypePtr type) const;
        bool is_float(TypePtr type) const;
    };

} // namespace Fern
