// codegen_module.hpp - Module-Level Code Generation Context
#pragma once

#include "fnir/fnir.hpp"
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
        std::unordered_map<FNIR::IRTypePtr, llvm::Type*> type_cache;
        std::unordered_map<FNIR::IRStruct*, llvm::StructType*> struct_cache;

        // Function mappings
        std::unordered_map<FNIR::Function*, llvm::Function*> function_cache;

        // Global variables (if needed later)
        std::unordered_map<std::string, llvm::GlobalVariable*> global_cache;

    public:
        CodeGenModule(llvm::LLVMContext& ctx, llvm::Module& mod)
            : context(ctx), module(mod) {}

        // === Type Management ===

        /**
         * @brief Declare all struct types from FNIR module
         */
        void declare_types(FNIR::Module* fnir_module);

        /**
         * @brief Get or create LLVM type for IRType
         */
        llvm::Type* get_or_create_type(FNIR::IRTypePtr type);

        /**
         * @brief Get struct type for IR struct
         */
        llvm::StructType* get_struct_type(FNIR::IRStruct* ir_struct);

        /**
         * @brief Check if type has been declared
         */
        bool has_type(FNIR::IRTypePtr type) const;

        // === Function Management ===

        /**
         * @brief Declare all functions from FNIR module
         */
        void declare_functions(FNIR::Module* fnir_module);

        /**
         * @brief Declare a single function
         */
        llvm::Function* declare_function(FNIR::Function* fnir_func);

        /**
         * @brief Get declared LLVM function
         */
        llvm::Function* get_function(FNIR::Function* fnir_func);

        /**
         * @brief Check if function has been declared
         */
        bool has_function(FNIR::Function* fnir_func) const;

        // === Type Utilities ===

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
        TypeProperties get_type_properties(FNIR::IRTypePtr type) const;

        /**
         * @brief Get pointee type if pointer, otherwise return type
         */
        llvm::Type* get_pointee_type_or_self(FNIR::IRTypePtr type);

        // Accessors
        llvm::LLVMContext& get_context() { return context; }
        llvm::Module& get_module() { return module; }

    private:
        // Internal helpers
        llvm::FunctionType* get_function_type(FNIR::Function* fnir_func);

        // Type property helpers
        bool is_signed_int(FNIR::IRTypePtr type) const;
        bool is_unsigned_int(FNIR::IRTypePtr type) const;
        bool is_float(FNIR::IRTypePtr type) const;
    };

} // namespace Fern
