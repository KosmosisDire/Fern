#pragma once
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <memory>
#include <string>
#include <vector>
#include <optional>
#include <iostream>
#include <type_traits>
#include "jit.hpp"
#include "common/logger.hpp"
#include "common/error.hpp"
#include "common/source_database.hpp"

namespace Fern
{
    // Forward declaration
    class JIT;

    class CompiledModule : public DiagnosticSystem
    {
    private:
        std::unique_ptr<llvm::LLVMContext> context;
        std::unique_ptr<llvm::Module> module;
        std::string module_name;
        std::shared_ptr<SourceDatabase> source_db;

    public:
        CompiledModule() : context(nullptr), module(nullptr), DiagnosticSystem("CompiledModule") {}

        CompiledModule(const std::vector<Diagnostic> &diagnostics,
                       std::shared_ptr<SourceDatabase> source_db = nullptr)
            : context(nullptr), module(nullptr),
              DiagnosticSystem("CompiledModule", diagnostics),
              source_db(std::move(source_db)) {}

        CompiledModule(std::unique_ptr<llvm::LLVMContext> ctx,
                       std::unique_ptr<llvm::Module> mod,
                       const std::string &name,
                       const std::vector<Diagnostic> &diagnostics = {},
                       std::shared_ptr<SourceDatabase> source_db = nullptr)
            : context(std::move(ctx)),
              module(std::move(mod)),
              module_name(name),
              DiagnosticSystem("CompiledModule", diagnostics),
              source_db(std::move(source_db)) {}

        // Move-only type
        CompiledModule(CompiledModule &&) = default;
        CompiledModule &operator=(CompiledModule &&) = default;
        CompiledModule(const CompiledModule &) = delete;
        CompiledModule &operator=(const CompiledModule &) = delete;

        // Check if compilation succeeded
        bool is_valid() const { return module != nullptr && !has_errors(); }

        // Format a diagnostic with filename context from the source database
        std::string format_diagnostic(const Diagnostic &diag) const
        {
            if (source_db)
            {
                auto filename = source_db->get_path(diag.location.start.file_id);
                if (!filename.empty())
                    return diag.to_string(filename);
            }
            return diag.to_string();
        }

        // Output options
        bool write_ir(const std::string &filename) const;
        bool write_object_file(const std::string &filename) const;
        bool write_assembly(const std::string &filename) const;
        
        // Generic JIT execution for any return type and function signature
        template<typename ReturnType, typename... Args>
        std::optional<ReturnType> execute_jit(const std::string &function_name, Args... args);
        
        // Specialization for void return type
        template<typename... Args>
        bool execute_jit_void(const std::string &function_name, Args... args);

        // Get LLVM IR as string
        std::string get_ir_string() const;

        // For debugging
        void dump_ir() const;
        
        // Get raw module pointer (for advanced use)
        llvm::Module* get_module() const { return module.get(); }
        llvm::LLVMContext* get_context() const { return context.get(); }
    };

    // Template implementation (must be in header)
    template<typename ReturnType, typename... Args>
    std::optional<ReturnType> CompiledModule::execute_jit(const std::string &function_name, Args... args)
    {
        static_assert(!std::is_void_v<ReturnType>, "Use execute_jit_void for void functions");
        
        if (!is_valid())
        {
            LOG_ERROR("Cannot execute: module is invalid.", LogCategory::JIT);
            for (const auto &error : get_diagnostics())
            {
                LOG_ERROR("  - " + error.message, LogCategory::JIT);
            }
            return std::nullopt;
        }

        // Verify module before JIT execution
        std::string verify_error;
        llvm::raw_string_ostream error_stream(verify_error);
        if (llvm::verifyModule(*module, &error_stream))
        {
            LOG_ERROR("Module verification failed:\n" + verify_error, LogCategory::JIT);
            return std::nullopt;
        }

        // Create JIT instance
        JIT jit;

        // Clone the module to preserve the original
        auto cloned_module = llvm::CloneModule(*module);
        
        // The cloned module needs its own context for the JIT
        auto jit_context = std::make_unique<llvm::LLVMContext>();

        // Move ownership to JIT
        if (!jit.add_module(std::move(cloned_module), std::move(jit_context)))
        {
            LOG_ERROR("Failed to add module to JIT", LogCategory::JIT);
            return std::nullopt;
        }

        // Get function pointer using JIT's get_function template
        using FuncType = ReturnType(Args...);
        auto func = jit.get_function<FuncType>(function_name);
        
        if (!func)
        {
            LOG_ERROR("Failed to find function: " + function_name, LogCategory::JIT);
            return std::nullopt;
        }

        // Execute function with provided arguments
        try
        {
            ReturnType result = func(args...);
            return result;
        }
        catch (...)
        {
            LOG_ERROR("Exception during JIT execution", LogCategory::JIT);
            return std::nullopt;
        }
    }

    // Template implementation for void functions
    template<typename... Args>
    bool CompiledModule::execute_jit_void(const std::string &function_name, Args... args)
    {
        if (!is_valid())
        {
            LOG_ERROR("Cannot execute: module is invalid.", LogCategory::JIT);
            for (const auto &error : get_diagnostics())
            {
                LOG_ERROR("  - " + error.message, LogCategory::JIT);
            }
            return false;
        }

        // Verify module before JIT execution
        std::string verify_error;
        llvm::raw_string_ostream error_stream(verify_error);
        if (llvm::verifyModule(*module, &error_stream))
        {
            LOG_ERROR("Module verification failed:\n" + verify_error, LogCategory::JIT);
            return false;
        }

        // Create JIT instance
        JIT jit;

        // Clone the module to preserve the original
        auto cloned_module = llvm::CloneModule(*module);
        
        // The cloned module needs its own context for the JIT
        auto jit_context = std::make_unique<llvm::LLVMContext>();

        // Move ownership to JIT
        if (!jit.add_module(std::move(cloned_module), std::move(jit_context)))
        {
            LOG_ERROR("Failed to add module to JIT", LogCategory::JIT);
            return false;
        }

        // Get function pointer using JIT's get_function template
        using FuncType = void(Args...);
        auto func = jit.get_function<FuncType>(function_name);
        
        if (!func)
        {
            LOG_ERROR("Failed to find function: " + function_name, LogCategory::JIT);
            return false;
        }

        // Execute function
        try
        {
            func(args...);
            return true;
        }
        catch (...)
        {
            LOG_ERROR("Exception during JIT execution", LogCategory::JIT);
            return false;
        }
    }

} // namespace Fern
