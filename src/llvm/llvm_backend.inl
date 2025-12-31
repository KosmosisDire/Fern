// llvm_backend.inl - Template implementations for LLVMBackend
#pragma once

#include "jit.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Utils/Cloning.h>

namespace Fern
{

    template<typename ReturnType, typename... Args>
    std::optional<ReturnType> LLVMBackend::execute_jit(const std::string& function_name,
                                                        const std::vector<std::string>& libraries,
                                                        Args... args)
    {
        static_assert(!std::is_void_v<ReturnType>, "Use execute_jit_void for void functions");

        if (!is_valid())
        {
            LOG_ERROR("Cannot execute: module is invalid.", LogCategory::JIT);
            for (const auto& error : get_diagnostics())
            {
                LOG_ERROR("  - " + error.message, LogCategory::JIT);
            }
            return std::nullopt;
        }

        std::string verify_error;
        llvm::raw_string_ostream error_stream(verify_error);
        if (llvm::verifyModule(*module, &error_stream))
        {
            LOG_ERROR("Module verification failed:\n" + verify_error, LogCategory::JIT);
            return std::nullopt;
        }

        JIT jit;

        if (!jit.load_libraries(libraries))
        {
            LOG_ERROR("Failed to load dynamic libraries", LogCategory::JIT);
            return std::nullopt;
        }

        auto cloned_module = llvm::CloneModule(*module);
        auto jit_context = std::make_unique<llvm::LLVMContext>();

        if (!jit.add_module(std::move(cloned_module), std::move(jit_context)))
        {
            LOG_ERROR("Failed to add module to JIT", LogCategory::JIT);
            return std::nullopt;
        }

        using FuncType = ReturnType(Args...);
        auto func = jit.get_function<FuncType>(function_name);

        if (!func)
        {
            LOG_ERROR("Failed to find function: " + function_name, LogCategory::JIT);
            return std::nullopt;
        }

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

    template<typename... Args>
    bool LLVMBackend::execute_jit_void(const std::string& function_name,
                                        const std::vector<std::string>& libraries,
                                        Args... args)
    {
        if (!is_valid())
        {
            LOG_ERROR("Cannot execute: module is invalid.", LogCategory::JIT);
            for (const auto& error : get_diagnostics())
            {
                LOG_ERROR("  - " + error.message, LogCategory::JIT);
            }
            return false;
        }

        std::string verify_error;
        llvm::raw_string_ostream error_stream(verify_error);
        if (llvm::verifyModule(*module, &error_stream))
        {
            LOG_ERROR("Module verification failed:\n" + verify_error, LogCategory::JIT);
            return false;
        }

        JIT jit;

        if (!jit.load_libraries(libraries))
        {
            LOG_ERROR("Failed to load dynamic libraries", LogCategory::JIT);
            return false;
        }

        auto cloned_module = llvm::CloneModule(*module);
        auto jit_context = std::make_unique<llvm::LLVMContext>();

        if (!jit.add_module(std::move(cloned_module), std::move(jit_context)))
        {
            LOG_ERROR("Failed to add module to JIT", LogCategory::JIT);
            return false;
        }

        using FuncType = void(Args...);
        auto func = jit.get_function<FuncType>(function_name);

        if (!func)
        {
            LOG_ERROR("Failed to find function: " + function_name, LogCategory::JIT);
            return false;
        }

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
