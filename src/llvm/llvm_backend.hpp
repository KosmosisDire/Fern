// llvm_backend.hpp - LLVM Backend Interface
#pragma once

#include "backend/backend.hpp"
#include "flir/flir.hpp"
#include "common/logger.hpp"
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <memory>
#include <string>
#include <vector>
#include <optional>
#include <type_traits>

namespace Fern
{

class LLVMBackend : public Backend
{
private:
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
    std::string module_name;

public:
    LLVMBackend();
    ~LLVMBackend() = default;

    LLVMBackend(LLVMBackend&&) = default;
    LLVMBackend& operator=(LLVMBackend&&) = default;
    LLVMBackend(const LLVMBackend&) = delete;
    LLVMBackend& operator=(const LLVMBackend&) = delete;

    BackendType type() const override { return BackendType::LLVM; }

    bool lower(FLIR::Module* flir_module) override;

    bool is_valid() const override { return module != nullptr && !has_errors(); }

    #pragma region Backend Interface
    
    std::optional<float> execute(const std::string& function_name) override;

    bool supports_native_output() const override { return true; }
    bool write_object_file(const std::string& filename) const override;
    bool write_assembly(const std::string& filename) const override;

    void dump() const override;
    std::string get_dump_string() const override;

    #pragma region LLVM-Specific

    bool write_ir(const std::string& filename) const;
    std::string get_ir_string() const;
    void dump_ir() const;

    template<typename ReturnType, typename... Args>
    std::optional<ReturnType> execute_jit(const std::string& function_name,
                                           const std::vector<std::string>& libraries,
                                           Args... args);

    template<typename ReturnType, typename... Args>
    std::optional<ReturnType> execute_jit(const std::string& function_name, Args... args)
    {
        return execute_jit<ReturnType, Args...>(function_name, {}, args...);
    }

    template<typename... Args>
    bool execute_jit_void(const std::string& function_name,
                          const std::vector<std::string>& libraries,
                          Args... args);

    template<typename... Args>
    bool execute_jit_void(const std::string& function_name, Args... args)
    {
        return execute_jit_void<Args...>(function_name, {}, args...);
    }

    llvm::Module* get_module() const { return module.get(); }
    llvm::LLVMContext* get_context() const { return context.get(); }

private:
    static void initialize_targets();
};

} // namespace Fern

#include "llvm_backend.inl"
