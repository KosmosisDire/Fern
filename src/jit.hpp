#pragma once
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Error.h>
#include <memory>
#include <string>
#include <vector>

namespace Fern
{
    class JIT
    {
    private:
        std::unique_ptr<llvm::orc::LLJIT> jit;

    public:
        JIT();
        ~JIT() = default;

        bool add_module(std::unique_ptr<llvm::Module> module,
                        std::unique_ptr<llvm::LLVMContext> context);

        bool load_library(const std::string &path);
        bool load_libraries(const std::vector<std::string> &paths);

        llvm::Expected<llvm::orc::ExecutorAddr> lookup(const std::string &name);

        template <typename FuncType>
        FuncType *get_function(const std::string &name)
        {
            auto addr_or_err = lookup(name);
            
            // Check if lookup returned an error
            if (auto E = addr_or_err.takeError())
            {
                // Handle the error - consume it to avoid assertion failure
                llvm::consumeError(std::move(E));
                return nullptr;
            }
            
            // Get the address value
            auto addr = *addr_or_err;
            
            // Check if the address is valid (non-null)
            // ExecutorAddr can be implicitly converted to uint64_t
            if (!addr)
            {
                // Function not found - address is null
                return nullptr;
            }
            
            // Convert to function pointer
            return addr.toPtr<FuncType *>();
        }
    };
}