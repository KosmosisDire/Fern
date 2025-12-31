// backend.cpp - Backend Factory Implementation
#include "backend/backend.hpp"

#ifdef FERN_VM_AVAILABLE
#include "vm/vm_backend.hpp"
#endif

#ifdef FERN_LLVM_AVAILABLE
#include "llvm/llvm_backend.hpp"
#endif

namespace Fern
{

std::vector<BackendType> get_available_backends()
{
    std::vector<BackendType> backends;

#ifdef FERN_VM_AVAILABLE
    backends.push_back(BackendType::VM);
#endif

#ifdef FERN_LLVM_AVAILABLE
    backends.push_back(BackendType::LLVM);
#endif

    return backends;
}

bool is_backend_available(BackendType type)
{
    switch (type)
    {
    case BackendType::VM:
#ifdef FERN_VM_AVAILABLE
        return true;
#else
        return false;
#endif

    case BackendType::LLVM:
#ifdef FERN_LLVM_AVAILABLE
        return true;
#else
        return false;
#endif
    }
    return false;
}

std::unique_ptr<Backend> create_backend(BackendType type)
{
    switch (type)
    {
    case BackendType::VM:
#ifdef FERN_VM_AVAILABLE
        return std::make_unique<VMBackend>();
#else
        return nullptr;
#endif

    case BackendType::LLVM:
#ifdef FERN_LLVM_AVAILABLE
        return std::make_unique<LLVMBackend>();
#else
        return nullptr;
#endif
    }
    return nullptr;
}

} // namespace Fern
