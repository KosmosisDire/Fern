#include "abi_rules.hpp"
#include "windows_x64.hpp"
#include <stdexcept>

namespace Fern::FLIR::ABI
{

#pragma region Factory

std::unique_ptr<Rules> create_rules_for_target(Target target, IRTypeSystem* type_system)
{
    switch (target)
    {
        case Target::Windows_x64:
            return std::make_unique<WindowsX64Rules>(type_system);

        case Target::SystemV_x64:
            throw std::runtime_error("System V x64 ABI not yet implemented");

        case Target::ARM64:
            throw std::runtime_error("ARM64 ABI not yet implemented");

        case Target::WASM32:
            throw std::runtime_error("WASM32 ABI not yet implemented");

        default:
            throw std::runtime_error("Unknown ABI target");
    }
}

#pragma region Host Detection

Target get_host_target()
{
#if defined(_WIN32) && defined(_M_X64)
    return Target::Windows_x64;
#elif defined(__linux__) && defined(__x86_64__)
    return Target::SystemV_x64;
#elif defined(__APPLE__) && defined(__x86_64__)
    return Target::SystemV_x64;
#elif defined(__APPLE__) && defined(__aarch64__)
    return Target::ARM64;
#elif defined(__linux__) && defined(__aarch64__)
    return Target::ARM64;
#else
    // Default fallback - will likely fail at runtime
    return Target::Windows_x64;
#endif
}

} // namespace Fern::FLIR::ABI
