#pragma once

#include "abi_info.hpp"
#include "../ir_type.hpp"
#include <memory>
#include <string>

namespace Fern::FLIR::ABI
{

#pragma region Target Enum

enum class Target
{
    Windows_x64,
    SystemV_x64,    // Linux, macOS, BSD on x86-64
    ARM64,          // Apple Silicon, Linux ARM64
    WASM32,         // WebAssembly 32-bit
    WASM64,         // WebAssembly 64-bit
};

#pragma region Rules Interface

class Rules
{
public:
    virtual ~Rules() = default;

    virtual Target get_target() const = 0;
    virtual std::string get_target_name() const = 0;

    // Classify how a parameter should be passed
    virtual ArgInfo classify_param(IRTypePtr type) = 0;

    // Classify how a return value should be passed
    virtual ArgInfo classify_return(IRTypePtr type) = 0;

    // Convenience: classify an entire function signature
    FunctionABIInfo classify_function(IRTypePtr return_type, const std::vector<IRTypePtr>& param_types)
    {
        FunctionABIInfo info;
        info.return_info = classify_return(return_type);
        info.param_infos.reserve(param_types.size());
        for (auto param_type : param_types)
            info.param_infos.push_back(classify_param(param_type));
        return info;
    }
};

#pragma region Factory

std::unique_ptr<Rules> create_rules_for_target(Target target, IRTypeSystem* type_system);

// Helper to detect host target
Target get_host_target();

} // namespace Fern::FLIR::ABI
