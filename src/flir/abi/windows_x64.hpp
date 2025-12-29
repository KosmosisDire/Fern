#pragma once

#include "abi_rules.hpp"

namespace Fern::FLIR::ABI
{

class WindowsX64Rules : public Rules
{
public:
    explicit WindowsX64Rules(IRTypeSystem* type_system);

    Target get_target() const override { return Target::Windows_x64; }
    std::string get_target_name() const override { return "Windows x64"; }

    ArgInfo classify_param(IRTypePtr type) override;
    ArgInfo classify_return(IRTypePtr type) override;

private:
    IRTypeSystem* types;

    IRTypePtr get_coerce_type(size_t size);
    bool can_coerce(size_t size);
};

} // namespace Fern::FLIR::ABI
