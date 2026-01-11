#include "vm_backend.hpp"
#include <iostream>

namespace Fern
{

VMBackend::VMBackend()
    : Backend("VMBackend")
{
}

bool VMBackend::lower(FLIR::Module* flir_module)
{
    if (!flir_module)
    {
        error("Cannot lower null FLIR module", SourceRange());
        return false;
    }

    module = flir_module;
    return true;
}

std::optional<float> VMBackend::execute(const std::string& function_name)
{
    (void)function_name;
    if (!is_valid()) return std::nullopt;
    return 0.0f;
}

void VMBackend::dump() const
{
    std::cout << get_dump_string() << std::endl;
}

std::string VMBackend::get_dump_string() const
{
    if (!module) return "(no module)";
    return module->dump();
}

} // namespace Fern
