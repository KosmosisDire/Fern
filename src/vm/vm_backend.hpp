#pragma once

#include "backend/backend.hpp"

namespace Fern
{

class VMBackend : public Backend
{
private:
    FLIR::Module* module = nullptr;

public:
    VMBackend();
    ~VMBackend() = default;

    VMBackend(VMBackend&&) = default;
    VMBackend& operator=(VMBackend&&) = default;

    BackendType type() const override { return BackendType::VM; }

    bool lower(FLIR::Module* flir_module) override;

    bool is_valid() const override { return module != nullptr && !has_errors(); }

    std::optional<float> execute(const std::string& function_name) override;

    void dump() const override;
    std::string get_dump_string() const override;

    FLIR::Module* get_module() { return module; }
};

} // namespace Fern
