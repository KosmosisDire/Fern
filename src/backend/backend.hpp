// backend.hpp - Abstract Backend Interface
#pragma once

#include "flir/flir.hpp"
#include "common/error.hpp"
#include <memory>
#include <string>
#include <optional>

namespace Fern
{

enum class BackendType
{
    VM,
    LLVM
};

class Backend : public DiagnosticSystem
{
public:
    Backend(const std::string& name) : DiagnosticSystem(name) {}
    virtual ~Backend() = default;

    Backend(Backend&&) = default;
    Backend& operator=(Backend&&) = default;
    Backend(const Backend&) = delete;
    Backend& operator=(const Backend&) = delete;

    virtual BackendType type() const = 0;

    virtual bool lower(FLIR::Module* flir_module) = 0;

    virtual bool is_valid() const = 0;

    #pragma region Execution

    virtual std::optional<float> execute(const std::string& function_name) = 0;

    #pragma region Output

    virtual bool supports_native_output() const { return false; }
    virtual bool write_object_file(const std::string& filename) const { (void)filename; return false; }
    virtual bool write_assembly(const std::string& filename) const { (void)filename; return false; }

    #pragma region Debug

    virtual void dump() const = 0;
    virtual std::string get_dump_string() const = 0;
};

std::vector<BackendType> get_available_backends();
bool is_backend_available(BackendType type);
std::unique_ptr<Backend> create_backend(BackendType type);

} // namespace Fern
