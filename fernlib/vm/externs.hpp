#pragma once

#include <cstdint>
#include <memory>
#include <unordered_map>
#include <vector>

#include <vm/value.hpp>

namespace Fern
{

class Interpreter;
class VmMemory;
struct MethodSymbol;
struct TypeSymbol;

// Calls @Extern functions through libffi. The C symbol is looked up in the running process and the call
// is prepared once per method. Without libffi every bind errors.
class ExternCalls
{
public:
    ExternCalls(Interpreter& vm, VmMemory& memory);
    ~ExternCalls();

    // Looks the C function up and prepares the call. Errors when the symbol is missing.
    void bind(MethodSymbol* method);
    Value call(MethodSymbol* method, const std::vector<Value>& args, bool hasResultDest, uint64_t resultDest);

private:
    struct Binding;
    Binding& binding_for(MethodSymbol* method);

    Interpreter& vm;
    VmMemory& memory;
    std::unordered_map<MethodSymbol*, std::unique_ptr<Binding>> bindings;
};

}
