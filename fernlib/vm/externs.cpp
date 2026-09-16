#include <vm/externs.hpp>

#include <algorithm>
#include <cstring>
#include <format>
#include <string>

#include <semantic/symbol/symbol.hpp>
#include <vm/memory.hpp>
#include <vm/vm.hpp>

#ifdef FERN_LIBFFI_AVAILABLE
#include <ffi.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#define PSAPI_VERSION 2
#include <windows.h>
#include <psapi.h>
#else
#include <dlfcn.h>
#endif
#endif

namespace Fern
{

#ifdef FERN_LIBFFI_AVAILABLE

#pragma region Binding

// A value type crossing by value, described to libffi field by field so it computes the C layout
struct StructType
{
    ffi_type type{};
    std::vector<ffi_type*> elements;
};

struct ExternCalls::Binding
{
    void* function = nullptr;
    ffi_cif cif{};
    std::vector<ffi_type*> argTypes;
    // A value type argument arrives as the address of its bytes, which is what libffi wants for a struct
    std::vector<bool> argIsStruct;
    std::vector<std::unique_ptr<StructType>> structs;
    bool hasReturn = false;
    bool returnsStruct = false;
    Value::Kind returnKind = Value::Kind::I32;
};

// The file name a platform gives a library called name, keeping any directory in front of it
static std::string platform_library_name(const std::string& name)
{
    size_t slash = name.find_last_of("/\\");
    std::string directory = slash == std::string::npos ? "" : name.substr(0, slash + 1);
    std::string file = slash == std::string::npos ? name : name.substr(slash + 1);
#if defined(_WIN32)
    return std::format("{}{}.dll", directory, file);
#elif defined(__APPLE__)
    return std::format("{}lib{}.dylib", directory, file);
#else
    return std::format("{}lib{}.so", directory, file);
#endif
}

void* ExternCalls::load_library(const std::string& name)
{
    auto it = libraries.find(name);
    if (it != libraries.end()) return it->second;

    void* handle = nullptr;
    for (const std::string& candidate : {name, platform_library_name(name)})
    {
#ifdef _WIN32
        handle = reinterpret_cast<void*>(LoadLibraryA(candidate.c_str()));
#else
        handle = dlopen(candidate.c_str(), RTLD_NOW);
#endif
        if (handle) break;
    }
    libraries.emplace(name, handle);
    return handle;
}

// The symbol from the library, or with no library from any module the process has loaded, so the C
// runtime is always in reach
static void* find_symbol(void* library, const std::string& name)
{
#ifdef _WIN32
    if (library) return reinterpret_cast<void*>(GetProcAddress(static_cast<HMODULE>(library), name.c_str()));

    HMODULE modules[1024];
    DWORD needed = 0;
    if (!EnumProcessModules(GetCurrentProcess(), modules, sizeof(modules), &needed)) return nullptr;
    DWORD count = std::min<DWORD>(needed / sizeof(HMODULE), 1024);
    for (DWORD i = 0; i < count; i++)
    {
        if (FARPROC address = GetProcAddress(modules[i], name.c_str())) return reinterpret_cast<void*>(address);
    }
    return nullptr;
#else
    return dlsym(library ? library : RTLD_DEFAULT, name.c_str());
#endif
}

static ffi_type* scalar_type(Value::Kind kind)
{
    switch (kind)
    {
        case Value::Kind::I8:   return &ffi_type_sint8;
        case Value::Kind::I16:  return &ffi_type_sint16;
        case Value::Kind::I32:  return &ffi_type_sint32;
        case Value::Kind::I64:  return &ffi_type_sint64;
        case Value::Kind::U8:   return &ffi_type_uint8;
        case Value::Kind::U16:  return &ffi_type_uint16;
        case Value::Kind::U32:  return &ffi_type_uint32;
        case Value::Kind::U64:  return &ffi_type_uint64;
        case Value::Kind::F16:  return &ffi_type_uint16;
        case Value::Kind::F32:  return &ffi_type_float;
        case Value::Kind::F64:  return &ffi_type_double;
        case Value::Kind::Bool: return &ffi_type_uint8;
        case Value::Kind::C8:   return &ffi_type_uint8;
        case Value::Kind::Addr: return &ffi_type_pointer;
    }
    return &ffi_type_pointer;
}

// The libffi type for a Fern type. A struct type is built here and owned by the structs list.
static ffi_type* type_for(Interpreter& vm, TypeSymbol* type, std::vector<std::unique_ptr<StructType>>& structs)
{
    if (!is_memory_value(type)) return scalar_type(vm.type_kind(type));

    auto structType = std::make_unique<StructType>();
    for (auto* field : type->as<NamedTypeSymbol>()->fields)
    {
        if (has_modifier(field->modifiers, Modifier::Static)) continue;
        structType->elements.push_back(type_for(vm, field->type, structs));
    }
    structType->elements.push_back(nullptr);
    structType->type.type = FFI_TYPE_STRUCT;
    structType->type.elements = structType->elements.data();

    ffi_type* result = &structType->type;
    structs.push_back(std::move(structType));
    return result;
}

ExternCalls::Binding& ExternCalls::binding_for(MethodSymbol* method)
{
    auto it = bindings.find(method);
    if (it != bindings.end()) return *it->second;

    auto binding = std::make_unique<Binding>();
    std::string name(method->extern_name());
    std::string libraryName(method->extern_library());
    void* library = nullptr;
    if (!libraryName.empty())
    {
        library = load_library(libraryName);
        if (!library)
            throw VmError{std::format("extern '{}' needs library '{}', which could not be loaded", method->name, libraryName)};
    }
    binding->function = find_symbol(library, name);
    if (!binding->function)
    {
        std::string where = libraryName.empty() ? "the running process" : std::format("library '{}'", libraryName);
        throw VmError{std::format("extern '{}' names C function '{}', which is not in {}", method->name, name, where)};
    }

    for (auto* param : method->parameters)
    {
        binding->argTypes.push_back(type_for(vm, param->type, binding->structs));
        binding->argIsStruct.push_back(is_memory_value(param->type));
    }

    ffi_type* returnType = &ffi_type_void;
    if (TypeSymbol* type = method->get_return_type())
    {
        returnType = type_for(vm, type, binding->structs);
        binding->hasReturn = true;
        binding->returnsStruct = is_memory_value(type);
        binding->returnKind = vm.type_kind(type);
    }

    unsigned argCount = static_cast<unsigned>(binding->argTypes.size());
    if (ffi_prep_cif(&binding->cif, FFI_DEFAULT_ABI, argCount, returnType, binding->argTypes.data()) != FFI_OK)
        throw VmError{std::format("extern '{}' has a signature libffi cannot call", method->name)};

    Binding& result = *binding;
    bindings.emplace(method, std::move(binding));
    return result;
}

#pragma region Call

Value ExternCalls::call(MethodSymbol* method, const std::vector<Value>& args, bool hasResultDest, uint64_t resultDest)
{
    Binding& binding = binding_for(method);

    // Scalars are copied into one word each. Host memory is outside the VM's tables so the VM writes it unchecked.
    std::vector<uint64_t> slots(args.size(), 0);
    std::vector<void*> values(args.size(), nullptr);
    for (size_t i = 0; i < args.size(); i++)
    {
        if (binding.argIsStruct[i])
        {
            values[i] = memory.host_ptr(args[i].as_addr(), binding.argTypes[i]->size);
        }
        else
        {
            vm.store_scalar(to_addr(&slots[i]), args[i]);
            values[i] = &slots[i];
        }
    }

    // libffi widens a small integer return to a whole ffi_arg, so the buffer is at least one of those
    size_t returnSize = std::max<size_t>(binding.cif.rtype->size, sizeof(ffi_arg));
    std::vector<uint64_t> result((returnSize + 7) / 8, 0);

    ffi_call(&binding.cif, FFI_FN(binding.function), result.data(), values.data());

    if (!binding.hasReturn) return Value{};
    if (binding.returnsStruct)
    {
        if (hasResultDest) memory.write_bytes(resultDest, result.data(), binding.cif.rtype->size);
        return Value{};
    }
    // Reading the low bytes of the widened word is the narrow value on a little endian host
    return vm.load_scalar(to_addr(result.data()), binding.returnKind);
}

#else

struct ExternCalls::Binding
{
};

ExternCalls::Binding& ExternCalls::binding_for(MethodSymbol* method)
{
    throw VmError{std::format("this build has no libffi, so extern '{}' cannot be called", method->name)};
}

Value ExternCalls::call(MethodSymbol* method, const std::vector<Value>&, bool, uint64_t)
{
    binding_for(method);
    return Value{};
}

#endif

void ExternCalls::bind(MethodSymbol* method)
{
    binding_for(method);
}

ExternCalls::ExternCalls(Interpreter& vm, VmMemory& memory)
    : vm(vm)
    , memory(memory)
{
}

ExternCalls::~ExternCalls() = default;

}
