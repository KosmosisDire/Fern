#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <memory>
#include <unordered_map>

namespace Fern {
    // Forward declarations from semantic system
    struct Type;
    struct TypeSymbol;
    class TypeSystem;
    using TypePtr = std::shared_ptr<Type>;
}

namespace Fern::FLIR
{

// Forward declarations
struct IRType;
struct IRStruct;

using IRTypePtr = IRType*;

#pragma region IR Type Kinds

enum class IRTypeKind
{
    Void,
    Bool,
    Int,
    Float,
    Pointer,
    Array,
    Struct
};

#pragma region IR Type

struct IRType
{
    IRTypeKind kind;

    // For Int: bit width and signedness
    uint8_t bit_width = 0;
    bool is_signed = true;

    // For Pointer: pointee type
    IRTypePtr pointee = nullptr;

    // For Array: element type and size (-1 = dynamic)
    IRTypePtr element = nullptr;
    int32_t array_size = -1;

    // For Struct: reference to struct definition
    IRStruct* struct_def = nullptr;

    // Helpers
    bool is_void() const { return kind == IRTypeKind::Void; }
    bool is_bool() const { return kind == IRTypeKind::Bool; }
    bool is_int() const { return kind == IRTypeKind::Int; }
    bool is_float() const { return kind == IRTypeKind::Float; }
    bool is_pointer() const { return kind == IRTypeKind::Pointer; }
    bool is_array() const { return kind == IRTypeKind::Array; }
    bool is_struct() const { return kind == IRTypeKind::Struct; }

    std::string get_name() const;
    size_t get_size() const;
    size_t get_alignment() const;
};

#pragma region IR Struct

struct IRStructField
{
    std::string name;
    IRTypePtr type = nullptr;
    size_t offset = 0;
};

struct IRStruct
{
    std::string name;
    std::vector<IRStructField> fields;
    size_t size = 0;
    size_t alignment = 1;

    // Link back to semantic symbol (for lookups)
    TypeSymbol* symbol = nullptr;
};

#pragma region IR Type System

class IRTypeSystem
{
public:
    IRTypeSystem() { init_primitives(); }

    // Convert from semantic type to IR type (this is where lowering happens)
    IRTypePtr convert(TypePtr semantic_type);

    // Primitive types
    IRTypePtr get_void() { return &void_type; }
    IRTypePtr get_bool() { return &bool_type; }
    IRTypePtr get_i8() { return &i8_type; }
    IRTypePtr get_i16() { return &i16_type; }
    IRTypePtr get_i32() { return &i32_type; }
    IRTypePtr get_i64() { return &i64_type; }
    IRTypePtr get_u8() { return &u8_type; }
    IRTypePtr get_u16() { return &u16_type; }
    IRTypePtr get_u32() { return &u32_type; }
    IRTypePtr get_u64() { return &u64_type; }
    IRTypePtr get_f16() { return &f16_type; }
    IRTypePtr get_f32() { return &f32_type; }
    IRTypePtr get_f64() { return &f64_type; }

    // Compound types
    IRTypePtr get_pointer(IRTypePtr pointee);
    IRTypePtr get_array(IRTypePtr element, int32_t size);
    IRTypePtr get_struct(TypeSymbol* symbol);

    // Define a struct (called during lowering setup)
    IRStruct* define_struct(TypeSymbol* symbol);

    // Lookup struct by symbol
    IRStruct* find_struct(TypeSymbol* symbol);

    // Get all defined structs (for codegen)
    const std::vector<std::unique_ptr<IRStruct>>& get_all_structs() const { return struct_defs; }

private:
    void init_primitives();

    // Primitive type storage (statically allocated)
    IRType void_type;
    IRType bool_type;
    IRType i8_type, i16_type, i32_type, i64_type;
    IRType u8_type, u16_type, u32_type, u64_type;
    IRType f16_type, f32_type, f64_type;

    // Compound type storage (dynamically allocated)
    std::vector<std::unique_ptr<IRType>> pointer_types;
    std::vector<std::unique_ptr<IRType>> array_types;
    std::vector<std::unique_ptr<IRStruct>> struct_defs;

    // Caches for deduplication
    std::unordered_map<IRTypePtr, IRTypePtr> pointer_cache;
    std::unordered_map<TypeSymbol*, IRStruct*> struct_cache;
    std::unordered_map<TypeSymbol*, IRTypePtr> struct_type_cache;
};

} // namespace Fern::FLIR