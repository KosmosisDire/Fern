#include "ir_type.hpp"
#include "semantic/type.hpp"
#include "semantic/symbol.hpp"
#include "semantic/type_system.hpp"

namespace Fern::FLIR
{

#pragma region IRType Implementation

std::string IRType::get_name() const
{
    switch (kind)
    {
    case IRTypeKind::Void:
        return "void";
    case IRTypeKind::Bool:
        return "bool";
    case IRTypeKind::Int:
        if (is_signed) {
            return "i" + std::to_string(bit_width);
        } else {
            return "u" + std::to_string(bit_width);
        }
    case IRTypeKind::Float:
        return "f" + std::to_string(bit_width);
    case IRTypeKind::Pointer:
        return pointee ? pointee->get_name() + "*" : "void*";
    case IRTypeKind::Array:
        if (array_size >= 0) {
            return element->get_name() + "[" + std::to_string(array_size) + "]";
        }
        return element->get_name() + "[]";
    case IRTypeKind::Struct:
        return struct_def ? struct_def->name : "?struct";
    }
    return "?";
}

size_t IRType::get_size() const
{
    switch (kind)
    {
    case IRTypeKind::Void:
        return 0;
    case IRTypeKind::Bool:
        return 1;
    case IRTypeKind::Int:
    case IRTypeKind::Float:
        return bit_width / 8;
    case IRTypeKind::Pointer:
        return 8; // 64-bit pointers
    case IRTypeKind::Array:
        if (array_size >= 0) {
            return element->get_size() * array_size;
        }
        return 16; // Dynamic array: ptr + length + padding
    case IRTypeKind::Struct:
        return struct_def ? struct_def->size : 0;
    }
    return 0;
}

size_t IRType::get_alignment() const
{
    switch (kind)
    {
    case IRTypeKind::Void:
        return 1;
    case IRTypeKind::Bool:
        return 1;
    case IRTypeKind::Int:
    case IRTypeKind::Float:
        return bit_width / 8;
    case IRTypeKind::Pointer:
        return 8;
    case IRTypeKind::Array:
        if (array_size >= 0) {
            return element->get_alignment();
        }
        return 8; // Dynamic array aligned to pointer
    case IRTypeKind::Struct:
        return struct_def ? struct_def->alignment : 1;
    }
    return 1;
}

#pragma region IRTypeSystem Implementation

void IRTypeSystem::init_primitives()
{
    void_type.kind = IRTypeKind::Void;

    bool_type.kind = IRTypeKind::Bool;

    i8_type.kind = IRTypeKind::Int;
    i8_type.bit_width = 8;
    i8_type.is_signed = true;

    i16_type.kind = IRTypeKind::Int;
    i16_type.bit_width = 16;
    i16_type.is_signed = true;

    i32_type.kind = IRTypeKind::Int;
    i32_type.bit_width = 32;
    i32_type.is_signed = true;

    i64_type.kind = IRTypeKind::Int;
    i64_type.bit_width = 64;
    i64_type.is_signed = true;

    u8_type.kind = IRTypeKind::Int;
    u8_type.bit_width = 8;
    u8_type.is_signed = false;

    u16_type.kind = IRTypeKind::Int;
    u16_type.bit_width = 16;
    u16_type.is_signed = false;

    u32_type.kind = IRTypeKind::Int;
    u32_type.bit_width = 32;
    u32_type.is_signed = false;

    u64_type.kind = IRTypeKind::Int;
    u64_type.bit_width = 64;
    u64_type.is_signed = false;

    f16_type.kind = IRTypeKind::Float;
    f16_type.bit_width = 16;

    f32_type.kind = IRTypeKind::Float;
    f32_type.bit_width = 32;

    f64_type.kind = IRTypeKind::Float;
    f64_type.bit_width = 64;
}

IRTypePtr IRTypeSystem::get_pointer(IRTypePtr pointee)
{
    // Check cache
    auto it = pointer_cache.find(pointee);
    if (it != pointer_cache.end()) {
        return it->second;
    }

    // Create new pointer type
    auto ptr_type = std::make_unique<IRType>();
    ptr_type->kind = IRTypeKind::Pointer;
    ptr_type->pointee = pointee;

    IRTypePtr result = ptr_type.get();
    pointer_types.push_back(std::move(ptr_type));
    pointer_cache[pointee] = result;
    return result;
}

IRTypePtr IRTypeSystem::get_array(IRTypePtr element, int32_t size)
{
    // Arrays aren't cached since they're less common
    auto arr_type = std::make_unique<IRType>();
    arr_type->kind = IRTypeKind::Array;
    arr_type->element = element;
    arr_type->array_size = size;

    IRTypePtr result = arr_type.get();
    array_types.push_back(std::move(arr_type));
    return result;
}

IRTypePtr IRTypeSystem::get_struct(TypeSymbol* symbol)
{
    // Check cache
    auto it = struct_type_cache.find(symbol);
    if (it != struct_type_cache.end()) {
        return it->second;
    }

    // Find or create struct definition
    IRStruct* def = find_struct(symbol);
    if (!def) {
        def = define_struct(symbol);
    }

    // Create struct type
    auto struct_type = std::make_unique<IRType>();
    struct_type->kind = IRTypeKind::Struct;
    struct_type->struct_def = def;

    IRTypePtr result = struct_type.get();
    // Store in array_types (reusing for all compound types)
    array_types.push_back(std::move(struct_type));
    struct_type_cache[symbol] = result;
    return result;
}

IRStruct* IRTypeSystem::define_struct(TypeSymbol* symbol)
{
    // Check if already defined
    auto it = struct_cache.find(symbol);
    if (it != struct_cache.end()) {
        return it->second;
    }

    auto def = std::make_unique<IRStruct>();
    def->name = symbol->get_qualified_name();
    def->symbol = symbol;

    // Calculate layout - fields will be added later during conversion
    // since we might not have converted all field types yet

    IRStruct* result = def.get();
    struct_defs.push_back(std::move(def));
    struct_cache[symbol] = result;
    return result;
}

IRStruct* IRTypeSystem::find_struct(TypeSymbol* symbol)
{
    auto it = struct_cache.find(symbol);
    if (it != struct_cache.end()) {
        return it->second;
    }
    return nullptr;
}

IRTypePtr IRTypeSystem::convert(TypePtr semantic_type)
{
    if (!semantic_type) {
        return get_void();
    }

    // Handle primitives
    if (auto prim = semantic_type->as<PrimitiveType>()) {
        switch (prim->kind) {
        case LiteralKind::Void:
            return get_void();
        case LiteralKind::Bool:
            return get_bool();
        case LiteralKind::Char:
            return get_i8();
        case LiteralKind::I8:
            return get_i8();
        case LiteralKind::U8:
            return get_u8();
        case LiteralKind::I16:
            return get_i16();
        case LiteralKind::U16:
            return get_u16();
        case LiteralKind::I32:
            return get_i32();
        case LiteralKind::U32:
            return get_u32();
        case LiteralKind::I64:
            return get_i64();
        case LiteralKind::U64:
            return get_u64();
        case LiteralKind::F16:
            return get_f16();
        case LiteralKind::F32:
            return get_f32();
        case LiteralKind::F64:
            return get_f64();
        case LiteralKind::String:
            return get_pointer(get_i8());
        case LiteralKind::Null:
            return get_pointer(get_void());
        default:
            return get_void();
        }
    }

    // Handle pointers - recursively convert pointee
    if (auto ptr = semantic_type->as<PointerType>()) {
        IRTypePtr pointee = convert(ptr->pointee);
        return get_pointer(pointee);
    }

    // Handle arrays - recursively convert element
    if (auto arr = semantic_type->as<ArrayType>()) {
        IRTypePtr element = convert(arr->element);
        return get_array(element, arr->size);
    }

    // Handle named types (structs, ref types)
    if (auto named = semantic_type->as<NamedType>()) {
        TypeSymbol* sym = named->symbol;
        if (!sym) {
            return get_void();
        }

        // Get the struct type
        IRTypePtr struct_type = get_struct(sym);

        // If it's a reference type, wrap in pointer
        if (sym->is_ref()) {
            return get_pointer(struct_type);
        }

        return struct_type;
    }

    // Handle function types - for now just return void pointer
    if (semantic_type->is<FunctionType>()) {
        return get_pointer(get_void());
    }

    // Fallback
    return get_void();
}

} // namespace Fern::FLIR
