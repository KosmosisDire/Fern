#include "type_system.hpp"
#include <iostream>
namespace Fern
{

TypeSystem::TypeSystem() {
    init_primitives();
}

void TypeSystem::init_primitives() {
    auto add_primitive = [this](LiteralKind kind) {
        auto type = std::make_shared<Type>();
        type->kind = PrimitiveType{kind};
        all_types.push_back(type);
        primitives[kind] = type;
        return type;
    };

    add_primitive(LiteralKind::Void);
    add_primitive(LiteralKind::Bool);
    add_primitive(LiteralKind::Char);
    add_primitive(LiteralKind::I8);
    add_primitive(LiteralKind::U8);
    add_primitive(LiteralKind::I16);
    add_primitive(LiteralKind::U16);
    add_primitive(LiteralKind::I32);
    add_primitive(LiteralKind::U32);
    add_primitive(LiteralKind::I64);
    add_primitive(LiteralKind::U64);
    add_primitive(LiteralKind::F16);
    add_primitive(LiteralKind::F32);
    add_primitive(LiteralKind::F64);
    add_primitive(LiteralKind::Null);

    // add string as an empty named type
    // String type will be initialized later with its symbol
    auto new_type = std::make_shared<Type>();
    new_type->kind = NamedType{nullptr};
    all_types.push_back(new_type);
    primitives[LiteralKind::String] = new_type;
}

void TypeSystem::init_string_type(TypeSymbol* string_symbol)
{
    primitives[LiteralKind::String] = get_named(string_symbol);
}

TypePtr TypeSystem::get_string_type() const
{
    auto it = primitives.find(LiteralKind::String);
    return it != primitives.end() ? it->second : nullptr;
}

bool TypeSystem::is_string_type(TypePtr type) const
{
    if (!type) return false;
    auto string_type = get_string_type();
    return string_type && type == string_type;
}

TypePtr TypeSystem::get_void() { 
    return primitives[LiteralKind::Void]; 
}

TypePtr TypeSystem::get_bool() {
    return primitives[LiteralKind::Bool];
}

TypePtr TypeSystem::get_i8() {
    return primitives[LiteralKind::I8];
}

TypePtr TypeSystem::get_u8() {
    return primitives[LiteralKind::U8];
}

TypePtr TypeSystem::get_i16() {
    return primitives[LiteralKind::I16];
}

TypePtr TypeSystem::get_u16() {
    return primitives[LiteralKind::U16];
}

TypePtr TypeSystem::get_i32() {
    return primitives[LiteralKind::I32];
}

TypePtr TypeSystem::get_u32() {
    return primitives[LiteralKind::U32];
}

TypePtr TypeSystem::get_i64() {
    return primitives[LiteralKind::I64];
}

TypePtr TypeSystem::get_u64() {
    return primitives[LiteralKind::U64];
}

TypePtr TypeSystem::get_f16() {
    return primitives[LiteralKind::F16];
}

TypePtr TypeSystem::get_f32() {
    return primitives[LiteralKind::F32];
}

TypePtr TypeSystem::get_f64() {
    return primitives[LiteralKind::F64];
}

TypePtr TypeSystem::get_null() {
    return primitives[LiteralKind::Null];
}

TypePtr TypeSystem::get_primitive(const std::string& name) {

    // alias ptr as void*
    if (name == "ptr")
    {
        return get_pointer(get_void());
    }

    static std::unordered_map<std::string, LiteralKind> name_map = {
        {"void", LiteralKind::Void},
        {"bool", LiteralKind::Bool},
        {"char", LiteralKind::Char},
        {"i8", LiteralKind::I8},
        {"u8", LiteralKind::U8},
        {"i16", LiteralKind::I16},
        {"u16", LiteralKind::U16},
        {"i32", LiteralKind::I32},
        {"int", LiteralKind::I32},
        {"u32", LiteralKind::U32},
        {"i64", LiteralKind::I64},
        {"u64", LiteralKind::U64},
        {"f16", LiteralKind::F16},
        {"f32", LiteralKind::F32},
        {"float", LiteralKind::F32},
        {"f64", LiteralKind::F64},
        {"double", LiteralKind::F64},
        {"string", LiteralKind::String}
    };

    auto it = name_map.find(name);
    if (it != name_map.end()) {
        return primitives[it->second];
    }
    return nullptr;
}

TypePtr TypeSystem::get_pointer(TypePtr pointee) {
    auto it = pointer_types.find(pointee);
    if (it != pointer_types.end()) {
        return it->second;
    }

    auto new_type = std::make_shared<Type>();
    new_type->kind = PointerType{pointee};
    all_types.push_back(new_type);
    pointer_types[pointee] = new_type;
    return new_type;
}

TypePtr TypeSystem::get_array(TypePtr element, int32_t size) {
    ArrayKey key{element, size};
    auto it = array_types.find(key);
    if (it != array_types.end()) {
        return it->second;
    }

    auto new_type = std::make_shared<Type>();
    new_type->kind = ArrayType{element, size};
    all_types.push_back(new_type);
    array_types[key] = new_type;
    return new_type;
}

TypePtr TypeSystem::get_function(TypePtr return_type, std::vector<TypePtr> params) {
    // Function types are less common, use linear search
    for (const auto& type : all_types) {
        if (auto* func = type->as<FunctionType>()) {
            if (func->returnType == return_type && func->paramTypes == params) {
                return type;
            }
        }
    }

    auto new_type = std::make_shared<Type>();
    new_type->kind = FunctionType{return_type, std::move(params)};
    all_types.push_back(new_type);
    return new_type;
}

TypePtr TypeSystem::get_named(TypeSymbol* symbol) {
    auto it = named_types.find(symbol);
    if (it != named_types.end()) {
        return it->second;
    }

    auto new_type = std::make_shared<Type>();
    new_type->kind = NamedType{symbol};
    all_types.push_back(new_type);
    named_types[symbol] = new_type;
    return new_type;
}

TypePtr TypeSystem::get_unresolved() {
    auto new_type = std::make_shared<Type>();
    new_type->kind = UnresolvedType{next_unresolved_id++};
    all_types.push_back(new_type);
    return new_type;
}

TypePtr TypeSystem::get_type_type(TypePtr inner) {
    auto it = meta_types.find(inner);
    if (it != meta_types.end()) {
        return it->second;
    }

    auto new_type = std::make_shared<Type>();
    new_type->kind = MetaType{inner};
    all_types.push_back(new_type);
    meta_types[inner] = new_type;
    return new_type;
}

bool TypeSystem::are_equal(TypePtr a, TypePtr b) const {
    // With canonicalization, pointer equality is sufficient
    return a == b;
}

bool TypeSystem::is_assignable(TypePtr from, TypePtr to) const {
    if (are_equal(from, to)) return true;
    
    // Add conversion rules here
    // For now, just check for exact match
    return false;
}

} // namespace Fern
