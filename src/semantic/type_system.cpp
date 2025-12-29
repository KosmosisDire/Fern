#include "type_system.hpp"
#include <iostream>
namespace Fern
{

TypeSystem::TypeSystem() {
    // Pre-create primitive types
    init_primitives();
}

template<typename T>
TypePtr TypeSystem::find_or_create(const T& type_kind) {
    // Linear search for now (could optimize with better hashing)
    for (const auto& type : all_types) {
        if (type->is<T>() && compare_types(type->as<T>(), type_kind)) {
            return type;
        }
    }
    
    // Create new type
    auto new_type = std::make_shared<Type>();
    new_type->kind = type_kind;
    all_types.push_back(new_type);
    return new_type;
}

bool TypeSystem::compare_types(const PrimitiveType& a, const PrimitiveType& b) const {
    return a.kind == b.kind;
}

bool TypeSystem::compare_types(const PointerType& a, const PointerType& b) const {
    return a.pointee == b.pointee;
}

bool TypeSystem::compare_types(const ArrayType& a, const ArrayType& b) const {
    return a.element == b.element && a.size == b.size;
}

bool TypeSystem::compare_types(const NamedType& a, const NamedType& b) const {
    return a.symbol == b.symbol;
}

bool TypeSystem::compare_types(const UnresolvedType& a, const UnresolvedType& b) const {
    return a.id == b.id;
}

bool TypeSystem::compare_types(const MetaType& a, const MetaType& b) const {
    return a.inner == b.inner;
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
    return find_or_create(PointerType{pointee});
}

TypePtr TypeSystem::get_array(TypePtr element, int32_t size) {
    return find_or_create(ArrayType{element, size});
}

TypePtr TypeSystem::get_function(TypePtr return_type, std::vector<TypePtr> params) {
    return find_or_create(FunctionType{return_type, std::move(params)});
}

TypePtr TypeSystem::get_named(TypeSymbol* symbol) {
    auto it = named_types.find(symbol);
    if (it != named_types.end()) {
        return it->second;
    }

    auto type = find_or_create(NamedType{symbol});
    named_types[symbol] = type;
    return type;
}

TypePtr TypeSystem::get_unresolved() {
    return find_or_create(UnresolvedType{next_unresolved_id++});
}

TypePtr TypeSystem::get_type_type(TypePtr inner) {
    return find_or_create(MetaType{inner});
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
