#include "type_system.hpp"

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

void TypeSystem::init_primitives() {
    auto add_primitive = [this](PrimitiveKind kind) {
        auto type = std::make_shared<Type>();
        type->kind = PrimitiveType{kind};
        all_types.push_back(type);
        primitives[kind] = type;
        return type;
    };
    
    add_primitive(PrimitiveKind::Void);
    add_primitive(PrimitiveKind::Bool);
    add_primitive(PrimitiveKind::Char);
    add_primitive(PrimitiveKind::String);
    add_primitive(PrimitiveKind::I8);
    add_primitive(PrimitiveKind::I16);
    add_primitive(PrimitiveKind::I32);
    add_primitive(PrimitiveKind::I64);
    add_primitive(PrimitiveKind::U8);
    add_primitive(PrimitiveKind::U16);
    add_primitive(PrimitiveKind::U32);
    add_primitive(PrimitiveKind::U64);
    add_primitive(PrimitiveKind::F32);
    add_primitive(PrimitiveKind::F64);
}

TypePtr TypeSystem::get_void() { 
    return primitives[PrimitiveKind::Void]; 
}

TypePtr TypeSystem::get_bool() { 
    return primitives[PrimitiveKind::Bool]; 
}

TypePtr TypeSystem::get_i32() { 
    return primitives[PrimitiveKind::I32]; 
}

TypePtr TypeSystem::get_i64() { 
    return primitives[PrimitiveKind::I64]; 
}

TypePtr TypeSystem::get_f32() { 
    return primitives[PrimitiveKind::F32]; 
}

TypePtr TypeSystem::get_f64() { 
    return primitives[PrimitiveKind::F64]; 
}

TypePtr TypeSystem::get_primitive(const std::string& name) {
    static std::unordered_map<std::string, PrimitiveKind> name_map = {
        {"void", PrimitiveKind::Void},
        {"bool", PrimitiveKind::Bool},
        {"char", PrimitiveKind::Char},
        {"i8", PrimitiveKind::I8},
        {"i16", PrimitiveKind::I16},
        {"i32", PrimitiveKind::I32},
        {"i64", PrimitiveKind::I64},
        {"u8", PrimitiveKind::U8},
        {"u16", PrimitiveKind::U16},
        {"u32", PrimitiveKind::U32},
        {"u64", PrimitiveKind::U64},
        {"f32", PrimitiveKind::F32},
        {"f64", PrimitiveKind::F64},
        {"string", PrimitiveKind::String}
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

TypePtr TypeSystem::get_generic(TypeSymbol* generic, std::vector<TypePtr> args) {
    return find_or_create(GenericType{generic, std::move(args)});
}

TypePtr TypeSystem::get_type_parameter(const std::string& name, uint32_t index) {
    return find_or_create(TypeParameter{name, index});
}

TypePtr TypeSystem::get_unresolved() {
    return find_or_create(UnresolvedType{next_unresolved_id++});
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
