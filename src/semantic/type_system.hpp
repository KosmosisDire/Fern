#pragma once

#include <memory>
#include <variant>
#include <vector>
#include <string>
#include <unordered_map>
#include "type.hpp"
#include "symbol.hpp"

namespace Fern
{
    class TypeSystem {
    private:
        // Canonicalization - ensure pointer equality for type equality
        std::vector<TypePtr> all_types;
        
        // Quick lookup for primitives
        std::unordered_map<LiteralKind, TypePtr> primitives;
        
        // Quick lookup for named types
        std::unordered_map<TypeSymbol*, TypePtr> named_types;
        
        // For type inference
        uint32_t next_unresolved_id = 0;
        
        // Helper to find or create a type
        template<typename T>
        TypePtr find_or_create(const T& type_kind);
        
        // Type comparison helpers
        bool compare_types(const PrimitiveType& a, const PrimitiveType& b) const;
        bool compare_types(const PointerType& a, const PointerType& b) const;
        bool compare_types(const ArrayType& a, const ArrayType& b) const;
        bool compare_types(const NamedType& a, const NamedType& b) const;
        bool compare_types(const UnresolvedType& a, const UnresolvedType& b) const;
        bool compare_types(const MetaType& a, const MetaType& b) const;

        template<typename T, typename U>
        bool compare_types(const T&, const U&) const { return false; }
        
    public:
        TypeSystem();
        
        void init_primitives();
        void init_string_type(TypeSymbol* string_symbol);

        // Built-in type queries
        TypePtr get_string_type() const;
        bool is_string_type(TypePtr type) const;

        // Type creation methods
        TypePtr get_void();
        TypePtr get_ptr();
        TypePtr get_bool();
        TypePtr get_i8();
        TypePtr get_u8();
        TypePtr get_i16();
        TypePtr get_u16();
        TypePtr get_i32();
        TypePtr get_u32();
        TypePtr get_i64();
        TypePtr get_u64();
        TypePtr get_f16();
        TypePtr get_f32();
        TypePtr get_f64();
        TypePtr get_null();
        
        TypePtr get_primitive(const std::string& name);
        TypePtr get_pointer(TypePtr pointee);
        TypePtr get_array(TypePtr element, int32_t size = -1);
        TypePtr get_function(TypePtr return_type, std::vector<TypePtr> params);
        TypePtr get_named(TypeSymbol* symbol);
        TypePtr get_unresolved();
        TypePtr get_type_type(TypePtr inner);
        
        // Type checking
        bool are_equal(TypePtr a, TypePtr b) const;
        bool is_assignable(TypePtr from, TypePtr to) const;
    };
    
} // namespace Fern
