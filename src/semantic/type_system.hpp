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

        // Quick lookup for pointer types (keyed by pointee)
        std::unordered_map<TypePtr, TypePtr> pointer_types;

        // Quick lookup for array types (keyed by {element, size})
        struct ArrayKey {
            TypePtr element;
            int32_t size;
            bool operator==(const ArrayKey& other) const {
                return element == other.element && size == other.size;
            }
        };
        struct ArrayKeyHash {
            size_t operator()(const ArrayKey& k) const {
                return std::hash<void*>()(k.element.get()) ^ (std::hash<int32_t>()(k.size) << 1);
            }
        };
        std::unordered_map<ArrayKey, TypePtr, ArrayKeyHash> array_types;

        // Quick lookup for meta types (keyed by inner)
        std::unordered_map<TypePtr, TypePtr> meta_types;

        // For type inference
        uint32_t next_unresolved_id = 0;
        
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
