#pragma once

#include <string>
#include <vector>
#include <memory>
#include <variant>
#include "common/token_kind.hpp"


namespace Fern
{
    struct Type;
    struct TypeSymbol;
    using TypePtr = std::shared_ptr<Type>;

    #pragma region Type Variants

    struct PrimitiveType {
        LiteralKind kind;
    };
    
    struct PointerType {
        TypePtr pointee;
    };
    
    struct ArrayType {
        TypePtr element;
        int32_t size = -1;  // -1 = dynamic array
    };
    
    struct FunctionType {
        TypePtr returnType;
        std::vector<TypePtr> paramTypes;
    };
    
    struct NamedType {
        TypeSymbol* symbol;  // Points to the type's symbol
    };

    struct UnresolvedType {
        uint32_t id;  // For type resolution
    };

    struct MetaType {
        TypePtr inner;  // The type this meta-type represents
    };

    #pragma region Type

    struct Type {
        std::variant<
            PrimitiveType,
            PointerType,
            ArrayType,
            FunctionType,
            NamedType,
            UnresolvedType,
            MetaType
        > kind;

        // Helper methods
        template<typename T>
        bool is() const { return std::holds_alternative<T>(kind); }

        template<typename T>
        const T* as() const {
            if (auto ptr = std::get_if<T>(&kind)) {
                return ptr;
            }
            return nullptr;
        }

        bool is_void() const;
        bool is_value_type() const;
        bool is_reference_type() const;
        std::string get_name() const;

        // Structural equality (works across different TypeSystem instances)
        static bool equals(TypePtr a, TypePtr b);
    };

} // namespace Fern
