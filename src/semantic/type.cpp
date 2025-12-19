#include "type.hpp"
#include "symbol.hpp"
#include "type_system.hpp"

namespace Fern
{
    bool Type::is_void() const {
        return is<PrimitiveType>() && as<PrimitiveType>()->kind == LiteralKind::Void;
    }
    
    bool Type::is_value_type() const {
        if (is<PrimitiveType>()) return true;
        if (is<PointerType>()) return true;  // Pointers themselves are values
        if (is<ArrayType>()) return true;  // Arrays are value types (copy semantics)
        if (is<NamedType>()) {
            // Check if the type symbol is a value type (struct)
            return as<NamedType>()->symbol && !as<NamedType>()->symbol->is_ref();
        }
        return false;
    }
    
    bool Type::is_reference_type() const {
        return !is_value_type() && !is_void();
    }
    
    std::string Type::get_name() const {
        return std::visit([](const auto& t) -> std::string {
            using T = std::decay_t<decltype(t)>;
            
            if constexpr (std::is_same_v<T, PrimitiveType>) {
                switch (t.kind) {
                    case LiteralKind::Void: return "void";
                    case LiteralKind::Bool: return "bool";
                    case LiteralKind::Char: return "char";
                    case LiteralKind::I32: return "i32";
                    case LiteralKind::F32: return "f32";
                    case LiteralKind::String: return "string";
                    case LiteralKind::Null: return "null";
                    default: break;
                }
                return "primitive?";
            }
            else if constexpr (std::is_same_v<T, PointerType>) {
                return t.pointee->get_name() + "*";
            }
            else if constexpr (std::is_same_v<T, ArrayType>) {
                if (t.size >= 0) {
                    return t.element->get_name() + "[" + std::to_string(t.size) + "]";
                }
                return t.element->get_name() + "[]";
            }
            else if constexpr (std::is_same_v<T, FunctionType>) {
                std::string result = "fn(";
                for (size_t i = 0; i < t.paramTypes.size(); i++) {
                    if (i > 0) result += ", ";
                    result += t.paramTypes[i]->get_name();
                }
                result += ") -> ";
                result += t.returnType->get_name();
                return result;
            }
            else if constexpr (std::is_same_v<T, NamedType>) {
                return t.symbol ? t.symbol->name : "?type";
            }
            else if constexpr (std::is_same_v<T, GenericType>) {
                std::string result = t.genericSymbol ? t.genericSymbol->name : "?generic";
                result += "<";
                for (size_t i = 0; i < t.typeArgs.size(); i++) {
                    if (i > 0) result += ", ";
                    result += t.typeArgs[i]->get_name();
                }
                result += ">";
                return result;
            }
            else if constexpr (std::is_same_v<T, TypeParameter>) {
                return t.name;
            }
            else if constexpr (std::is_same_v<T, UnresolvedType>) {
                return "?" + std::to_string(t.id);
            }
            else if constexpr (std::is_same_v<T, MetaType>) {
                return "Type<" + (t.inner ? t.inner->get_name() : "?") + ">";
            }
            else {
                return "?unknown";
            }
        }, kind);
    }

    int Type::get_size() const
    {
        return std::visit([](const auto& t) -> int {
            using T = std::decay_t<decltype(t)>;

            if constexpr (std::is_same_v<T, PrimitiveType>) {
                switch (t.kind) {
                    case LiteralKind::Void: return 0;
                    case LiteralKind::Bool: return 1;
                    case LiteralKind::Char: return 1;
                    case LiteralKind::I32: return 4;
                    case LiteralKind::F32: return 4;
                    case LiteralKind::String: return 16; // ptr (8) + length (4) + padding (4)
                    case LiteralKind::Null: return 8;    // null is a pointer
                    default: break;
                }
                return 4;
            }
            else if constexpr (std::is_same_v<T, PointerType>) {
                return 8; // 64-bit pointers
            }
            else if constexpr (std::is_same_v<T, ArrayType>) {
                if (t.size >= 0) {
                    // Fixed-size array: element_size * count
                    return t.element->get_size() * t.size;
                }
                // Dynamic array: pointer (8) + length (4) + padding (4) = 16
                return 16;
            }
            else if constexpr (std::is_same_v<T, FunctionType>) {
                return 8; // Function pointer
            }
            else if constexpr (std::is_same_v<T, NamedType>) {
                if (!t.symbol) return 0;

                // Calculate struct size with proper alignment
                int size = 0;
                int max_align = 1;

                for (auto* member : t.symbol->member_order) {
                    if (auto* var = member->as<VariableSymbol>())
                    {
                        int field_size = var->type ? var->type->get_size() : 4;
                        int field_align = var->type ? var->type->get_alignment() : 4;

                        int padding = (field_align - (size % field_align)) % field_align;
                        size += padding + field_size;

                        if (field_align > max_align) max_align = field_align;
                    }
                }

                // Final padding for struct alignment
                int final_padding = (max_align - (size % max_align)) % max_align;
                return size + final_padding;
            }
            else if constexpr (std::is_same_v<T, GenericType>) {
                return 0; // Generic types have no concrete size
            }
            else if constexpr (std::is_same_v<T, TypeParameter>) {
                return 0; // Type parameters have no concrete size
            }
            else if constexpr (std::is_same_v<T, UnresolvedType>) {
                return 0; // Unresolved types have no size
            }
            else if constexpr (std::is_same_v<T, MetaType>) {
                return 0; // Meta types have no runtime size
            }
            else {
                return 4;
            }
        }, kind);
    }

    int Type::get_alignment() const {
        return std::visit([](const auto& t) -> int {
            using T = std::decay_t<decltype(t)>;

            if constexpr (std::is_same_v<T, PrimitiveType>) {
                switch (t.kind) {
                    case LiteralKind::Void: return 1;
                    case LiteralKind::Bool: return 1;
                    case LiteralKind::Char: return 1;
                    case LiteralKind::I32: return 4;
                    case LiteralKind::F32: return 4;
                    case LiteralKind::String: return 8; // Aligned to pointer
                    case LiteralKind::Null: return 8;   // null is pointer-aligned
                    default: break;
                }
                return 4;
            }
            else if constexpr (std::is_same_v<T, PointerType>) {
                return 8; // 64-bit pointer alignment
            }
            else if constexpr (std::is_same_v<T, ArrayType>) {
                if (t.size >= 0) {
                    // Fixed-size array: same alignment as element
                    return t.element->get_alignment();
                }
                return 8; // Dynamic array aligned to pointer
            }
            else if constexpr (std::is_same_v<T, FunctionType>) {
                return 8; // Function pointer alignment
            }
            else if constexpr (std::is_same_v<T, NamedType>) {
                if (!t.symbol) return 1;

                // Struct alignment = max of field alignments
                int max_align = 1;
                for (auto* member : t.symbol->member_order)
                {
                    if (auto* var = member->as<VariableSymbol>()) {
                        int field_align = var->type ? var->type->get_alignment() : 4;
                        if (field_align > max_align) max_align = field_align;
                    }
                }
                return max_align;
            }
            else if constexpr (std::is_same_v<T, GenericType>) {
                return 1;
            }
            else if constexpr (std::is_same_v<T, TypeParameter>) {
                return 1;
            }
            else if constexpr (std::is_same_v<T, UnresolvedType>) {
                return 1;
            }
            else if constexpr (std::is_same_v<T, MetaType>) {
                return 1;
            }
            else {
                return 4;
            }
        }, kind);
    }

    TypePtr Type::lower_references_to_ptrs(TypeSystem* type_system) const {
        return std::visit([type_system](const auto& t) -> TypePtr {
            using T = std::decay_t<decltype(t)>;

            if constexpr (std::is_same_v<T, PrimitiveType>) {
                // Primitives are value types, return as-is
                switch (t.kind) {
                    case LiteralKind::Void: return type_system->get_void();
                    case LiteralKind::Bool: return type_system->get_bool();
                    case LiteralKind::I32: return type_system->get_i32();
                    case LiteralKind::F32: return type_system->get_f32();
                    case LiteralKind::Null: return type_system->get_null();
                    default: return type_system->get_void(); // Fallback
                }
            }
            else if constexpr (std::is_same_v<T, PointerType>) {
                // Recursively lower the pointee, then wrap in pointer
                TypePtr lowered_pointee = t.pointee->lower_references_to_ptrs(type_system);
                return type_system->get_pointer(lowered_pointee);
            }
            else if constexpr (std::is_same_v<T, ArrayType>) {
                // Recursively lower the element type
                TypePtr lowered_element = t.element->lower_references_to_ptrs(type_system);
                return type_system->get_array(lowered_element, t.size);
            }
            else if constexpr (std::is_same_v<T, FunctionType>) {
                // Recursively lower return type and parameter types
                TypePtr lowered_return = t.returnType->lower_references_to_ptrs(type_system);
                std::vector<TypePtr> lowered_params;
                lowered_params.reserve(t.paramTypes.size());
                for (const auto& param : t.paramTypes) {
                    lowered_params.push_back(param->lower_references_to_ptrs(type_system));
                }
                return type_system->get_function(lowered_return, std::move(lowered_params));
            }
            else if constexpr (std::is_same_v<T, NamedType>) {
                // If it's a reference type, wrap in pointer; otherwise return as-is
                TypePtr named_type = type_system->get_named(t.symbol);
                if (t.symbol && t.symbol->is_ref()) {
                    return type_system->get_pointer(named_type);
                }
                return named_type;
            }
            else if constexpr (std::is_same_v<T, GenericType>) {
                // Recursively lower type arguments
                std::vector<TypePtr> lowered_args;
                lowered_args.reserve(t.typeArgs.size());
                for (const auto& arg : t.typeArgs) {
                    lowered_args.push_back(arg->lower_references_to_ptrs(type_system));
                }
                return type_system->get_generic(t.genericSymbol, std::move(lowered_args));
            }
            else if constexpr (std::is_same_v<T, TypeParameter>) {
                // Type parameters don't have concrete types to lower
                return type_system->get_type_parameter(t.name, t.index);
            }
            else if constexpr (std::is_same_v<T, UnresolvedType>) {
                // Unresolved types shouldn't be lowered
                return type_system->get_unresolved();
            }
            else if constexpr (std::is_same_v<T, MetaType>) {
                // Recursively lower the inner type
                TypePtr lowered_inner = t.inner ? t.inner->lower_references_to_ptrs(type_system) : nullptr;
                return type_system->get_type_type(lowered_inner);
            }
            else {
                return type_system->get_void(); // Fallback
            }
        }, kind);
    }

} // namespace Fern
