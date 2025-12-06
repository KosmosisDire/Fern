#include "type.hpp"
#include "symbol.hpp"

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
                    if (auto* field = member->as<FieldSymbol>()) {
                        int field_size = field->type ? field->type->get_size() : 4;
                        int field_align = field->type ? field->type->get_alignment() : 4;

                        // Align the current offset
                        int padding = (field_align - (size % field_align)) % field_align;
                        size += padding + field_size;

                        if (field_align > max_align) max_align = field_align;
                    }
                    else if (auto* var = member->as<VariableSymbol>()) {
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
                for (auto* member : t.symbol->member_order) {
                    if (auto* field = member->as<FieldSymbol>()) {
                        int field_align = field->type ? field->type->get_alignment() : 4;
                        if (field_align > max_align) max_align = field_align;
                    }
                    else if (auto* var = member->as<VariableSymbol>()) {
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
            else {
                return 4;
            }
        }, kind);
    }

} // namespace Fern
