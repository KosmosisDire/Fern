#include "type.hpp"
#include "symbol.hpp"

namespace Fern
{
    bool Type::is_void() const {
        return is<PrimitiveType>() && as<PrimitiveType>()->kind == PrimitiveKind::Void;
    }
    
    bool Type::is_value_type() const {
        if (is<PrimitiveType>()) return true;
        if (is<PointerType>()) return true;  // Pointers themselves are values
        if (is<ArrayType>()) return false;  // Arrays are reference types
        if (is<NamedType>()) {
            // Check if the type symbol is a value type (struct)
            return as<NamedType>()->symbol && !as<NamedType>()->symbol->isRef;
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
                    case PrimitiveKind::Void: return "void";
                    case PrimitiveKind::Bool: return "bool";
                    case PrimitiveKind::Char: return "char";
                    case PrimitiveKind::I8: return "i8";
                    case PrimitiveKind::I16: return "i16";
                    case PrimitiveKind::I32: return "i32";
                    case PrimitiveKind::I64: return "i64";
                    case PrimitiveKind::U8: return "u8";
                    case PrimitiveKind::U16: return "u16";
                    case PrimitiveKind::U32: return "u32";
                    case PrimitiveKind::U64: return "u64";
                    case PrimitiveKind::F32: return "f32";
                    case PrimitiveKind::F64: return "f64";
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
        // TODO: Implement proper size calculation based on type
        return 4;
    }

    int Type::get_alignment() const {
        // TODO: Implement proper alignment calculation based on type
        return 4;
    }

} // namespace Fern
