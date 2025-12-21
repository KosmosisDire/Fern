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
        if (is<ArrayType>()) return true;    // Arrays are value types (copy semantics)
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

} // namespace Fern
