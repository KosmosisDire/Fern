#pragma once

#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include <variant>
#include "common/source_location.hpp"
#include "common/ordermap/ordered_map.h"
#include "type.hpp"
#include "common/token.hpp"

namespace Fern
{
    // Forward declarations
    struct Symbol;
    struct NamespaceSymbol;
    struct TypeSymbol;
    struct FunctionSymbol;
    struct VariableSymbol;
    struct ParameterSymbol;
    struct PropertySymbol;
    struct EnumCaseSymbol;
    struct ContainerSymbol;
    
    using SymbolPtr = Symbol*;
    
    
    #pragma region Symbol
    
    struct Symbol {
        SymbolKind kind;
        std::string name;
        SourceRange location;
        ModifierKindFlags modifiers = ModifierKindFlags::None;
        
        // Tree structure
        ContainerSymbol* parent = nullptr;
        
        // Modifier helper functions
        bool is_static() const { return has_flag(modifiers, ModifierKindFlags::Static); }
        bool is_abstract() const { return has_flag(modifiers, ModifierKindFlags::Abstract); }
        bool is_virtual() const { return has_flag(modifiers, ModifierKindFlags::Virtual); }
        bool is_override() const { return has_flag(modifiers, ModifierKindFlags::Override); }
        bool is_ref() const { return has_flag(modifiers, ModifierKindFlags::Ref); }
        bool is_extern() const { return has_flag(modifiers, ModifierKindFlags::Extern); }
        bool is_public() const { return get_access_modifier() == AccessModifierKind::Public; }
        bool is_private() const { return get_access_modifier() == AccessModifierKind::Private; }
        bool is_protected() const { return get_access_modifier() == AccessModifierKind::Protected; }
        
        AccessModifierKind get_access_modifier() const {
            return Fern::get_access_modifier(modifiers);
        }
        
        virtual ~Symbol() = default;
        
        // Type-safe casting
        template<typename T>
        T* as() { return dynamic_cast<T*>(this); }
        
        template<typename T>  
        const T* as() const { return dynamic_cast<const T*>(this); }

        template<typename T>
        bool is() const { return dynamic_cast<const T*>(this) != nullptr; }
        
        // Get fully qualified name
        std::string get_qualified_name() const;
        
        // Find enclosing symbol of type
        template<typename T>
        T* get_enclosing() {
            Symbol* current = (Symbol*)parent;
            while (current) {
                if (auto result = current->as<T>()) {
                    return result;
                }
                current = (Symbol*)current->parent;
            }
            return nullptr;
        }
    };
    
    #pragma region Container Symbol
    
    struct ContainerSymbol : Symbol {
        // Children organized by name, preserving insertion order
        tsl::ordered_map<std::string, std::vector<std::unique_ptr<Symbol>>> members;

        // Add a member (for non-function symbols)
        Symbol* add_member(std::unique_ptr<Symbol> symbol);
        Symbol* get_member(const std::string& name);
        std::vector<Symbol*> get_members(const std::string& name);
        // get members of type template
        template<typename T>
        std::vector<T*> get_members(const std::string& name) {
            std::vector<T*> result;
            auto iter = members.find(name);
            if (iter != members.end()) {
                for (const auto& sym_ptr : iter->second) {
                    if (auto casted = sym_ptr->as<T>()) {
                        result.push_back(casted);
                    }
                }
            }
            return result;
        }

        // Flat iterator over all members, yielding (name, Symbol*) pairs
        class MemberIterator {
            using OuterIt = decltype(members)::iterator;
            using InnerIt = std::vector<std::unique_ptr<Symbol>>::const_iterator;

            OuterIt outer, outer_end;
            InnerIt inner;

            void skip_empty() {
                while (outer != outer_end && inner == outer->second.end()) {
                    if (++outer != outer_end) inner = outer->second.begin();
                }
            }

        public:
            using value_type = std::pair<const std::string&, Symbol*>;

            MemberIterator(OuterIt o, OuterIt end) : outer(o), outer_end(end) {
                if (outer != outer_end) { inner = outer->second.begin(); skip_empty(); }
            }

            value_type operator*() const { return {outer->first, inner->get()}; }

            MemberIterator& operator++() { ++inner; skip_empty(); return *this; }
            bool operator!=(const MemberIterator& o) const {
                return outer != o.outer || (outer != outer_end && inner != o.inner);
            }
        };

        MemberIterator begin() { return {members.begin(), members.end()}; }
        MemberIterator end() { return {members.end(), members.end()}; }
    };

    #pragma region Namespace Symbol
    
    struct NamespaceSymbol : ContainerSymbol {
        NamespaceSymbol(const std::string& name);
    };

    #pragma region Block Symbol

    struct BlockSymbol : ContainerSymbol {
        // Anonymous block scope for local variables
        // No special members needed - just a container for locals

        BlockSymbol(const std::string& debug_name);
    };

    #pragma region Type Symbol
    
    struct TypeSymbol : ContainerSymbol {
        TypePtr type;  // The semantic type this symbol represents
        bool has_fields() {
            for (auto [name, member_ptr] : *this) {
                if (member_ptr->is<VariableSymbol>()) {
                    return true;
                }
            }
            return false;
        }
        TypeSymbol(const std::string& name, TypePtr type);
    };

    #pragma region Function Symbol
    
    struct FunctionSymbol : ContainerSymbol {
        // Signature
        TypePtr return_type;
        std::vector<ParameterSymbol*> parameters;  // Points to child parameter symbols

        // Special kinds
        bool is_constructor = false;
        bool is_intrinsic = false;

        FunctionSymbol(const std::string& name, TypePtr return_type);

        // Check if signatures match (for overloading)
        bool signature_matches(FunctionSymbol* other) const;
        std::string get_signature() const;
    };

    #pragma region Variable Symbols

    // Base for all variables
    struct VariableSymbol : Symbol
    {
        TypePtr type;

        bool is_field() const
        {
            return parent && parent->is<TypeSymbol>();
        }
        
        VariableSymbol(const std::string& name, TypePtr type);
    };

    struct ParameterSymbol : VariableSymbol {
        uint32_t index;

        ParameterSymbol(const std::string& name, TypePtr type, uint32_t idx);
    };

    #pragma region Property Symbol
    
    struct PropertySymbol : ContainerSymbol {
        TypePtr type;
        bool has_getter = false;
        bool has_setter = false;
        
        PropertySymbol(const std::string& name, TypePtr type);
    };

    #pragma region Enum Case Symbol
    
    struct EnumCaseSymbol : Symbol {
        std::vector<TypePtr> associated_types;  // For tagged enums
        uint32_t value = 0;  // Discriminant value
        
        EnumCaseSymbol(const std::string& name);
    };
    

} // namespace Fern
