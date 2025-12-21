#pragma once

#include <string>
#include <vector>
#include <memory>
#include <map>
#include <unordered_map>
#include <variant>
#include "common/source_location.hpp"
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
    
    using SymbolPtr = Symbol*;
    
    
    #pragma region Symbol
    
    struct Symbol {
        SymbolKind kind;
        std::string name;
        SourceRange location;
        ModifierKindFlags modifiers = ModifierKindFlags::None;
        
        // Tree structure
        Symbol* parent = nullptr;
        
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
            Symbol* current = parent;
            while (current) {
                if (auto result = current->as<T>()) {
                    return result;
                }
                current = current->parent;
            }
            return nullptr;
        }
    };
    
    #pragma region Container Symbol
    
    struct ContainerSymbol : Symbol {
        // Children organized by name (multimap for overloads)
        // Using std::multimap instead of unordered_multimap to preserve insertion order
        std::multimap<std::string, std::unique_ptr<Symbol>> members;

        // Ordered list for deterministic iteration
        std::vector<Symbol*> member_order;
        
        // Add a member
        Symbol* add_member(std::unique_ptr<Symbol> symbol);
        
        // Lookup member by name (non-recursive)
        std::vector<Symbol*> get_member(const std::string& name);
        
        // Get all function overloads
        std::vector<FunctionSymbol*> get_functions(const std::string& name);
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

        // Get mangled name for code generation
        std::string get_mangled_name() const;

        // Check if signatures match (for overloading)
        bool signature_matches(FunctionSymbol* other) const;
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
