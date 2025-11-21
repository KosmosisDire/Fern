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
    struct FieldSymbol;
    struct ParameterSymbol;
    struct LocalSymbol;
    struct PropertySymbol;
    struct EnumCaseSymbol;
    
    using SymbolPtr = Symbol*;
    
    
    #pragma region Symbol
    
    struct Symbol {
        SymbolKind kind;
        std::string name;
        SourceRange location;
        AccessModifierKind access = AccessModifierKind::Private;
        
        // Tree structure
        Symbol* parent = nullptr;
        
        // Modifiers as simple flags
        bool isStatic = false;
        bool isAbstract = false;
        bool isVirtual = false;
        bool isOverride = false;
        bool isConst = false;
        bool isRef = false;  // For ref types or ref parameters
        
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
        // Using directives in this namespace
        std::vector<NamespaceSymbol*> using_namespaces;
        std::vector<TypeSymbol*> using_types;

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
        
        // Inheritance
        TypeSymbol* base_class = nullptr;
        std::vector<TypeSymbol*> interfaces;
        
        // For generics
        std::vector<Symbol*> type_parameters;  // Could be TypeParameterSymbol
        
        // Virtual method table
        std::vector<FunctionSymbol*> vtable;
        
        TypeSymbol(const std::string& name, TypePtr type);
        
        bool is_value_type() const;
        bool is_reference_type() const;
    };

    #pragma region Function Symbol
    
    struct FunctionSymbol : ContainerSymbol {
        // Signature
        TypePtr return_type;
        std::vector<ParameterSymbol*> parameters;  // Points to child parameter symbols

        // For generics
        std::vector<Symbol*> type_parameters;

        FunctionSymbol* overridden_method();
        uint32_t vtable_index = UINT32_MAX;

        // Special kinds
        bool is_constructor = false;
        bool is_operator = false;
        bool isExtern = false; 
        
        FunctionSymbol(const std::string& name, TypePtr return_type);
        
        // Get mangled name for code generation
        std::string get_mangled_name() const;
        
        // Check if signatures match (for overriding)
        bool signature_matches(FunctionSymbol* other) const;
    };

    #pragma region Variable Symbols

    // Base for all variables
    struct VariableSymbol : Symbol
    {
        
        TypePtr type;
        
        VariableSymbol(const std::string& name, TypePtr type);
    };

    struct FieldSymbol : VariableSymbol {
        uint32_t offset = 0;
        uint32_t alignment = 0;
        
        FieldSymbol(const std::string& name, TypePtr type);
    };

    struct ParameterSymbol : VariableSymbol {
        uint32_t index;
        bool has_default = false;
        bool is_ref = false;
        bool is_out = false;
        
        ParameterSymbol(const std::string& name, TypePtr type, uint32_t idx);
    };

    struct LocalSymbol : VariableSymbol {
        bool is_captured = false;
        
        LocalSymbol(const std::string& name, TypePtr type);
    };

    #pragma region Property Symbol
    
    // TODO: Maybe we sould actually be creating function symbols out of properties to begin with?
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
