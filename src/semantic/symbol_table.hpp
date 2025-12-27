#pragma once

#include <string>
#include <memory>
#include <vector>
#include <unordered_map>
#include "symbol.hpp"
#include "type_system.hpp"

namespace Fern
{

// Forward declaration
struct BaseSyntax;

class SymbolTable {
public:
    explicit SymbolTable(TypeSystem& type_system);
    
    // Scope management
    void push_scope(Symbol* scope);
    void pop_scope();
    Symbol* get_current_scope();
    ContainerSymbol* get_current_container();
    
    // Symbol definition
    NamespaceSymbol* define_namespace(const std::string& name);
    BlockSymbol* define_block(const std::string& debug_name);
    TypeSymbol* define_type(const std::string& name, TypePtr type);
    FunctionSymbol* define_function(const std::string& name, TypePtr return_type);
    VariableSymbol* define_variable(const std::string& name, TypePtr type);
    PropertySymbol* define_property(const std::string& name, TypePtr type);
    ParameterSymbol* define_parameter(const std::string& name, TypePtr type, uint32_t index);
    
    // Symbol resolution
    FunctionSymbol* resolve_function(const std::string& name, std::vector<TypePtr> arg_types);
    Symbol* resolve_single(const std::string& name);
    Symbol* resolve_single(const std::vector<std::string>& parts);
    std::vector<Symbol*> resolve(const std::string& name);
    std::vector<Symbol*> resolve(const std::vector<std::string>& parts);

    // Type-filtered symbol resolution
    template<typename T>
    T* resolve_single(const std::string& name)
    {
        auto symbols = resolve(name);
        for (auto* sym : symbols)
        {
            if (auto* typed = sym->as<T>())
                return typed;
        }
        return nullptr;
    }

    template<typename T>
    T* resolve_single(const std::vector<std::string>& parts)
    {
        auto symbols = resolve(parts);
        for (auto* sym : symbols)
        {
            if (auto* typed = sym->as<T>())
                return typed;
        }
        return nullptr;
    }

    FunctionSymbol* resolve_function_local(const std::string& name, std::vector<TypePtr> arg_types);
    Symbol* resolve_single_local(const std::string& name);
    Symbol* resolve_single_local(const std::vector<std::string>& parts);
    std::vector<Symbol*> resolve_local(const std::string& name);
    std::vector<Symbol*> resolve_local(const std::vector<std::string>& parts);
    
    // Access to global namespace
    NamespaceSymbol* get_global_namespace();

    // Access to type system (needed by symbol_table_builder)
    TypeSystem& get_type_system() { return types; }
    const TypeSystem& get_type_system() const { return types; }

    // AST node to symbol mapping (for binding phase)
    void map_ast_to_symbol(BaseSyntax* ast_node, Symbol* symbol);
    Symbol* get_symbol_for_ast(BaseSyntax* ast_node);

    // Merge another symbol table into this one
    void merge(SymbolTable& other);

    // Debugging
    std::string to_string() const;

private:
    TypeSystem& types;
    std::unique_ptr<NamespaceSymbol> global_namespace;
    Symbol* current_scope = nullptr;

    // Counter for generating unique anonymous block names
    uint32_t next_block_id = 0;

    // Mapping from AST nodes to their corresponding symbols
    std::unordered_map<BaseSyntax*, Symbol*> ast_to_symbol_map;

    // Recursively merge source namespace into target namespace
    void merge_namespace(NamespaceSymbol* target, NamespaceSymbol* source);

    // Update parent pointers recursively after moving a symbol
    void update_parent_pointers(Symbol* symbol, ContainerSymbol* new_parent);
};

} // namespace Fern