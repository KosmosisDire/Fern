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
    Symbol* resolve(const std::string& name);
    Symbol* resolve(const std::vector<std::string>& parts);
    Symbol* resolve_local(const std::string& name);
    Symbol* resolve_local(const std::vector<std::string>& parts);
    FunctionSymbol* resolve_function(const std::string& name, const std::vector<TypePtr>& arg_types);
    
    // Access to global namespace
    NamespaceSymbol* get_global_namespace();

    // Access to type system (needed by symbol_table_builder)
    TypeSystem& get_type_system() { return types; }
    const TypeSystem& get_type_system() const { return types; }

    // AST node to symbol mapping (for binding phase)
    void map_ast_to_symbol(BaseSyntax* ast_node, Symbol* symbol);
    Symbol* get_symbol_for_ast(BaseSyntax* ast_node);

    // Merge another symbol table into this one
    std::vector<std::string> merge(SymbolTable& other);

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
    void merge_namespace(NamespaceSymbol* target, NamespaceSymbol* source,
                        std::vector<std::string>& conflicts);

    // Update parent pointers recursively after moving a symbol
    void update_parent_pointers(Symbol* symbol, Symbol* new_parent);
};

} // namespace Fern