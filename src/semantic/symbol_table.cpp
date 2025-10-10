#include "symbol_table.hpp"
#include <sstream>
#include <functional>
#include <iostream>

namespace Fern
{

SymbolTable::SymbolTable(TypeSystem& type_system) 
    : types(type_system),
      global_namespace(std::make_unique<NamespaceSymbol>("")) {
    current_scope = global_namespace.get();
}

ContainerSymbol* SymbolTable::get_current_container() {
    return current_scope->as<ContainerSymbol>();
}

void SymbolTable::push_scope(Symbol* scope) {
    current_scope = scope;
}

void SymbolTable::pop_scope() {
    if (current_scope && current_scope->parent) {
        current_scope = current_scope->parent;
    }
}

Symbol* SymbolTable::get_current_scope() { 
    return current_scope; 
}

NamespaceSymbol* SymbolTable::define_namespace(const std::string& name) {
    auto ns = std::make_unique<NamespaceSymbol>(name);
    auto container = current_scope->as<ContainerSymbol>();
    if (!container) return nullptr;
    return static_cast<NamespaceSymbol*>(container->add_member(std::move(ns)));
}

BlockSymbol* SymbolTable::define_block(const std::string& debug_name) {
    auto block = std::make_unique<BlockSymbol>(debug_name);
    auto container = current_scope->as<ContainerSymbol>();
    if (!container) return nullptr;
    return static_cast<BlockSymbol*>(container->add_member(std::move(block)));
}

TypeSymbol* SymbolTable::define_type(const std::string& name, TypePtr type) {
    auto sym = std::make_unique<TypeSymbol>(name, type);
    auto container = current_scope->as<ContainerSymbol>();
    if (!container) return nullptr;
    return static_cast<TypeSymbol*>(container->add_member(std::move(sym)));
}

FunctionSymbol* SymbolTable::define_function(const std::string& name, TypePtr return_type) {
    auto sym = std::make_unique<FunctionSymbol>(name, return_type);
    auto container = current_scope->as<ContainerSymbol>();
    if (!container) return nullptr;
    return static_cast<FunctionSymbol*>(container->add_member(std::move(sym)));
}

FieldSymbol* SymbolTable::define_field(const std::string& name, TypePtr type) {
    auto sym = std::make_unique<FieldSymbol>(name, type);
    return static_cast<FieldSymbol*>(
        get_current_container()->add_member(std::move(sym))
    );
}

PropertySymbol* SymbolTable::define_property(const std::string& name, TypePtr type) {
    auto sym = std::make_unique<PropertySymbol>(name, type);
    return static_cast<PropertySymbol*>(
        get_current_container()->add_member(std::move(sym))
    );
}

ParameterSymbol* SymbolTable::define_parameter(const std::string& name, TypePtr type, uint32_t index) {
    auto sym = std::make_unique<ParameterSymbol>(name, type, index);
    return static_cast<ParameterSymbol*>(
        get_current_container()->add_member(std::move(sym))
    );
}

LocalSymbol* SymbolTable::define_local(const std::string& name, TypePtr type) {
    auto sym = std::make_unique<LocalSymbol>(name, type);
    return static_cast<LocalSymbol*>(
        get_current_container()->add_member(std::move(sym))
    );
}

Symbol* SymbolTable::resolve(const std::string& name) {
    // Walk up scopes looking for name
    Symbol* scope = current_scope;
    while (scope) {
        if (auto container = scope->as<ContainerSymbol>()) {
            auto members = container->get_member(name);
            if (!members.empty()) {
                return members[0];
            }
        }
        scope = scope->parent;
    }
    
    return nullptr;
}

Symbol* SymbolTable::resolve(const std::vector<std::string>& parts) {
    // convert to name, then call resolve
    if (parts.empty()) return nullptr;
    std::string name = parts[0];
    for (size_t i = 1; i < parts.size(); ++i) {
        name += "." + parts[i];
    }
    return resolve(name);
}

Symbol* SymbolTable::resolve_local(const std::string& name) {
    if (auto container = current_scope->as<ContainerSymbol>()) {
        auto members = container->get_member(name);
        if (!members.empty()) {
            return members[0]; // TODO: Handle ambiguity
        }
    }
    return nullptr;
}

Symbol* SymbolTable::resolve_local(const std::vector<std::string>& parts) {
    if (parts.empty()) return nullptr;
    // just look up the last part in the current scope
    return resolve_local(parts.back());
}

FunctionSymbol* SymbolTable::resolve_function(const std::string& name, const std::vector<TypePtr>& arg_types) {
    std::vector<FunctionSymbol*> candidates;
    
    // Collect all functions with this name
    Symbol* scope = current_scope;
    while (scope) {
        if (auto container = scope->as<ContainerSymbol>()) {
            auto funcs = container->get_functions(name);
            candidates.insert(candidates.end(), funcs.begin(), funcs.end());
        }
        scope = scope->parent;
    }
    
    // Simple overload resolution (exact match only for now)
    for (auto func : candidates) {
        if (func->parameters.size() != arg_types.size()) continue;
        
        bool matches = true;
        for (size_t i = 0; i < arg_types.size(); i++) {
            if (func->parameters[i]->type != arg_types[i]) {
                matches = false;
                break;
            }
        }
        
        if (matches) return func;
    }
    
    return nullptr;
}


NamespaceSymbol* SymbolTable::get_global_namespace() {
    return global_namespace.get();
}

void SymbolTable::map_ast_to_symbol(BaseSyntax* ast_node, Symbol* symbol) {
    if (ast_node && symbol) {
        ast_to_symbol_map[ast_node] = symbol;
    }
}

Symbol* SymbolTable::get_symbol_for_ast(BaseSyntax* ast_node) {
    if (!ast_node) return nullptr;
    auto it = ast_to_symbol_map.find(ast_node);
    if (it != ast_to_symbol_map.end()) {
        return it->second;
    }
    return nullptr;
}

std::vector<std::string> SymbolTable::merge(SymbolTable& other) {
    std::vector<std::string> conflicts;

    // Merge AST to symbol mappings
    for (auto& [ast_node, symbol] : other.ast_to_symbol_map) {
        ast_to_symbol_map[ast_node] = symbol;
    }

    // If we don't have a global namespace yet, adopt theirs
    if (!global_namespace && other.global_namespace) {
        global_namespace = std::move(other.global_namespace);
        current_scope = global_namespace.get();
        return conflicts;
    }

    // If they don't have a global namespace, nothing to merge
    if (!other.global_namespace) {
        return conflicts;
    }
    
    // Both have global namespaces - merge them
    merge_namespace(global_namespace.get(), other.global_namespace.get(), conflicts);
    
    // Clear other's state (global_namespace already cleared by move operations)
    other.current_scope = nullptr;
    
    return conflicts;
}

void SymbolTable::merge_namespace(NamespaceSymbol* target, NamespaceSymbol* source, 
                                 std::vector<std::string>& conflicts) {
    if (!target || !source) return;
    
    auto target_container = static_cast<ContainerSymbol*>(target);
    auto source_container = static_cast<ContainerSymbol*>(source);
    
    // We need to move all symbols from source to target
    // Since we're using unique_ptr, we need to extract and move them
    
    // First, collect all symbols we need to process (can't modify while iterating)
    std::vector<std::pair<std::string, std::unique_ptr<Symbol>>> symbols_to_move;
    
    // Extract all symbols from source
    for (auto it = source_container->members.begin(); it != source_container->members.end();) {
        symbols_to_move.push_back(std::make_pair(it->first, std::move(it->second)));
        it = source_container->members.erase(it);
    }
    
    // Clear member_order as we've moved the symbols
    source_container->member_order.clear();
    
    // Now process each symbol
    for (auto& [name, symbol_ptr] : symbols_to_move) {
        // Get existing symbols with this name in target
        auto existing_symbols = target_container->get_member(name);
        
        if (!existing_symbols.empty()) {
            // Symbol exists in both - check if we can merge
            bool merged = false;
            
            // Case 1: Both are namespaces - merge recursively
            if (existing_symbols.size() == 1) {
                auto source_ns = symbol_ptr->as<NamespaceSymbol>();
                auto target_ns = existing_symbols[0]->as<NamespaceSymbol>();
                
                if (source_ns && target_ns) {
                    // Recursively merge the namespaces
                    merge_namespace(target_ns, source_ns, conflicts);
                    merged = true;
                    // Don't add the namespace itself, we've merged its contents
                }
            }
            
            // Case 2: Function overloading
            if (!merged) {
                auto source_func = symbol_ptr->as<FunctionSymbol>();
                if (source_func) {
                    // Check if all existing symbols are functions (for overloading)
                    bool all_functions = true;
                    bool has_conflict = false;

                    for (auto existing : existing_symbols) {
                        auto existing_func = existing->as<FunctionSymbol>();
                        if (!existing_func) {
                            all_functions = false;
                            break;
                        }

                        // Check for signature conflict
                        if (source_func->signature_matches(existing_func)) {
                            conflicts.push_back("Function '" + source_func->get_mangled_name() +
                                              "' with same signature already exists in '" +
                                              target->get_qualified_name() + "'");
                            has_conflict = true;
                            break;
                        }
                    }

                    if (all_functions && !has_conflict) {
                        // No conflict - add as overload
                        symbol_ptr->parent = target;
                        target_container->add_member(std::move(symbol_ptr));
                        merged = true;
                    }
                    else
                    {
                    }
                }
            }

            if (!merged && symbol_ptr) {
                // Conflict - report it
                conflicts.push_back("Symbol conflict: '" + name +
                                  "' already exists in namespace '" +
                                  target->get_qualified_name() + "'");
            }
        } else {
            // No conflict - transfer symbol to target
            symbol_ptr->parent = target;
            
            // If it's a type or namespace, recursively update parent pointers
            update_parent_pointers(symbol_ptr.get(), target);
            
            target_container->add_member(std::move(symbol_ptr));
        }
    }
}

void SymbolTable::update_parent_pointers(Symbol* symbol, Symbol* new_parent) {
    symbol->parent = new_parent;
    
    // If this symbol is a container, update its children's parent pointers
    if (auto container = symbol->as<ContainerSymbol>()) {
        for (auto child : container->member_order) {
            if (child && child->parent == symbol) {
                // Child's parent is already correct (points to symbol)
                // But recursively update grandchildren if needed
                update_parent_pointers(child, symbol);
            }
        }
    }
}


std::string SymbolTable::to_string() const {
    std::stringstream ss;
    ss << "=== SYMBOL TABLE ===\n";
    
    // Print current scope info
    if (current_scope) {
        ss << "Current Scope: ";
        if (current_scope == global_namespace.get()) {
            ss << "global namespace\n";
        } else {
            ss << current_scope->get_qualified_name() << " (" << Symbol::kind_name(current_scope->kind) << ")\n";
        }
    }
    
    ss << "\nScope Hierarchy:\n";
    
    // Recursive function to print the symbol tree
    std::function<void(Symbol*, int)> print_tree = [&](Symbol* sym, int indent) {
        std::string indent_str(indent * 2, ' ');
        
        // Print symbol info
        ss << indent_str;
        
        // Print kind
        switch (sym->kind) {
            case SymbolKind::Namespace: ss << "namespace"; break;
            case SymbolKind::Type: ss << "type"; break;
            case SymbolKind::Function: ss << "function"; break;
            case SymbolKind::Variable: ss << "variable"; break;
            case SymbolKind::Property: ss << "property"; break;
            case SymbolKind::EnumCase: ss << "enum_case"; break;
            case SymbolKind::Block: ss << "block"; break;
        }
        
        ss << " " << sym->name;
        
        // Add access modifier if not public
        if (sym->access != Accessibility::Public) {
            ss << " [" << Symbol::access_name(sym->access) << "]";
        }
        
        // Add modifiers
        std::vector<std::string> modifiers;
        if (sym->isStatic) modifiers.push_back("static");
        if (sym->isAbstract) modifiers.push_back("abstract");
        if (sym->isVirtual) modifiers.push_back("virtual");
        if (sym->isOverride) modifiers.push_back("override");
        if (sym->isConst) modifiers.push_back("const");
        if (sym->isRef) modifiers.push_back("ref");
        
        if (!modifiers.empty()) {
            ss << " (";
            for (size_t i = 0; i < modifiers.size(); ++i) {
                if (i > 0) ss << ", ";
                ss << modifiers[i];
            }
            ss << ")";
        }
        
        // Add type-specific information
        if (auto type_sym = sym->as<TypeSymbol>()) {
            if (type_sym->type) {
                ss << " : " << type_sym->type->get_name();
            }
            if (type_sym->base_class) {
                ss << " extends " << type_sym->base_class->name;
            }
            if (!type_sym->interfaces.empty()) {
                ss << " implements ";
                for (size_t i = 0; i < type_sym->interfaces.size(); ++i) {
                    if (i > 0) ss << ", ";
                    ss << type_sym->interfaces[i]->name;
                }
            }
        }
        else if (auto func_sym = sym->as<FunctionSymbol>()) {
            ss << "(";
            for (size_t i = 0; i < func_sym->parameters.size(); ++i) {
                if (i > 0) ss << ", ";
                auto param = func_sym->parameters[i];
                ss << param->name << ": ";
                if (param->type) {
                    ss << param->type->get_name();
                } else {
                    ss << "?";
                }
            }
            ss << ")";
            if (func_sym->return_type) {
                ss << " -> " << func_sym->return_type->get_name();
            }
            if (func_sym->is_constructor) {
                ss << " [constructor]";
            }
            if (func_sym->is_operator) {
                ss << " [operator]";
            }
            if (func_sym->overridden_method()) {
                ss << " [overrides " << func_sym->overridden_method()->name << "]";
            }
        }
        else if (auto var_sym = sym->as<VariableSymbol>()) {
            if (var_sym->type) {
                ss << " : " << var_sym->type->get_name();
            }
            
            // Add variable-specific info
            if (auto field = sym->as<FieldSymbol>()) {
                ss << " [offset=" << field->offset << ", align=" << field->alignment << "]";
            }
            else if (auto param = sym->as<ParameterSymbol>()) {
                ss << " [param #" << param->index << "]";
                if (param->has_default) ss << " [has default]";
                if (param->is_ref) ss << " [ref]";
                if (param->is_out) ss << " [out]";
            }
            else if (auto local = sym->as<LocalSymbol>()) {
                ss << " [local]";
                if (local->is_captured) ss << " [captured]";
            }
        }
        else if (auto prop_sym = sym->as<PropertySymbol>()) {
            if (prop_sym->type) {
                ss << " : " << prop_sym->type->get_name();
            }
            ss << " { ";
            if (prop_sym->has_getter) ss << "get; ";
            if (prop_sym->has_setter) ss << "set; ";
            ss << "}";
        }
        else if (auto enum_case = sym->as<EnumCaseSymbol>()) {
            if (!enum_case->associated_types.empty()) {
                ss << "(";
                for (size_t i = 0; i < enum_case->associated_types.size(); ++i) {
                    if (i > 0) ss << ", ";
                    ss << enum_case->associated_types[i]->get_name();
                }
                ss << ")";
            }
            ss << " = " << enum_case->value;
        }
        
        // Check if this symbol is a container with members
        if (auto container = sym->as<ContainerSymbol>()) {
            if (!container->member_order.empty()) {
                ss << " {\n";
                
                // Print members in order
                for (auto member : container->member_order) {
                    print_tree(member, indent + 1);
                }
                
                ss << indent_str << "}";
            }
        }
        
        ss << "\n";
    };
    
    // Print the global namespace tree
    print_tree(global_namespace.get(), 0);
    
    return ss.str();
}

} // namespace Fern