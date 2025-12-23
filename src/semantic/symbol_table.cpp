#include "symbol_table.hpp"
#include <sstream>
#include <functional>
#include <iostream>

namespace Fern
{

    SymbolTable::SymbolTable(TypeSystem &type_system)
        : types(type_system),
          global_namespace(std::make_unique<NamespaceSymbol>(""))
    {
        current_scope = global_namespace.get();
    }

    ContainerSymbol *SymbolTable::get_current_container()
    {
        return current_scope->as<ContainerSymbol>();
    }

    void SymbolTable::push_scope(Symbol *scope)
    {
        current_scope = scope;
    }

    void SymbolTable::pop_scope()
    {
        if (current_scope && current_scope->parent)
        {
            current_scope = current_scope->parent;
        }
    }

    Symbol *SymbolTable::get_current_scope()
    {
        return current_scope;
    }

    NamespaceSymbol *SymbolTable::define_namespace(const std::string &name)
    {
        auto ns = std::make_unique<NamespaceSymbol>(name);
        auto container = current_scope->as<ContainerSymbol>();
        if (!container)
            return nullptr;
        return static_cast<NamespaceSymbol *>(container->add_member(std::move(ns)));
    }

    BlockSymbol *SymbolTable::define_block(const std::string &debug_name)
    {
        // Generate unique name to avoid collisions in the members map
        std::string unique_name = debug_name + "_" + std::to_string(next_block_id++);
        auto block = std::make_unique<BlockSymbol>(unique_name);
        auto container = current_scope->as<ContainerSymbol>();
        if (!container)
            return nullptr;
        return static_cast<BlockSymbol *>(container->add_member(std::move(block)));
    }

    TypeSymbol *SymbolTable::define_type(const std::string &name, TypePtr type)
    {
        auto sym = std::make_unique<TypeSymbol>(name, type);
        auto container = current_scope->as<ContainerSymbol>();
        if (!container)
            return nullptr;
        return static_cast<TypeSymbol *>(container->add_member(std::move(sym)));
    }

    FunctionSymbol *SymbolTable::define_function(const std::string &name, TypePtr return_type)
    {
        auto sym = std::make_unique<FunctionSymbol>(name, return_type);
        auto container = current_scope->as<ContainerSymbol>();
        if (!container)
            return nullptr;
        return static_cast<FunctionSymbol *>(container->add_member(std::move(sym)));
    }

    VariableSymbol *SymbolTable::define_variable(const std::string &name, TypePtr type)
    {
        auto sym = std::make_unique<VariableSymbol>(name, type);
        return static_cast<VariableSymbol *>(
            get_current_container()->add_member(std::move(sym)));
    }

    PropertySymbol *SymbolTable::define_property(const std::string &name, TypePtr type)
    {
        auto sym = std::make_unique<PropertySymbol>(name, type);
        return static_cast<PropertySymbol *>(
            get_current_container()->add_member(std::move(sym)));
    }

    ParameterSymbol *SymbolTable::define_parameter(const std::string &name, TypePtr type, uint32_t index)
    {
        auto sym = std::make_unique<ParameterSymbol>(name, type, index);
        return static_cast<ParameterSymbol *>(
            get_current_container()->add_member(std::move(sym)));
    }

    Symbol *SymbolTable::resolve(const std::string &name)
    {
        // Walk up scopes looking for name
        Symbol *scope = current_scope;
        while (scope)
        {
            if (scope->kind == SymbolKind::FunctionGroup)
            {
                throw std::runtime_error("Corrupted scope chain: FunctionGroupSymbol found in scope chain while resolving \"" + name + "\"");
            }

            if (auto container = scope->as<ContainerSymbol>())
            {
                if (auto member = container->get_member(name))
                {
                    return member;
                }
            }

            scope = scope->parent;
        }

        return nullptr;
    }

    Symbol *SymbolTable::resolve(const std::vector<std::string> &parts)
    {
        // convert to name, then call resolve
        if (parts.empty())
            return nullptr;
        std::string name = parts[0];
        for (size_t i = 1; i < parts.size(); ++i)
        {
            name += "." + parts[i];
        }
        return resolve(name);
    }

    Symbol *SymbolTable::resolve_local(const std::string &name)
    {
        if (auto container = current_scope->as<ContainerSymbol>())
        {
            return container->get_member(name);
        }
        return nullptr;
    }

    Symbol *SymbolTable::resolve_local(const std::vector<std::string> &parts)
    {
        if (parts.empty())
            return nullptr;
        // just look up the last part in the current scope
        return resolve_local(parts.back());
    }

    FunctionSymbol *SymbolTable::resolve_function(const std::string &name, const std::vector<TypePtr> &arg_types)
    {
        std::vector<FunctionSymbol *> candidates;

        // Collect all functions with this name
        Symbol *scope = current_scope;
        while (scope)
        {
            if (auto container = scope->as<ContainerSymbol>())
            {
                auto funcs = container->get_functions(name);
                candidates.insert(candidates.end(), funcs.begin(), funcs.end());
            }
            scope = scope->parent;
        }

        // Simple overload resolution (exact match only for now)
        for (auto func : candidates)
        {
            if (func->parameters.size() != arg_types.size())
                continue;

            bool matches = true;
            for (size_t i = 0; i < arg_types.size(); i++)
            {
                if (func->parameters[i]->type != arg_types[i])
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
                return func;
        }

        return nullptr;
    }

    NamespaceSymbol *SymbolTable::get_global_namespace()
    {
        return global_namespace.get();
    }

    void SymbolTable::map_ast_to_symbol(BaseSyntax *ast_node, Symbol *symbol)
    {
        if (ast_node && symbol)
        {
            ast_to_symbol_map[ast_node] = symbol;
        }
    }

    Symbol *SymbolTable::get_symbol_for_ast(BaseSyntax *ast_node)
    {
        if (!ast_node)
            return nullptr;
        auto it = ast_to_symbol_map.find(ast_node);
        if (it != ast_to_symbol_map.end())
        {
            return it->second;
        }
        return nullptr;
    }

    std::vector<std::string> SymbolTable::merge(SymbolTable &other)
    {
        std::vector<std::string> conflicts;

        // Merge AST to symbol mappings
        for (auto &[ast_node, symbol] : other.ast_to_symbol_map)
        {
            ast_to_symbol_map[ast_node] = symbol;
        }

        // If we don't have a global namespace yet, adopt theirs
        if (!global_namespace && other.global_namespace)
        {
            global_namespace = std::move(other.global_namespace);
            current_scope = global_namespace.get();
            return conflicts;
        }

        // If they don't have a global namespace, nothing to merge
        if (!other.global_namespace)
        {
            return conflicts;
        }

        // Both have global namespaces - merge them
        merge_namespace(global_namespace.get(), other.global_namespace.get(), conflicts);

        // Clear other's state (global_namespace already cleared by move operations)
        other.current_scope = nullptr;

        return conflicts;
    }

    void SymbolTable::merge_namespace(NamespaceSymbol *target, NamespaceSymbol *source,
                                      std::vector<std::string> &conflicts)
    {
        if (!target || !source)
            return;

        auto target_container = static_cast<ContainerSymbol *>(target);
        auto source_container = static_cast<ContainerSymbol *>(source);

        // We need to move all symbols from source to target
        // Since we're using unique_ptr, we need to extract and move them

        // First, collect all symbols we need to process (can't modify while iterating)
        std::vector<std::pair<std::string, std::unique_ptr<Symbol>>> symbols_to_move;

        // Extract all symbols from source
        // tsl::ordered_map requires it.value() for mutable access
        for (auto it = source_container->members.begin(); it != source_container->members.end();)
        {
            symbols_to_move.emplace_back(it->first, std::move(it.value()));
            it = source_container->members.erase(it);
        }

        // Now process each symbol
        for (auto &[name, symbol_ptr] : symbols_to_move)
        {
            // Get existing symbol with this name in target
            auto existing_symbol = target_container->get_member(name);

            if (existing_symbol)
            {
                bool merged = false;

                // Both are namespaces
                auto source_ns = symbol_ptr->as<NamespaceSymbol>();
                auto target_ns = existing_symbol->as<NamespaceSymbol>();
                if (source_ns && target_ns)
                {
                    // Recursively merge the namespaces
                    merge_namespace(target_ns, source_ns, conflicts);
                    merged = true;
                }

                // Function group merging (for function overloads)
                if (!merged)
                {
                    auto source_group = symbol_ptr->as<FunctionGroupSymbol>();
                    auto target_group = existing_symbol->as<FunctionGroupSymbol>();

                    if (source_group && target_group)
                    {
                        bool has_conflict = false;
                        for (auto &source_func : source_group->overloads)
                        {
                            // Check for signature conflict with existing overloads
                            bool signature_exists = false;
                            for (auto *target_func : target_group->get_overloads())
                            {
                                if (source_func->signature_matches(target_func))
                                {
                                    conflicts.push_back("Function '" + source_func->get_signature() +
                                                        "' with same signature already exists in '" +
                                                        target->get_qualified_name() + "'");
                                    signature_exists = true;
                                    has_conflict = true;
                                    break;
                                }
                            }

                            if (!signature_exists)
                            {
                                // Add this overload to target group
                                // Note: add_overload sets func->parent = target namespace
                                // We also need to update the function's children (blocks, etc.)
                                FunctionSymbol *added = target_group->add_overload(std::move(source_func));
                                if (added)
                                {
                                    // Recursively update children's parent pointers
                                    // (children point to FunctionSymbol, which is correct,
                                    // but we need to traverse to handle any nested structures)
                                    update_parent_pointers(added, added->parent);
                                }
                            }
                        }

                        merged = true;
                    }
                }

                if (!merged && symbol_ptr)
                {
                    // Conflict - report it
                    conflicts.push_back("Symbol conflict: '" + name +
                                        "' already exists in namespace '" +
                                        target->get_qualified_name() + "'");
                }
            }
            else
            {
                // No conflict - transfer symbol to target
                symbol_ptr->parent = target;

                // If it's a type or namespace, recursively update parent pointers
                update_parent_pointers(symbol_ptr.get(), target);

                target_container->add_member(std::move(symbol_ptr));
            }
        }
    }

    void SymbolTable::update_parent_pointers(Symbol *symbol, Symbol *new_parent)
    {
        symbol->parent = new_parent;

        // Handle FunctionGroupSymbol - update the parent of all overloads
        // FunctionSymbol's parent is the container, not the group
        if (auto func_group = symbol->as<FunctionGroupSymbol>())
        {
            for (auto *overload : func_group->get_overloads())
            {
                overload->parent = new_parent;
                // Recursively update children of the function (parameters, locals, etc.)
                update_parent_pointers(overload, new_parent);
            }
            return; // FunctionGroupSymbol is not a ContainerSymbol, so we're done
        }

        // If this symbol is a container, update its children's parent pointers
        if (auto container = symbol->as<ContainerSymbol>())
        {
            for (auto &[name, child_ptr] : container->members)
            {
                auto *child = child_ptr.get();
                if (child && child->parent == symbol)
                {
                    // Child's parent is already correct (points to symbol)
                    // But recursively update grandchildren if needed
                    update_parent_pointers(child, symbol);
                }
            }
        }
    }

    std::string SymbolTable::to_string() const
    {
        std::stringstream ss;
        ss << "=== SYMBOL TABLE ===\n";

        // Print current scope info
        if (current_scope)
        {
            ss << "Current Scope: ";
            if (current_scope == global_namespace.get())
            {
                ss << "global namespace\n";
            }
            else
            {
                ss << current_scope->get_qualified_name() << " (" << Fern::to_string(current_scope->kind) << ")\n";
            }
        }

        ss << "\nScope Hierarchy:\n";

        // Recursive function to print the symbol tree
        std::function<void(Symbol *, int)> print_tree = [&](Symbol *sym, int indent)
        {
            std::string indent_str(indent * 2, ' ');

            // Print symbol info
            ss << indent_str;

            // Add modifiers
            ss << Fern::to_string(sym->modifiers);

            if (sym->modifiers != ModifierKindFlags::None)
            {
                ss << " ";
            }

            // Print kind
            if (sym->kind != SymbolKind::Variable)
            {
                ss << Fern::to_string(sym->kind);
                ss << " " << sym->name;
            }

            // Add type-specific information
            if (auto type_sym = sym->as<TypeSymbol>())
            {
                if (type_sym->type)
                {
                    ss << " : " << type_sym->type->get_name();
                }
            }
            else if (auto group_sym = sym->as<FunctionGroupSymbol>())
            {
                // Print each overload in the group
                auto overloads = group_sym->get_overloads();
                if (overloads.size() == 1)
                {
                    // Single overload - print inline
                    auto func_sym = overloads[0];
                    ss << "(";
                    for (size_t i = 0; i < func_sym->parameters.size(); ++i)
                    {
                        if (i > 0)
                            ss << ", ";
                        auto param = func_sym->parameters[i];
                        if (param->type)
                        {
                            ss << param->type->get_name();
                        }
                        else
                        {
                            ss << "?";
                        }
                        ss << " " << param->name;
                    }
                    ss << ")";
                    if (func_sym->return_type)
                    {
                        ss << " -> " << func_sym->return_type->get_name();
                    }
                    if (func_sym->is_constructor)
                    {
                        ss << " [constructor]";
                    }
                }
                else
                {
                    // Multiple overloads - print count
                    ss << " [" << overloads.size() << " overloads]";
                }
            }
            else if (auto func_sym = sym->as<FunctionSymbol>())
            {
                ss << "(";
                for (size_t i = 0; i < func_sym->parameters.size(); ++i)
                {
                    if (i > 0)
                        ss << ", ";
                    auto param = func_sym->parameters[i];
                    if (param->type)
                    {
                        ss << param->type->get_name();
                    }
                    else
                    {
                        ss << "?";
                    }
                    ss << " " << param->name;
                }
                ss << ")";
                if (func_sym->return_type)
                {
                    ss << " -> " << func_sym->return_type->get_name();
                }
                if (func_sym->is_constructor)
                {
                    ss << " [constructor]";
                }
            }
            else if (auto var_sym = sym->as<VariableSymbol>())
            {
                if (auto param = sym->as<ParameterSymbol>())
                {
                    ss << "param";
                    ss << " ";
                }
                if (var_sym->type)
                {
                    ss << var_sym->type->get_name();
                }
                else
                {
                    ss << "?";
                }
                ss << " " << sym->name;
            }
            else if (auto prop_sym = sym->as<PropertySymbol>())
            {
                if (prop_sym->type)
                {
                    ss << " : " << prop_sym->type->get_name();
                }
                ss << " { ";
                if (prop_sym->has_getter)
                    ss << "get; ";
                if (prop_sym->has_setter)
                    ss << "set; ";
                ss << "}";
            }
            else if (auto enum_case = sym->as<EnumCaseSymbol>())
            {
                if (!enum_case->associated_types.empty())
                {
                    ss << "(";
                    for (size_t i = 0; i < enum_case->associated_types.size(); ++i)
                    {
                        if (i > 0)
                            ss << ", ";
                        ss << enum_case->associated_types[i]->get_name();
                    }
                    ss << ")";
                }
                ss << " = " << enum_case->value;
            }

            // Check if this symbol is a function group - print its overloads
            if (auto func_group = sym->as<FunctionGroupSymbol>())
            {
                auto overloads = func_group->get_overloads();
                if (overloads.size() == 1)
                {
                    // Single overload - print function's children directly (skip showing group)
                    auto *func = overloads[0];
                    if (!func->members.empty())
                    {
                        ss << " {\n";
                        for (auto &[name, member_ptr] : func->members)
                        {
                            print_tree(member_ptr.get(), indent + 1);
                        }
                        ss << indent_str << "}";
                    }
                }
                else if (overloads.size() > 1)
                {
                    // Multiple overloads - print each overload
                    ss << " {\n";
                    for (auto *func : overloads)
                    {
                        print_tree(func, indent + 1);
                    }
                    ss << indent_str << "}";
                }
            }
            // Check if this symbol is a container with members
            else if (auto container = sym->as<ContainerSymbol>())
            {
                if (!container->members.empty())
                {
                    ss << " {\n";

                    // Print members in insertion order
                    for (auto &[name, member_ptr] : container->members)
                    {
                        print_tree(member_ptr.get(), indent + 1);
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