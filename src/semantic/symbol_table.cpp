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

    FunctionSymbol* SymbolTable::resolve_function(const std::string &name, std::vector<TypePtr> arg_types)
    {
        // Look up in current scope
        auto container = get_current_container();
        if (!container)
            return nullptr;

        auto candidates = container->get_members(name);
        for (auto candidate : candidates)
        {
            if (auto func_sym = candidate->as<FunctionSymbol>())
            {
                // Check if parameter types match
                if (func_sym->parameters.size() != arg_types.size())
                    continue;

                bool match = true;
                for (size_t i = 0; i < arg_types.size(); ++i)
                {
                    if (!Type::equals(func_sym->parameters[i]->type, arg_types[i]))
                    {
                        match = false;
                        break;
                    }
                }

                if (match)
                {
                    return func_sym;
                }
            }
        }

        return nullptr;
    }

    std::vector<Symbol*> SymbolTable::resolve(const std::string &name)
    {
        // Walk up scopes looking for name
        Symbol *scope = current_scope;
        while (scope)
        {
            if (auto container = scope->as<ContainerSymbol>())
            {
                auto members = container->get_members(name);
                if (!members.empty())
                {
                    return members;
                }
            }

            scope = scope->parent;
        }

        return {};
    }

    Symbol* SymbolTable::resolve_single(const std::string &name)
    {
        auto symbols = resolve(name);
        if (!symbols.empty())
        {
            return symbols[0];
        }
        return nullptr;
    }

    Symbol* SymbolTable::resolve_single(const std::vector<std::string> &parts)
    {
        auto symbols = resolve(parts);
        if (!symbols.empty())
        {
            return symbols[0];
        }
        return nullptr;
    }

    std::vector<Symbol*> SymbolTable::resolve(const std::vector<std::string> &parts)
    {
        // convert to name, then call resolve
        if (parts.empty())
            return {};
        std::string name = parts[0];
        for (size_t i = 1; i < parts.size(); ++i)
        {
            name += "." + parts[i];
        }
        return resolve(name);
    }

    FunctionSymbol* SymbolTable::resolve_function_local(const std::string &name, std::vector<TypePtr> arg_types)
    {
        // Look up in current scope only
        auto container = get_current_container();
        if (!container)
            return nullptr;

        auto candidates = container->get_members(name);
        for (auto candidate : candidates)
        {
            if (auto func_sym = candidate->as<FunctionSymbol>())
            {
                // Check if parameter types match
                if (func_sym->parameters.size() != arg_types.size())
                    continue;

                bool match = true;
                for (size_t i = 0; i < arg_types.size(); ++i)
                {
                    if (!Type::equals(func_sym->parameters[i]->type, arg_types[i]))
                    {
                        match = false;
                        break;
                    }
                }

                if (match)
                {
                    return func_sym;
                }
            }
        }

        return nullptr;
    }

    Symbol* SymbolTable::resolve_single_local(const std::string &name)
    {
        auto symbols = resolve_local(name);
        if (!symbols.empty())
        {
            return symbols[0];
        }
        return nullptr;
    }

    Symbol* SymbolTable::resolve_single_local(const std::vector<std::string> &parts)
    {
        auto symbols = resolve_local(parts);
        if (!symbols.empty())
        {
            return symbols[0];
        }
        return nullptr;
    }

    std::vector<Symbol*> SymbolTable::resolve_local(const std::string &name)
    {
        if (auto container = current_scope->as<ContainerSymbol>())
        {
            return container->get_members(name);
        }
        return {};
    }

    std::vector<Symbol*> SymbolTable::resolve_local(const std::vector<std::string> &parts)
    {
        if (parts.empty())
            return {};
        // just look up the last part in the current scope
        return resolve_local(parts.back());
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
        return {};
    }
 
    void SymbolTable::merge(SymbolTable &other)
    {
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
            return;
        }

        // If they don't have a global namespace, nothing to merge
        if (!other.global_namespace)
        {
            return;
        }

        // Both have global namespaces - merge them
        merge_namespace(global_namespace.get(), other.global_namespace.get());

        // Clear other's state
        other.current_scope = nullptr;
    }

    void SymbolTable::merge_namespace(NamespaceSymbol *target, NamespaceSymbol *source)
    {
        if (!target || !source)
            return;

        auto target_container = static_cast<ContainerSymbol *>(target);
        auto source_container = static_cast<ContainerSymbol *>(source);

        for (auto it = source_container->members.begin(); it != source_container->members.end(); ++it)
        {
            const auto& name = it.key();
            auto& bucket = it.value();

            for (auto& member_ptr : bucket)
            {
                auto existing_symbol = target_container->get_member(name);

                if (existing_symbol)
                {
                    // Both are namespaces - recursively merge
                    auto source_ns = member_ptr->as<NamespaceSymbol>();
                    auto target_ns = existing_symbol->as<NamespaceSymbol>();
                    if (source_ns && target_ns)
                    {
                        merge_namespace(target_ns, source_ns);
                        continue;
                    }
                }
                
                // Move the symbol to target
                target_container->add_member(std::move(member_ptr));
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

            // Check if this symbol is a container with members
            if (auto container = sym->as<ContainerSymbol>())
            {
                if (!container->members.empty())
                {
                    ss << "\n" << indent_str << "{\n";

                    // Print members in insertion order
                    for (auto [name, member_ptr] : *container)
                    {
                        print_tree(member_ptr, indent + 1);
                    }

                    ss << indent_str << "}";
                }

                ss << "\n";
            }

            ss << "\n";
        };

        // Print the global namespace tree
        print_tree(global_namespace.get(), 0);

        return ss.str();
    }

} // namespace Fern