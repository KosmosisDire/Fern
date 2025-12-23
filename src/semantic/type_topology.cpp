#include "type_topology.hpp"
#include "type.hpp"
#include <algorithm>
#include <sstream>

namespace Fern
{

const std::vector<TypeSymbol*> TypeTopology::empty_deps;

void TypeTopology::build(NamespaceSymbol* global_ns)
{
    clear_diagnostics();
    all_types.clear();
    sorted_types.clear();
    dependencies.clear();
    cycles_detected = false;

    // Phase 1: Collect all types
    collect_types(global_ns);

    // Phase 2: Build dependency graph
    build_dependencies();

    // Phase 3: Topological sort (detects cycles)
    topological_sort();
}

void TypeTopology::collect_types(Symbol* symbol)
{
    if (auto* type_sym = symbol->as<TypeSymbol>()) {
        all_types.push_back(type_sym);
    }
    if (auto* container = symbol->as<ContainerSymbol>()) {
        for (auto& [name, child] : container->members) {
            collect_types(child.get());
        }
    }
}

TypeSymbol* TypeTopology::get_value_dependency(TypePtr type)
{
    if (!type) return nullptr;

    // For named types (structs), check if it's a value type
    if (auto named = type->as<NamedType>()) {
        TypeSymbol* sym = named->symbol;
        if (sym && !sym->is_ref()) {
            // Value type - we depend on its size
            return sym;
        }
    }

    // Arrays of value types also need the element type's size
    if (auto arr = type->as<ArrayType>()) {
        return get_value_dependency(arr->element);
    }

    // Pointers don't need the pointee's size (always 8 bytes)
    return nullptr;
}

void TypeTopology::build_dependencies()
{
    std::unordered_set<TypeSymbol*> type_set(all_types.begin(), all_types.end());

    for (auto* type_sym : all_types) {
        auto& deps = dependencies[type_sym];

        for (auto& [name, member_ptr] : type_sym->members) {
            if (auto* var = member_ptr->as<VariableSymbol>()) {
                if (!var->is_field()) continue;

                TypeSymbol* dep = get_value_dependency(var->type);
                if (dep && type_set.count(dep)) {
                    deps.push_back(dep);
                }
            }
        }
    }
}

void TypeTopology::topological_sort()
{
    std::unordered_set<TypeSymbol*> type_set(all_types.begin(), all_types.end());
    std::unordered_set<TypeSymbol*> visited;              // Successfully processed types
    std::unordered_set<VariableSymbol*> reported_fields;  // Fields already reported as cycle causes
    std::vector<TypeSymbol*> stack;                       // Current traversal path

    std::function<bool(TypeSymbol*, VariableSymbol*)> visit =
        [&](TypeSymbol* type_sym, VariableSymbol* from_field) -> bool {

        if (visited.count(type_sym)) return true;

        // Check if we're already visiting this type (cycle detected)
        auto it = std::find(stack.begin(), stack.end(), type_sym);
        if (it != stack.end()) {
            // Only report if this field hasn't been reported yet
            if (from_field && !reported_fields.count(from_field)) {
                std::string cycle_msg = "Cycle in value types: ";
                for (auto cycle_it = it; cycle_it != stack.end(); ++cycle_it) {
                    cycle_msg += (*cycle_it)->name + " -> ";
                }
                cycle_msg += type_sym->name;

                error(cycle_msg, from_field->location);
                reported_fields.insert(from_field);
                cycles_detected = true;
            }
            return false;
        }

        stack.push_back(type_sym);

        bool all_deps_ok = true;
        // Visit ALL dependencies (don't short-circuit) to find all cycle-causing fields
        for (auto& [name, member_ptr] : type_sym->members) {
            if (auto* var = member_ptr->as<VariableSymbol>()) {
                if (!var->is_field()) continue;

                TypeSymbol* dep = get_value_dependency(var->type);
                if (dep && type_set.count(dep)) {
                    if (!visit(dep, var)) {
                        all_deps_ok = false;
                        // Don't break - continue to find other cycle-causing fields
                    }
                }
            }
        }

        stack.pop_back();

        if (all_deps_ok) {
            visited.insert(type_sym);
            sorted_types.push_back(type_sym);
        }
        return all_deps_ok;
    };

    for (auto* type_sym : all_types) {
        visit(type_sym, nullptr);
    }
}

const std::vector<TypeSymbol*>& TypeTopology::get_dependencies(TypeSymbol* type) const
{
    auto it = dependencies.find(type);
    if (it != dependencies.end()) {
        return it->second;
    }
    return empty_deps;
}

std::string TypeTopology::dump_dot() const
{
    std::ostringstream ss;
    ss << "digraph TypeTopology {\n";
    ss << "    rankdir=BT;\n";  // Bottom to top (dependencies point up)
    ss << "    node [shape=box, style=filled, fillcolor=lightblue];\n";
    ss << "\n";

    // Define all nodes
    for (auto* type_sym : all_types) {
        ss << "    \"" << type_sym->name << "\";\n";
    }
    ss << "\n";

    // Define edges (type -> dependency)
    for (auto* type_sym : all_types) {
        auto it = dependencies.find(type_sym);
        if (it != dependencies.end()) {
            for (auto* dep : it->second) {
                ss << "    \"" << type_sym->name << "\" -> \"" << dep->name << "\";\n";
            }
        }
    }

    ss << "}\n";
    return ss.str();
}

} // namespace Fern
