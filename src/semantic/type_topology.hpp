#pragma once

#include <vector>
#include <unordered_map>
#include <unordered_set>
#include "symbol.hpp"
#include "common/error.hpp"

namespace Fern
{

// Builds a dependency graph of value types and provides topological ordering.
// Detects cycles in value type references (which are invalid - infinite size).
class TypeTopology : public DiagnosticSystem {
public:
    TypeTopology() : DiagnosticSystem("TypeTopology") {}

    // Build the topology from the global namespace
    void build(NamespaceSymbol* global_ns);

    // Get types in dependency order (dependencies come before dependents)
    // Safe to use for computing type sizes/layouts
    const std::vector<TypeSymbol*>& get_sorted_types() const { return sorted_types; }

    // Check if cycles were detected
    bool has_cycles() const { return cycles_detected; }

    // Get direct value-type dependencies for a type
    const std::vector<TypeSymbol*>& get_dependencies(TypeSymbol* type) const;

    // Dump the dependency graph in Graphviz DOT format
    std::string dump_dot() const;

private:
    std::vector<TypeSymbol*> all_types;
    std::vector<TypeSymbol*> sorted_types;
    std::unordered_map<TypeSymbol*, std::vector<TypeSymbol*>> dependencies;
    bool cycles_detected = false;

    // Empty vector for types with no dependencies
    static const std::vector<TypeSymbol*> empty_deps;

    void collect_types(Symbol* symbol);
    void build_dependencies();
    void topological_sort();

    // Get the TypeSymbol a type directly depends on (value types only)
    TypeSymbol* get_value_dependency(TypePtr type);
};

} // namespace Fern
