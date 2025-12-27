#include "symbol_table_validator.hpp"
#include <unordered_set>

namespace Fern
{

static const char* get_symbol_kind_name(Symbol* sym)
{
    if (sym->is<TypeSymbol>()) return "type";
    if (sym->is<FunctionSymbol>()) return "function";
    if (sym->is<VariableSymbol>()) return "variable";
    if (sym->is<PropertySymbol>()) return "property";
    if (sym->is<NamespaceSymbol>()) return "namespace";
    return "symbol";
}

#pragma region Untyped Validation

void SymbolTableValidator::validate_untyped(SymbolTable& symbols)
{
    validate_container_untyped(symbols.get_global_namespace());
}

void SymbolTableValidator::validate_container_untyped(ContainerSymbol* container)
{
    if (!container) return;

    for (auto& [name, symbol_vec] : container->members)
    {
        if (symbol_vec.size() < 2) continue;

        Symbol* first = symbol_vec[0].get();
        const char* first_kind = get_symbol_kind_name(first);
        bool has_conflict = false;

        for (size_t i = 1; i < symbol_vec.size(); ++i)
        {
            Symbol* sym = symbol_vec[i].get();
            bool same_kind = (first->kind == sym->kind);

            if (same_kind)
            {
                // Duplicate of same kind (except functions which can overload)
                if (!sym->is<FunctionSymbol>())
                {
                    has_conflict = true;
                    error("Duplicate " + std::string(first_kind) + " '" + name + "'", sym->location);
                }
            }
            else
            {
                // Different kinds with same name
                // Functions can't conflict with other functions (overloading), but can with other kinds
                if (first->is<FunctionSymbol>() && sym->is<FunctionSymbol>())
                    continue;

                has_conflict = true;
                error("Name conflict: '" + name + "' is already defined as a " + first_kind, sym->location);
            }
        }

        if (has_conflict)
            warn("'" + name + "' first defined here", first->location);
    }

    // Recursively validate nested containers
    for (auto& [name, symbol_vec] : container->members)
    {
        for (auto& sym_ptr : symbol_vec)
        {
            if (auto* type_sym = sym_ptr->as<TypeSymbol>())
            {
                validate_type_member_names(type_sym);
                validate_container_untyped(type_sym);
            }
            else if (auto* nested = sym_ptr->as<ContainerSymbol>())
            {
                validate_container_untyped(nested);
            }
        }
    }
}

void SymbolTableValidator::validate_type_member_names(TypeSymbol* type_sym)
{
    if (!type_sym) return;

    const std::string& type_name = type_sym->name;

    auto it = type_sym->members.find(type_name);
    if (it != type_sym->members.end())
    {
        for (auto& sym_ptr : it->second)
        {
            error("Member '" + type_name + "' cannot have the same name as its containing type",
                  sym_ptr->location);
        }
    }
}

#pragma endregion

#pragma region Typed Validation

static bool parameters_match(FunctionSymbol* a, FunctionSymbol* b)
{
    if (a->parameters.size() != b->parameters.size())
        return false;

    for (size_t i = 0; i < a->parameters.size(); ++i)
    {
        if (!Type::equals(a->parameters[i]->type, b->parameters[i]->type))
            return false;
    }
    return true;
}

void SymbolTableValidator::validate_typed(SymbolTable& symbols)
{
    validate_container_typed(symbols.get_global_namespace());
}

void SymbolTableValidator::validate_container_typed(ContainerSymbol* container)
{
    if (!container) return;

    for (auto& [name, symbol_vec] : container->members)
    {
        std::vector<FunctionSymbol*> functions;
        for (auto& sym_ptr : symbol_vec)
        {
            if (auto* f = sym_ptr->as<FunctionSymbol>())
            {
                functions.push_back(f);
            }
        }

        // Duplicate function signatures - track which have already been reported as duplicates
        std::unordered_set<size_t> is_duplicate;
        for (size_t i = 0; i < functions.size(); ++i)
        {
            if (is_duplicate.count(i)) continue;

            bool found_match = false;
            for (size_t j = i + 1; j < functions.size(); ++j)
            {
                if (is_duplicate.count(j)) continue;

                if (parameters_match(functions[i], functions[j]))
                {
                    found_match = true;
                    is_duplicate.insert(j);
                    std::string sig = functions[j]->get_signature();
                    error("Duplicate function signature '" + sig + "'", functions[j]->location);
                }
            }

            if (found_match)
            {
                std::string sig = functions[i]->get_signature();
                warn("'" + sig + "' first defined here", functions[i]->location);
            }
        }
    }

    for (auto& [name, symbol_vec] : container->members)
    {
        for (auto& sym_ptr : symbol_vec)
        {
            if (auto* nested = sym_ptr->as<ContainerSymbol>())
            {
                validate_container_typed(nested);
            }
        }
    }
}

#pragma endregion

} // namespace Fern
