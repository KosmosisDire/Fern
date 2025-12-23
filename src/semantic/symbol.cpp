#include "symbol.hpp"

namespace Fern
{
    // Symbol implementation
    std::string Symbol::get_qualified_name() const {
        if (!parent || parent->kind == SymbolKind::Namespace && parent->name.empty()) {
            return name;
        }
        return parent->get_qualified_name() + "." + name;
    }
    
    // ContainerSymbol implementation
    Symbol* ContainerSymbol::add_member(std::unique_ptr<Symbol> symbol) {
        // For functions, use add_function instead
        if (symbol->is<FunctionSymbol>()) {
            // This shouldn't happen - caller should use add_function
            // But handle it gracefully by delegating
            auto* func = static_cast<FunctionSymbol*>(symbol.release());
            return add_function(std::unique_ptr<FunctionSymbol>(func));
        }

        symbol->parent = this;
        Symbol* ptr = symbol.get();
        members[symbol->name] = std::move(symbol);
        return ptr;
    }

    FunctionSymbol* ContainerSymbol::add_function(std::unique_ptr<FunctionSymbol> func) {
        const std::string& name = func->name;

        // Check if a FunctionGroupSymbol already exists for this name
        auto it = members.find(name);
        if (it != members.end()) {
            if (auto* group = it->second->as<FunctionGroupSymbol>()) {
                // Add to existing group
                return group->add_overload(std::move(func));
            }
            // Name collision with non-function symbol - this is an error
            // For now, just return nullptr (caller should handle)
            return nullptr;
        }

        // Create new FunctionGroupSymbol
        auto group = std::make_unique<FunctionGroupSymbol>(name);
        group->parent = this;
        FunctionGroupSymbol* group_ptr = group.get();
        members[name] = std::move(group);

        // Add the function to the group
        return group_ptr->add_overload(std::move(func));
    }

    FunctionGroupSymbol* ContainerSymbol::get_function_group(const std::string& name) {
        auto it = members.find(name);
        if (it != members.end()) {
            return it->second->as<FunctionGroupSymbol>();
        }
        return nullptr;
    }

    Symbol* ContainerSymbol::get_member(const std::string& name) {
        auto it = members.find(name);
        if (it != members.end()) {
            return it->second.get();
        }
        return nullptr;
    }

    std::vector<FunctionSymbol*> ContainerSymbol::get_functions(const std::string& name) {
        if (auto* group = get_function_group(name)) {
            return group->get_overloads();
        }
        return {};
    }

    // NamespaceSymbol implementation
    NamespaceSymbol::NamespaceSymbol(const std::string& name) {
        kind = SymbolKind::Namespace;
        this->name = name;
        modifiers = ModifierKindFlags::Public;
    }

    // BlockSymbol implementation
    BlockSymbol::BlockSymbol(const std::string& debug_name) {
        kind = SymbolKind::Block;
        this->name = debug_name;
        modifiers = ModifierKindFlags::Private;  // Blocks are always private
    }

    // TypeSymbol implementation
    TypeSymbol::TypeSymbol(const std::string& name, TypePtr type) {
        kind = SymbolKind::Type;
        this->name = name;
        this->type = type;
    }
    
    // FunctionSymbol implementation
    FunctionSymbol::FunctionSymbol(const std::string& name, TypePtr return_type) {
        kind = SymbolKind::Function;
        this->name = name;
        this->return_type = return_type;
    }
    
    bool FunctionSymbol::signature_matches(FunctionSymbol* other) const {
        if (parameters.size() != other->parameters.size()) return false;
        if (return_type != other->return_type) return false;

        for (size_t i = 0; i < parameters.size(); i++) {
            if (parameters[i]->type != other->parameters[i]->type) {
                return false;
            }
        }
        return true;
    }

    std::string FunctionSymbol::get_signature() const {
        std::string sig = name + "(";
        for (size_t i = 0; i < parameters.size(); i++) {
            if (i > 0) sig += ", ";
            sig += parameters[i]->type ? parameters[i]->type->get_name() : "?";
        }
        sig += ") -> ";
        sig += return_type ? return_type->get_name() : "?";
        return sig;
    }

    // FunctionGroupSymbol implementation
    FunctionGroupSymbol::FunctionGroupSymbol(const std::string& name) {
        kind = SymbolKind::FunctionGroup;
        this->name = name;
    }

    FunctionSymbol* FunctionGroupSymbol::add_overload(std::unique_ptr<FunctionSymbol> func) {
        func->parent = this->parent;  // Function's parent is the container, not the group
        FunctionSymbol* ptr = func.get();
        overloads.push_back(std::move(func));
        return ptr;
    }

    std::vector<FunctionSymbol*> FunctionGroupSymbol::get_overloads() const {
        std::vector<FunctionSymbol*> result;
        result.reserve(overloads.size());
        for (const auto& func : overloads) {
            result.push_back(func.get());
        }
        return result;
    }

    FunctionSymbol* FunctionGroupSymbol::resolve(const std::vector<TypePtr>& arg_types) const {
        // Simple exact-match resolution for now
        // TODO: Add scoring for implicit conversions
        for (const auto& func : overloads) {
            if (func->parameters.size() != arg_types.size()) continue;

            bool matches = true;
            for (size_t i = 0; i < arg_types.size(); i++) {
                if (func->parameters[i]->type != arg_types[i]) {
                    matches = false;
                    break;
                }
            }

            if (matches) return func.get();
        }

        return nullptr;
    }

    // VariableSymbol implementation
    VariableSymbol::VariableSymbol(const std::string& name, TypePtr type) 
        : type(type) {
        this->name = name;
        kind = SymbolKind::Variable;
    }
    
    // ParameterSymbol implementation
    ParameterSymbol::ParameterSymbol(const std::string& name, TypePtr type, uint32_t idx)
        : VariableSymbol(name, type), index(idx) {}

    // PropertySymbol implementation
    PropertySymbol::PropertySymbol(const std::string& name, TypePtr type) {
        kind = SymbolKind::Property;
        this->name = name;
        this->type = type;
    }
    
    // EnumCaseSymbol implementation
    EnumCaseSymbol::EnumCaseSymbol(const std::string& name) {
        kind = SymbolKind::EnumCase;
        this->name = name;
    }

} // namespace Fern
