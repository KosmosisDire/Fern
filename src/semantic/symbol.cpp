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
        symbol->parent = this;
        Symbol* ptr = symbol.get();
        member_order.push_back(ptr);
        members.emplace(symbol->name, std::move(symbol));
        return ptr;
    }
    
    std::vector<Symbol*> ContainerSymbol::get_member(const std::string& name) {
        std::vector<Symbol*> results;
        auto range = members.equal_range(name);
        for (auto it = range.first; it != range.second; ++it) {
            results.push_back(it->second.get());
        }
        return results;
    }
    
    std::vector<FunctionSymbol*> ContainerSymbol::get_functions(const std::string& name) {
        std::vector<FunctionSymbol*> results;
        auto range = members.equal_range(name);
        for (auto it = range.first; it != range.second; ++it) {
            if (auto func = it->second->as<FunctionSymbol>()) {
                results.push_back(func);
            }
        }
        return results;
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
    
    std::string FunctionSymbol::get_mangled_name() const {
        std::string mangled = get_qualified_name();
        for (auto param : parameters) {
            mangled += "_";
            if (param->type) {
                mangled += param->type->get_name();
            }
        }
        return mangled;
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
