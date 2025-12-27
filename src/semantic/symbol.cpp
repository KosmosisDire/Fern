#include "symbol.hpp"
#include "type.hpp"

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
    Symbol* ContainerSymbol::add_member(std::unique_ptr<Symbol> symbol)
    {
        symbol->parent = this;
        Symbol* ptr = symbol.get();
        members[symbol->name].emplace_back(std::move(symbol));

        return ptr;
    }

    Symbol* ContainerSymbol::get_member(const std::string& name) {
        auto iter = members.find(name);
        if (iter != members.end()) {
            return iter->second[0].get();
        }
        return nullptr;
    }

    std::vector<Symbol*> ContainerSymbol::get_members(const std::string& name)
    {
        std::vector<Symbol*> result;
        auto iter = members.find(name);
        if (iter != members.end()) {
            for (const auto& sym_ptr : iter->second) {
                result.push_back(sym_ptr.get());
            }
        }
        return result;
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
        if (!Type::equals(return_type, other->return_type)) return false;

        for (size_t i = 0; i < parameters.size(); i++) {
            if (!Type::equals(parameters[i]->type, other->parameters[i]->type)) {
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
