#include "type_binder.hpp"

#include <semantic/symbol/symbol.hpp>

namespace Fern
{

// TODO: I think this method kinda breaks down with multiple overloads we should clean that up
Symbol* TypeBinder::lookup_in_single_binder(std::string_view name)
{
    if (!typeSymbol) return nullptr;

    if (auto* field = typeSymbol->find_field(name)) return field;
    if (auto* method = typeSymbol->find_method(name)) return method;
    if (auto* nested = typeSymbol->find_nested_type(name)) return nested;

    for (auto* param : typeSymbol->typeParamSymbols)
    {
        if (param->name == name) return param;
    }

    auto it = typeParamSubs.find(std::string(name));
    if (it != typeParamSubs.end()) return it->second;

    return nullptr;
}

}
