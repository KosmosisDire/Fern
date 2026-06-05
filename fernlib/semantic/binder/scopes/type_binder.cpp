#include "type_binder.hpp"

#include <semantic/symbol/symbol.hpp>

namespace Fern
{

LookupResult TypeBinder::lookup_in_single_binder(std::string_view name)
{
    LookupResult result;
    if (!typeSymbol) return result;

    if (auto* field = typeSymbol->find_field(name))
    {
        result.symbols.push_back(field);
        return result;
    }

    auto methods = typeSymbol->collect_methods(name);
    if (!methods.empty())
    {
        result.symbols.reserve(methods.size());
        for (auto* m : methods) result.symbols.push_back(m);
        return result;
    }

    if (auto* nested = typeSymbol->find_nested_type(name))
    {
        result.symbols.push_back(nested);
        return result;
    }

    for (auto* param : typeSymbol->typeParamSymbols)
    {
        if (param->name == name)
        {
            result.symbols.push_back(param);
            return result;
        }
    }

    auto it = typeParamSubs.find(std::string(name));
    if (it != typeParamSubs.end())
    {
        result.symbols.push_back(it->second);
        return result;
    }

    return result;
}

}
