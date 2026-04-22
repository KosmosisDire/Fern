#include "type_binder.hpp"

#include <semantic/symbol/symbol.hpp>

namespace Fern
{

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

    return nullptr;
}

}
