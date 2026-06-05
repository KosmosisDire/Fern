#include "method_binder.hpp"

#include <semantic/symbol/symbol.hpp>

namespace Fern
{

LookupResult MethodBinder::lookup_in_single_binder(std::string_view name)
{
    LookupResult result;
    if (!method) return result;

    for (auto* param : method->parameters)
    {
        if (param->name == name)
        {
            result.symbols.push_back(param);
            return result;
        }
    }

    return result;
}

}
