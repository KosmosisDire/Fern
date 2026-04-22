#include "method_binder.hpp"

#include <semantic/symbol/symbol.hpp>

namespace Fern
{

Symbol* MethodBinder::lookup_in_single_binder(std::string_view name)
{
    if (!method) return nullptr;

    for (auto* param : method->parameters)
    {
        if (param->name == name) return param;
    }

    return nullptr;
}

}
