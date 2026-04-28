#include "root_binder.hpp"

#include <semantic/context.hpp>

namespace Fern
{

LookupResult RootBinder::lookup_in_single_binder(std::string_view name)
{
    LookupResult result;
    if (TypeSymbol* aliased = context.resolve_type_name(name)) result.symbols.push_back(aliased);
    return result;
}

}
