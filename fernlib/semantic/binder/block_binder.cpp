#include "block_binder.hpp"

namespace Fern
{

LookupResult BlockBinder::lookup_in_single_binder(std::string_view name)
{
    LookupResult result;
    if (Symbol* local = blockScope.find(name)) result.symbols.push_back(local);
    return result;
}

}
