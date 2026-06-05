#include "namespace_binder.hpp"

#include <semantic/symbol/symbol.hpp>

namespace Fern
{

LookupResult NamespaceBinder::lookup_in_single_binder(std::string_view name)
{
    LookupResult result;
    if (!namespaceSymbol) return result;
    if (Symbol* member = namespaceSymbol->find_member(name)) result.symbols.push_back(member);
    return result;
}

}
