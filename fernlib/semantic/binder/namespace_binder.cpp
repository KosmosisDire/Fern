#include "namespace_binder.hpp"

#include <semantic/symbol/symbol.hpp>

namespace Fern
{

Symbol* NamespaceBinder::lookup_in_single_binder(std::string_view name)
{
    if (!namespaceSymbol) return nullptr;
    return namespaceSymbol->find_member(name);
}

}
