#include "root_binder.hpp"

#include <semantic/context.hpp>

namespace Fern
{

Symbol* RootBinder::lookup_in_single_binder(std::string_view name)
{
    if (TypeSymbol* aliased = context.resolve_type_name(name)) return aliased;
    return nullptr;
}

}
