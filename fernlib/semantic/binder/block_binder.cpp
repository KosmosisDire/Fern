#include "block_binder.hpp"

namespace Fern
{

Symbol* BlockBinder::lookup_in_single_binder(std::string_view name)
{
    return blockScope.find(name);
}

}
