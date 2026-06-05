#pragma once

#include <semantic/binder/core/binder.hpp>

namespace Fern
{

// Chain terminator. Contributes primitive type aliases and global types via the
// semantic context. Has no state of its own every other binder's chain walks
// up to here.
class RootBinder : public Binder
{
public:
    RootBinder(SemanticContext& context)
        : Binder(context)
    {
    }

protected:
    LookupResult lookup_in_single_binder(std::string_view name) override;
};

}
