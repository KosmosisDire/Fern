#pragma once

#include "binder.hpp"
#include "scope.hpp"

namespace Fern
{

// Scope contributor for a `{ ... }` block. Owns locals declared within its braces.
class BlockBinder : public Binder
{
public:
    explicit BlockBinder(Binder& parent)
        : Binder(parent)
    {
    }

protected:
    LookupResult lookup_in_single_binder(std::string_view name) override;
    Scope* current_block_scope() override { return &blockScope; }

private:
    Scope blockScope;
};

}
