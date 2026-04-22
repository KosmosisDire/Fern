#pragma once

#include <vector>

#include "binder.hpp"
#include "scope.hpp"

namespace Fern
{

struct FhirStmt;

// Scope contributor for a `{ ... }` block. Owns locals declared within its
// braces and the pending-statements list used by initializer lowering to
// inject setup statements ahead of the expression that produced them.
class BlockBinder : public Binder
{
public:
    explicit BlockBinder(Binder& parent)
        : Binder(parent)
    {
    }

    void set_pending_statements(std::vector<FhirStmt*>* stmts) { pendingStmts = stmts; }

protected:
    Symbol* lookup_in_single_binder(std::string_view name) override;
    std::vector<FhirStmt*>* pending_statements() override { return pendingStmts; }
    Scope* current_block_scope() override { return &blockScope; }

private:
    Scope blockScope;
    std::vector<FhirStmt*>* pendingStmts = nullptr;
};

}
