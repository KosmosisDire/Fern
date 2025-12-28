#pragma once

#include "binding/bound_tree.hpp"
#include "common/error.hpp"

namespace Fern
{

class SemanticValidator : public DiagnosticSystem, public DefaultBoundVisitor
{
public:
    SemanticValidator() : DiagnosticSystem("SemanticValidator") {}

    void validate(BoundCompilationUnit* unit);

private:
    void visit(BoundTypeExpression* node) override;
};

} // namespace Fern
