#pragma once

#include "binding/bound_tree.hpp"
#include "common/error.hpp"

namespace Fern
{

class SemanticValidator : public DiagnosticSystem
{
public:
    SemanticValidator() : DiagnosticSystem("SemanticValidator") {}

    // Validate the bound tree after type resolution
    void validate(BoundCompilationUnit* unit);

private:
    void validate_declaration(BoundDeclaration* decl);
    void validate_statement(BoundStatement* stmt);
    void validate_expression(BoundExpression* expr);
};

} // namespace Fern
