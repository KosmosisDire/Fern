#include "semantic_validator.hpp"

namespace Fern
{

void SemanticValidator::validate(BoundCompilationUnit* unit)
{
    if (!unit) return;

    for (auto* decl : unit->statements)
    {
        validate_statement(decl);
    }
}

void SemanticValidator::validate_declaration(BoundDeclaration* decl)
{
    if (!decl) return;

    // Placeholder for future semantic checks on declarations
    // Examples: check return paths, unreachable code, etc.
}

void SemanticValidator::validate_statement(BoundStatement* stmt)
{
    if (!stmt) return;

    // Placeholder for future semantic checks on statements
}

void SemanticValidator::validate_expression(BoundExpression* expr)
{
    if (!expr) return;

    // Placeholder for future semantic checks on expressions
}

} // namespace Fern
