#include "semantic_validator.hpp"

namespace Fern
{

void SemanticValidator::validate(BoundCompilationUnit* unit)
{
    if (!unit) return;

    for (auto* stmt : unit->statements)
    {
        if (stmt)
            stmt->accept(this);
    }
}

void SemanticValidator::visit(BoundTypeExpression* node)
{
    if (!node) return;

    if (node->resolvedTypeReference && node->resolvedTypeReference->is<UnresolvedType>())
    {
        std::string typeName;
        for (size_t i = 0; i < node->parts.size(); ++i)
        {
            if (i > 0) typeName += ".";
            typeName += node->parts[i];
        }

        if (!typeName.empty())
        {
            error("'" + typeName + "' is not a known type", node->location);
        }
        else
        {
            error("Unknown type", node->location);
        }
    }

    DefaultBoundVisitor::visit(node);
}

} // namespace Fern
