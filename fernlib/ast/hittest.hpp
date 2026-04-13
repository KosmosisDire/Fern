#pragma once

#include "ast.hpp"

namespace Fern
{

class HitTestVisitor : public DefaultAstVisitor
{
    uint32_t line;
    uint32_t col;
    BaseSyntax* best = nullptr;

    void on_visit(BaseSyntax* node) override
    {
        if (node->span.contains(line, col))
        {
            best = node;
        }
    }

public:
    static BaseSyntax* find(BaseSyntax* root, uint32_t line, uint32_t col)
    {
        if (!root) return nullptr;
        HitTestVisitor visitor;
        visitor.line = line;
        visitor.col = col;
        root->accept(&visitor);
        return visitor.best;
    }
};

}
