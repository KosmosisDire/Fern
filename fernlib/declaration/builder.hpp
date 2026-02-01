#pragma once

#include <ast/ast.hpp>
#include <declaration/table.hpp>

namespace Fern
{

class DeclarationBuilder
{
public:
    static Declaration build(RootSyntax* root)
    {
        Declaration result;
        Visitor visitor(result);
        root->accept(&visitor);
        return result;
    }

private:
    class Visitor : public DefaultAstVisitor
    {
        Declaration& result;
        std::vector<Declaration*> stack;

    public:
        Visitor(Declaration& result) : result(result)
        {
            stack.push_back(&result);
        }

        void visit(RootSyntax* node) override
        {
            result.kind = DeclarationKind::Global;
            for (auto& decl : node->declarations)
            {
                if (decl) decl->accept(this);
            }
        }

        void visit(TypeDeclSyntax* node) override
        {
            Declaration decl;
            decl.name = std::string(node->name.lexeme);
            decl.kind = DeclarationKind::Type;

            stack.back()->children.push_back(std::move(decl));
            stack.push_back(&stack.back()->children.back());

            for (auto& child : node->declarations)
            {
                if (child) child->accept(this);
            }

            stack.pop_back();
        }

        void visit(NamespaceDeclSyntax* node) override
        {
            Declaration decl;
            decl.name = std::string(node->name.lexeme);
            decl.kind = DeclarationKind::Namespace;

            stack.back()->children.push_back(std::move(decl));
            stack.push_back(&stack.back()->children.back());

            for (auto& child : node->declarations)
            {
                if (child) child->accept(this);
            }

            stack.pop_back();
        }
    };
};

}