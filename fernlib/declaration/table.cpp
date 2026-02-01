#include <declaration/table.hpp>
#include <declaration/builder.hpp>

namespace Fern
{

void DeclarationTable::add(RootSyntax* ast)
{
    Declaration root = DeclarationBuilder::build(ast);

    for (const auto& child : root.children)
    {
        merge_into(merged.children, child);
    }

    roots.push_back(std::move(root));
}

void DeclarationTable::merge_into(std::vector<Declaration>& target, const Declaration& source)
{
    if (source.kind == DeclarationKind::Namespace)
    {
        for (auto& existing : target)
        {
            if (existing.kind == DeclarationKind::Namespace && existing.name == source.name)
            {
                for (const auto& child : source.children)
                {
                    merge_into(existing.children, child);
                }
                return;
            }
        }
    }

    target.push_back(source);
}

}
