#pragma once

#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace Fern
{


struct RootSyntax;

enum class DeclarationKind
{
    Global,
    Type,
    Namespace,
};

struct Declaration
{
    std::string name;
    DeclarationKind kind;
    std::vector<Declaration> children;

    const Declaration* find_child(std::string_view childName) const
    {
        for (const auto& child : children)
        {
            if (child.name == childName)
            {
                return &child;
            }
        }
        return nullptr;
    }

    std::string format(int indent = 0) const
    {
        std::string result;
        std::string prefix(indent * 2, ' ');

        int childIndent = indent;
        bool hasChildren = !children.empty();

        switch (kind)
        {
            case DeclarationKind::Global:
                break;
            case DeclarationKind::Type:
                result += prefix + "type " + name + "\n";
                childIndent = indent + 1;
                break;
            case DeclarationKind::Namespace:
                result += prefix + "namespace " + name + "\n";
                childIndent = indent + 1;
                break;
        }

        if (hasChildren && kind != DeclarationKind::Global)
        {
            result += prefix + "{\n";
        }

        for (const auto& child : children)
        {
            result += child.format(childIndent);
        }

        if (hasChildren && kind != DeclarationKind::Global)
        {
            result += prefix + "}\n";
        }

        return result;
    }
};

struct DeclarationTable
{
    std::vector<Declaration> roots;
    Declaration merged;

    DeclarationTable()
    {
        merged.kind = DeclarationKind::Global;
    }

    void add(RootSyntax* ast);

    std::string format() const
    {
        return merged.format();
    }

    const Declaration* find(std::span<const std::string_view> path) const
    {
        if (path.empty())
        {
            return nullptr;
        }

        const Declaration* current = &merged;
        for (const auto& segment : path)
        {
            current = current->find_child(segment);
            if (!current)
            {
                return nullptr;
            }
        }
        return current;
    }

    std::vector<const Declaration*> find_all(std::string_view name) const
    {
        std::vector<const Declaration*> results;
        find_all_recursive(name, &merged, results);
        return results;
    }

private:
    void merge_into(std::vector<Declaration>& target, const Declaration& source);

    void find_all_recursive(
        std::string_view name,
        const Declaration* current,
        std::vector<const Declaration*>& results) const
    {
        for (const auto& child : current->children)
        {
            if (child.name == name)
            {
                results.push_back(&child);
            }
            find_all_recursive(name, &child, results);
        }
    }
};


}