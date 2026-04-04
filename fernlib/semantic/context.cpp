#include <semantic/context.hpp>
#include <unordered_map>

namespace Fern
{

static const std::unordered_map<std::string_view, std::pair<std::string_view, std::string_view>> typeAliases =
{
    {"i32",  {"Core", "I32"}},
    {"f32",  {"Core", "F32"}},
    {"bool", {"Core", "Bool"}},
};

TypeSymbol* SemanticContext::resolve_type_name(std::string_view alias)
{
    auto it = typeAliases.find(alias);
    if (it != typeAliases.end())
    {
        Symbol* sym = symbols.lookup({it->second.first, it->second.second});
        if (sym) return sym->as<TypeSymbol>();
    }
    return nullptr;
}

TypeSymbol* SemanticContext::resolve_type_name(Token name)
{
    TypeSymbol* aliased = resolve_type_name(name.lexeme);
    if (aliased) return aliased;

    Symbol* sym = symbols.lookup({name.lexeme});
    if (sym) return sym->as<TypeSymbol>();
    return nullptr;
}

std::string SemanticContext::format() const
{
    return symbols.format();
}

}
