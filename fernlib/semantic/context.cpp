#include <semantic/context.hpp>

namespace Fern
{

TypeSymbol* SemanticContext::resolve_type_name(TokenKind kind)
{
    switch (kind)
    {
        case TokenKind::F32Keyword:
        {
            Symbol* sym = symbols.lookup({"Core", "F32"});
            if (sym) return sym->as<TypeSymbol>();
            return nullptr;
        }
        default:
            return nullptr;
    }
}

TypeSymbol* SemanticContext::resolve_type_name(Token name)
{
    TypeSymbol* builtin = resolve_type_name(name.kind);
    if (builtin) return builtin;

    Symbol* sym = symbols.lookup({name.lexeme});
    if (sym) return sym->as<TypeSymbol>();
    return nullptr;
}

std::string SemanticContext::format() const
{
    return symbols.format();
}

}
