#pragma once

#include <unordered_map>

namespace Fern
{

struct BaseExprSyntax;
struct BaseDeclSyntax;
struct TypeSymbol;
struct Symbol;

struct AstBinding
{
    std::unordered_map<BaseExprSyntax*, TypeSymbol*> types;
    std::unordered_map<BaseExprSyntax*, Symbol*> symbols;
    std::unordered_map<BaseDeclSyntax*, Symbol*> declarations;

    TypeSymbol* get_type(BaseExprSyntax* expr) const
    {
        auto it = types.find(expr);
        return it != types.end() ? it->second : nullptr;
    }

    Symbol* get_symbol(BaseExprSyntax* expr) const
    {
        auto it = symbols.find(expr);
        return it != symbols.end() ? it->second : nullptr;
    }

    Symbol* get_decl(BaseDeclSyntax* decl) const
    {
        auto it = declarations.find(decl);
        return it != declarations.end() ? it->second : nullptr;
    }

    void set_type(BaseExprSyntax* expr, TypeSymbol* type)
    {
        types[expr] = type;
    }

    void set_symbol(BaseExprSyntax* expr, Symbol* symbol)
    {
        symbols[expr] = symbol;
    }

    void set_decl(BaseDeclSyntax* decl, Symbol* symbol)
    {
        declarations[decl] = symbol;
    }
};

}
