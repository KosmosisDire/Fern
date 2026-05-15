#pragma once

#include <unordered_map>
#include <vector>

#include <flir/flir.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

class AllocArena;
class Diagnostics;
struct SemanticContext;

struct FlirContext
{
    FlirContext(AllocArena& arena, Diagnostics& diag)
        : arena(arena)
        , diag(diag)
    {
    }

    FlirContext(const FlirContext&) = delete;
    FlirContext& operator=(const FlirContext&) = delete;

    AllocArena& arena;
    Diagnostics& diag;

    std::vector<FlirMethod*> methods;
    std::unordered_map<MethodSymbol*, FlirMethod*> loweredMethods;
    std::unordered_map<Symbol*, FlirLocal*> slots;

    FlirLocal* lookup_local_symbol(LocalSymbol* symbol)
    {
        auto it = slots.find(symbol);
        return it != slots.end() ? it->second : nullptr;
    }

    FlirLocal* lookup_param_symbol(ParameterSymbol* symbol)
    {
        auto it = slots.find(symbol);
        return it != slots.end() ? it->second : nullptr;
    }

    FlirMethod* lower_single_method(SemanticContext& semantic, MethodSymbol* method);
};

}
