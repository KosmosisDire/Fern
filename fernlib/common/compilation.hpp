#pragma once

#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include <arena.hpp>
#include <common/diagnostic.hpp>
#include <flir/context.hpp>
#include <semantic/context.hpp>
#include <source/file.hpp>
#include <token/token.hpp>

namespace Fern
{

struct RootSyntax;

struct CompilationUnit
{
    std::unique_ptr<SourceFile> sourceFile;
    std::vector<Token> tokens;
    RootSyntax* ast = nullptr;
};

class Compilation
{
public:
    Compilation()
        : semanticContext(arena, diag)
        , flirContext(arena, diag)
    {
    }

    void add_file(std::string_view path);
    void add_source(std::string source, std::string_view path);
    void compile();

    SemanticContext& semantic() { return semanticContext; }
    const SemanticContext& semantic() const { return semanticContext; }

    FlirContext& flir() { return flirContext; }
    const FlirContext& flir() const { return flirContext; }

    const auto& get_units() const { return units; }

    Diagnostics diag;

private:
    AllocArena arena;
    SemanticContext semanticContext;
    FlirContext flirContext;
    std::vector<std::unique_ptr<CompilationUnit>> units;
    bool compiled = false;
};

}
