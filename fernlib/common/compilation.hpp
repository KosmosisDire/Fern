#pragma once

#include <string>
#include <string_view>
#include <vector>

#include <arena.hpp>
#include <common/diagnostic.hpp>
#include <source/file.hpp>
#include <token/token.hpp>
#include <semantic/context.hpp>

namespace Fern
{

struct RootSyntax;
class Binder;

struct CompilationUnit
{
    AllocArena arena;
    std::unique_ptr<SourceFile> sourceFile;
    std::vector<Token> tokens;
    RootSyntax* ast = nullptr;
};

class Compilation : public DiagnosticSystem
{
public:
    Compilation()
        : DiagnosticSystem("Compilation")
    {
    }

    void add_file(std::string_view path);
    void add_source(std::string source, std::string_view path);
    void compile();

    SemanticContext & semantic() { return semanticContext; }
    const SemanticContext & semantic() const { return semanticContext; }

    const auto& get_units() const
    {
        return units;
    }

private:
    std::vector<std::unique_ptr<CompilationUnit>> units;
    SemanticContext  semanticContext;
    bool compiled = false;
};

}
