#include "compilation.hpp"

#include <fstream>
#include <functional>
#include <sstream>
#include <stdexcept>

#include <lexer/lexer.hpp>
#include <parser/parser.hpp>
#include <token/walker.hpp>
#include <binder/binder.hpp>

namespace Fern
{

void Compilation::add_file(std::string_view path)
{
    if (compiled)
    {
        throw std::runtime_error("Cannot add files after compile()");
    }

    std::ifstream file{std::string{path}};
    if (!file)
    {
        throw std::runtime_error("Could not open file: " + std::string(path));
    }

    std::ostringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();

    add_source(std::move(source), path);
}

void Compilation::add_source(std::string source, std::string_view path)
{
    if (compiled)
    {
        throw std::runtime_error("Cannot add sources after compile()");
    }

    int fileId = static_cast<int>(units.size());
    auto unit = std::make_unique<CompilationUnit>();
    unit->sourceFile = std::make_unique<SourceFile>(std::move(source), std::string(path), fileId);
    units.push_back(std::move(unit));
}

void Compilation::compile()
{
    if (compiled)
    {
        throw std::runtime_error("compile() can only be called once");
    }

    // Phase 1: Parse all files
    for (auto& unit : units)
    {
        Lexer lexer(*unit->sourceFile);
        unit->tokens = lexer.tokenize();

        TokenWalker walker(unit->tokens);
        Parser parser(walker, unit->arena);
        unit->ast = parser.parse();
    }

    // Phase 2: Bind symbols and method bodies
    Binder binder(semanticContext);
    for (auto& unit : units)
    {
        binder.bind_ast(unit->ast);
    }
    binder.resolve_all_types();
    binder.bind_all_methods();

    compiled = true;
}

}
