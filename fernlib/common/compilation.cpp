#include "compilation.hpp"

#include <fstream>
#include <sstream>
#include <stdexcept>

#include <ast/validate.hpp>
#include <binder/binder_pipeline.hpp>
#include <lexer/lexer.hpp>
#include <parser/parser.hpp>
#include <semantic/fhir/flow.hpp>
#include <token/walker.hpp>

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
        diag.error("Could not open file: " + std::string(path), Span{});
        return;
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

    for (auto& unit : units)
    {
        Lexer lexer(*unit->sourceFile, diag);
        unit->tokens = lexer.tokenize();

        TokenWalker walker(unit->tokens);
        Parser parser(walker, arena, diag);
        unit->ast = parser.parse();
    }

    for (auto& unit : units)
    {
        AstValidator validator(diag);
        validator.validate(unit->ast);
    }

    BinderPipeline binder(semanticContext);
    for (auto& unit : units)
    {
        binder.declare_symbols(unit->ast);
    }
    binder.resolve_signatures();
    binder.resolve_attributes();
    binder.bind_methods();
    binder.validate_signatures();

    for (auto* method : semanticContext.methods)
    {
        FlowAnalyzer::analyze(method, diag);
    }

    compiled = true;
}

}
