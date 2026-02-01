#include "compilation.hpp"

#include <fstream>
#include <sstream>
#include <stdexcept>

#include <lexer/lexer.hpp>
#include <parser/parser.hpp>
#include <token/walker.hpp>

namespace Fern
{

void Compilation::add_file(std::string_view path)
{
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
    CompilationUnit unit;
    unit.sourceFile = std::make_unique<SourceFile>(std::move(source), std::string(path), m_nextFileId++);
    m_units.push_back(std::move(unit));
}

void Compilation::compile()
{
    for (auto& unit : m_units)
    {
        Lexer lexer(*unit.sourceFile);
        unit.tokens = lexer.tokenize();

        TokenWalker walker(unit.tokens);
        Parser parser(walker, m_arena);
        unit.ast = parser.parse();
    }

    for (const auto& unit : m_units)
    {
        m_declTable.add(unit.ast);
    }
}

const DeclarationTable& Compilation::declaration_table() const
{
    return m_declTable;
}

const std::vector<CompilationUnit>& Compilation::units() const
{
    return m_units;
}

}
