#pragma once

#include <string>
#include <string_view>
#include <vector>

#include <arena.hpp>
#include <source/file.hpp>
#include <token/token.hpp>
#include <declaration/table.hpp>

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
    Compilation() = default;

    void add_file(std::string_view path);
    void add_source(std::string source, std::string_view path);
    void compile();

    const DeclarationTable& declaration_table() const;
    const std::vector<CompilationUnit>& units() const;

private:
    std::vector<CompilationUnit> m_units;
    AllocArena m_arena;
    DeclarationTable m_declTable;
    uint32_t m_nextFileId = 0;
};

}
