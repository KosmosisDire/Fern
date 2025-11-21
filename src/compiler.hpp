// compiler.hpp
#pragma once
#include "ast/ast.hpp"
#include "semantic/symbol_table.hpp"
#include "semantic/type_system.hpp"
#include "compiled_module.hpp"
#include "parser/token_stream.hpp"
#include "binding/bound_tree.hpp"
#include "binding/bound_tree_builder.hpp"
#include "common/error.hpp"

#include <string>
#include <memory>

namespace Fern
{

    class Parser;

    struct SourceFile
    {
        std::string filename;
        std::string source;
    };

    struct FileCompilationState
    {
        SourceFile file;
        std::unique_ptr<TokenStream> tokens;      // store the token stream here
        std::unique_ptr<Parser> parser;           // store the parser since it owns the AST
        std::unique_ptr<TypeSystem> typeSystem;   // type system for this file
        std::unique_ptr<SymbolTable> symbolTable; // symbols local to this file
        std::unique_ptr<BoundTreeBuilder> boundTreeBuilder;       // binder for this file
        CompilationUnitSyntax *ast;               // pointer to the AST root
        BoundCompilationUnit *boundTree;          // pointer to the bound tree root
        

        std::vector<Diagnostic> diagnostics;

        bool parse_complete = false;
        bool symbols_complete = false;
    };

    class Compiler
    {
    private:
        // Configuration options
        bool verbose = false;
        bool print_ast = false;
        bool print_symbols = false;
        bool print_hlir = false;

        void add_builtin_functions(SymbolTable& global_symbols);

    public:

        // Main compilation function
        std::unique_ptr<CompiledModule> compile(const std::vector<SourceFile> &source_files);
        std::unique_ptr<CompiledModule> compile(const SourceFile &source)
        {
            return compile(std::vector<SourceFile>{source});
        }

        // Configuration
        void set_verbose(bool v) { verbose = v; }
        void set_print_ast(bool p) { print_ast = p; }
        void set_print_symbols(bool p) { print_symbols = p; }
        void set_print_hlir(bool p) { print_hlir = p; }
    };

} // namespace Fern