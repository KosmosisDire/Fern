// compiler.hpp
#pragma once
#include "ast/ast.hpp"
#include "semantic/symbol_table.hpp"
#include "semantic/type_system.hpp"
#include "compiled_module.hpp"
#include "parser/token_stream.hpp"
#include "binding/bound_tree.hpp"
#include "binding/binding_arena.hpp"
#include "common/error.hpp"

#include <string>
#include <memory>
#include <iostream>

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
        std::unique_ptr<BindingArena> arena;      // owns all bound tree nodes
        CompilationUnitSyntax *ast;               // pointer to the AST root
        BoundCompilationUnit *boundTree;          // pointer to the bound tree root

        std::vector<Diagnostic> diagnostics;

        bool parse_complete = false;
        bool symbols_complete = false;

        // Diagnostic helpers
        void collect_diagnostics(const DiagnosticSystem& system)
        {
            const auto& diags = system.get_diagnostics();
            diagnostics.insert(diagnostics.end(), diags.begin(), diags.end());
        }

        bool has_errors() const
        {
            for (const auto& diag : diagnostics)
            {
                if (diag.severity == Diagnostic::Severity::Error ||
                    diag.severity == Diagnostic::Severity::Fatal)
                {
                    return true;
                }
            }
            return false;
        }

        void print_diagnostics() const
        {
            if (diagnostics.empty())
                return;
            std::cerr << "\n=== Diagnostics for " << file.filename << " ===\n";
            for (const auto& diag : diagnostics)
            {
                std::cerr << diag.to_string() << "\n";
            }
        }
    };

    class Compiler
    {
    private:
        // Configuration options
        bool verbose = false;
        bool print_ast = false;
        bool print_symbols = false;
        bool print_fnir = false;

        void add_builtin_functions(SymbolTable& global_symbols);

        // Multi-file diagnostic helpers
        static bool has_any_errors(const std::vector<FileCompilationState>& states,
                                   const std::vector<Diagnostic>& global_diagnostics = {});
        static void print_all_diagnostics(const std::vector<FileCompilationState>& states);
        static std::vector<Diagnostic> gather_all_diagnostics(const std::vector<FileCompilationState>& states,
                                                              const std::vector<Diagnostic>& global_diagnostics = {});

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
        void set_print_fnir(bool p) { print_fnir = p; }
    };

} // namespace Fern