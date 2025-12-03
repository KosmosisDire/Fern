#include "compiler.hpp"

#include "common/logger.hpp"
#include "codegen/hlir_codegen.hpp"
#include "semantic/symbol_table.hpp"
#include "parser/lexer.hpp"
#include "parser/parser.hpp"
#include "ast/ast.hpp"
#include "ast/ast_printer.hpp"
#include "ast/ast_code_printer.hpp"
#include "binding/bound_tree_builder.hpp"
#include "binding/bound_tree_printer.hpp"
#include "semantic/type_resolver.hpp"
#include "semantic/symbol_table_builder.hpp"
#include "semantic/syntax_validator.hpp"
#include "hlir/hlir.hpp"
#include "hlir/bound_to_hlir.hpp"
#include "binding/conversion_inserter.hpp"

#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <optional>
#include <iostream>

namespace Fern
{
    bool Compiler::has_any_errors(const std::vector<FileCompilationState>& states,
                                   const std::vector<Diagnostic>& global_diagnostics)
    {
        for (const auto& state : states)
        {
            if (state.has_errors())
                return true;
        }
        for (const auto& diag : global_diagnostics)
        {
            if (diag.severity == Diagnostic::Severity::Error ||
                diag.severity == Diagnostic::Severity::Fatal)
            {
                return true;
            }
        }
        return false;
    }

    void Compiler::print_all_diagnostics(const std::vector<FileCompilationState>& states)
    {
        for (const auto& state : states)
        {
            state.print_diagnostics();
        }
    }

    std::vector<Diagnostic> Compiler::gather_all_diagnostics(const std::vector<FileCompilationState>& states,
                                                              const std::vector<Diagnostic>& global_diagnostics)
    {
        std::vector<Diagnostic> all_diagnostics;
        for (const auto& state : states)
        {
            all_diagnostics.insert(all_diagnostics.end(), state.diagnostics.begin(), state.diagnostics.end());
        }
        all_diagnostics.insert(all_diagnostics.end(), global_diagnostics.begin(), global_diagnostics.end());
        return all_diagnostics;
    }

    std::unique_ptr<CompiledModule> Compiler::compile(const std::vector<SourceFile> &source_files)
    {
        if (source_files.empty())
        {
            return std::make_unique<CompiledModule>();
        }

        std::vector<FileCompilationState> file_states(source_files.size());

        // Global diagnostics for errors not associated with a specific file
        // (e.g., codegen errors that operate on merged HLIR)
        std::vector<Diagnostic> global_diagnostics;

        // === Parse all files sequentially ===
        LOG_HEADER("Sequential parsing", LogCategory::COMPILER);

        for (size_t i = 0; i < source_files.size(); ++i)
        {
            auto &state = file_states[i];
            state.file = source_files[i];

            LOG_INFO("Parsing: " + state.file.filename, LogCategory::COMPILER);

            // Lex and parse
            auto lexer = Lexer(state.file.source);
            auto tokens = lexer.tokenize_all();

            std::cout << tokens.to_string();

            if (lexer.has_errors())
            {
                state.collect_diagnostics(lexer);
                continue;
            }

            state.tokens = std::make_unique<TokenStream>(std::move(tokens));
            state.parser = std::make_unique<Parser>(*state.tokens);
            state.ast = state.parser->parse();

            if (!state.ast)
            {
                continue;
            }

            if (state.parser->has_errors())
            {
                state.collect_diagnostics(*state.parser);
                continue;
            }

            if (print_ast)
            {
                LOG_INFO("\nAST for " + state.file.filename + ":\n", LogCategory::COMPILER);
                AstPrinter printer;
                std::cout << printer.get_string(state.ast) << "\n";
            }

            state.parse_complete = true;
        }

        // Early return if parsing failed - can't continue without ASTs
        if (has_any_errors(file_states))
        {
            return std::make_unique<CompiledModule>(gather_all_diagnostics(file_states));
        }

        // === Syntax validation (AST structural validation) ===
        LOG_HEADER("Syntax validation", LogCategory::COMPILER);

        for (size_t i = 0; i < file_states.size(); ++i)
        {
            auto &state = file_states[i];
            if (!state.parse_complete)
                continue;

            LOG_INFO("Validating: " + state.file.filename, LogCategory::COMPILER);

            SyntaxValidator validator;
            validator.validate(state.ast);

            state.collect_diagnostics(validator);
        }

        if (has_any_errors(file_states))
        {
            return std::make_unique<CompiledModule>(gather_all_diagnostics(file_states));
        }

        // // === Build local symbol tables sequentially ===
        LOG_HEADER("Sequential symbol collection", LogCategory::COMPILER);

        for (size_t i = 0; i < file_states.size(); ++i)
        {
            auto &state = file_states[i];

            if (!state.parse_complete)
                continue;

            LOG_INFO("Building symbols for: " + state.file.filename, LogCategory::COMPILER);

            // Create type system and symbol table for this file
            state.typeSystem = std::make_unique<TypeSystem>();
            state.typeSystem->init_primitives();
            state.symbolTable = std::make_unique<SymbolTable>(*state.typeSystem);

            // Collect declarations
            SymbolTableBuilder builder(*state.symbolTable);
            builder.build(state.ast);

            // Collect symbol table builder diagnostics
            state.collect_diagnostics(builder);

            if (print_symbols)
            {
                LOG_INFO("\nLocal Symbol Table for " + state.file.filename + ":\n", LogCategory::COMPILER);
                LOG_INFO(state.symbolTable->to_string() + "\n", LogCategory::COMPILER);
            }

            state.symbols_complete = true;
        }

        // Early return if symbol building failed - can't continue without symbols
        if (has_any_errors(file_states))
        {
            return std::make_unique<CompiledModule>(gather_all_diagnostics(file_states));
        }

        // === Merge symbol tables ===
        LOG_HEADER("Merging symbol tables", LogCategory::COMPILER);

        // Create the global symbol table
        auto global_type_system = std::make_unique<TypeSystem>();
        auto global_symbols = std::make_unique<SymbolTable>(*global_type_system);

        // Merge all local symbol tables into global
        for (auto &state : file_states)
        {
            if (!state.symbols_complete)
                continue;

            LOG_INFO("Merging symbols from: " + state.file.filename, LogCategory::COMPILER);

            // Merge the local table into global
            auto conflicts = global_symbols->merge(*state.symbolTable);

            // Report conflicts as errors (TODO: convert conflicts to proper Diagnostics)
            for (const auto &conflict : conflicts)
            {
                state.diagnostics.push_back(Diagnostic(
                    Diagnostic::Severity::Error,
                    "Symbol merge conflict: " + conflict,
                    SourceRange(),
                    "SymbolMerge"
                ));
            }
        }

        // Early return if merge conflicts - can't continue with conflicting symbols
        if (has_any_errors(file_states))
        {
            return std::make_unique<CompiledModule>(gather_all_diagnostics(file_states));
        }

        // Add built-in functions to global symbol table
        // add_builtin_functions(*global_symbols);

        // print global symbol table
        LOG_INFO("\nGlobal Symbol Table after Merging:\n", LogCategory::COMPILER);
        LOG_INFO(global_symbols->to_string(), LogCategory::COMPILER);

        // === Binding ===
        // SymbolResolutionVisitor resolver_visitor(*global_symbols);
        for (auto &state : file_states)
        {
            if (!state.symbols_complete)
                continue;

            LOG_INFO("Binding AST for: " + state.file.filename, LogCategory::COMPILER);

            // Create arena and binder
            state.arena = std::make_unique<BindingArena>();
            BoundTreeBuilder builder(*state.arena, *global_symbols);
            state.boundTree = builder.bind(state.ast);

            // Collect binder diagnostics
            state.collect_diagnostics(builder);

            if (!state.boundTree)
            {
                continue;
            }

            if (print_ast)
            {
                LOG_INFO("\nBound Tree for " + state.file.filename + ":\n", LogCategory::COMPILER);
                BoundTreePrinter printer;
                printer.visit(state.boundTree);
            }

            state.parse_complete = true;
        }

        // Early return if binding failed - can't continue without bound trees
        if (has_any_errors(file_states))
        {
            return std::make_unique<CompiledModule>(gather_all_diagnostics(file_states));
        }

        // === Type resolution with global symbol table ===
        LOG_HEADER("Type resolution", LogCategory::COMPILER);

        TypeResolver resolver(*global_symbols);
        for (int i = 0; i < 10; ++i)
        {
            for (const auto &state : file_states)
            {
                if (state.boundTree)
                {
                    resolver.resolve(state.boundTree);
                }
            }
        }

        // run the resolver a second time to do a final pass after all files have run
        for (auto &state : file_states)
        {
            if (state.boundTree)
            {
                resolver.resolve(state.boundTree);

                if (print_ast)
                {
                    LOG_INFO("\n Bound Tree for " + state.file.filename + ":\n", LogCategory::COMPILER);
                    BoundTreePrinter printer;
                    printer.visit(state.boundTree);
                }

                // Collect type resolver diagnostics for this file
                // Note: TypeResolver diagnostics are collected per-file after each file's resolution
                state.collect_diagnostics(resolver);
                resolver.clear_diagnostics();
            }
        }

        if (print_symbols)
        {
            LOG_INFO("\nGlobal Symbol Table after Type Resolution:\n", LogCategory::COMPILER);
            LOG_INFO(global_symbols->to_string(), LogCategory::COMPILER);
        }

        // Early return if type resolution failed - can't continue with unresolved types
        if (has_any_errors(file_states))
        {
            return std::make_unique<CompiledModule>(gather_all_diagnostics(file_states));
        }

        // === Insert implicit conversions ===
        LOG_HEADER("Conversion insertion", LogCategory::COMPILER);

        for (auto &state : file_states)
        {
            if (!state.boundTree)
                continue;

            ConversionInserter inserter(*state.arena);
            inserter.transform(state.boundTree);

            // Collect conversion inserter diagnostics
            state.collect_diagnostics(inserter);
        }

        // Early return if conversion insertion failed
        if (has_any_errors(file_states))
        {
            return std::make_unique<CompiledModule>(gather_all_diagnostics(file_states));
        }

        // === Convert bound tree to HLIR ===
        LOG_HEADER("HLIR generation", LogCategory::COMPILER);

        // Create HLIR module
        auto hlir_module = std::make_unique<HLIR::Module>("FernProgram", global_symbols->get_global_namespace());
        
        // Convert each bound tree to HLIR
        for (auto &state : file_states)
        {
            if (!state.boundTree)
                continue;

            LOG_INFO("Generating HLIR for: " + state.file.filename, LogCategory::COMPILER);

            HLIR::BoundToHLIR converter(hlir_module.get(), global_type_system.get());
            converter.build(state.boundTree);

            // Collect HLIR generation diagnostics
            state.collect_diagnostics(converter);
        }

        // Early return if HLIR generation failed
        if (has_any_errors(file_states))
        {
            return std::make_unique<CompiledModule>(gather_all_diagnostics(file_states));
        }

        // Dump non-SSA HLIR if requested
        if (print_hlir)
        {
            LOG_HEADER("HLIR Output (Non-SSA)", LogCategory::COMPILER);
            std::cout << hlir_module->dump() << "\n";
        }

        // === LLVM Code Generation from HLIR ===
        LOG_HEADER("LLVM code generation", LogCategory::COMPILER);

        auto llvm_context = std::make_unique<llvm::LLVMContext>();
        HLIRCodeGen codegen(*llvm_context, "FernProgram");

        std::unique_ptr<llvm::Module> llvm_module;
        try
        {
            llvm_module = codegen.lower(hlir_module.get());
            LOG_INFO("LLVM IR generation successful", LogCategory::COMPILER);
        }
        catch (const std::exception &e)
        {
            global_diagnostics.push_back(Diagnostic(
                Diagnostic::Severity::Error,
                "LLVM code generation error: " + std::string(e.what()),
                SourceRange(),
                "CodeGen"
            ));
        }

        // Collect any codegen diagnostics (in addition to exceptions)
        const auto& codegen_diags = codegen.get_diagnostics();
        global_diagnostics.insert(global_diagnostics.end(), codegen_diags.begin(), codegen_diags.end());

        // Early return if code generation failed
        if (has_any_errors(file_states, global_diagnostics))
        {
            return std::make_unique<CompiledModule>(gather_all_diagnostics(file_states, global_diagnostics));
        }

        // === Run optimization passes ===
        LOG_HEADER("Running optimization passes", LogCategory::COMPILER);

        // Create analysis managers
        llvm::LoopAnalysisManager LAM;
        llvm::FunctionAnalysisManager FAM;
        llvm::CGSCCAnalysisManager CGAM;
        llvm::ModuleAnalysisManager MAM;

        // Create pass builder
        llvm::PassBuilder PB;

        // Register analysis managers
        // PB.registerModuleAnalyses(MAM);
        // PB.registerCGSCCAnalyses(CGAM);
        // PB.registerFunctionAnalyses(FAM);
        // PB.registerLoopAnalyses(LAM);
        // PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

        // // Build optimization pipeline (O2 level)
        // llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O2);

        // // Run the optimization passes
        // MPM.run(*llvm_module, MAM);

        LOG_INFO("Optimization passes completed", LogCategory::COMPILER);

        return std::make_unique<CompiledModule>(
            std::move(llvm_context),
            std::move(llvm_module),
            "FernProgram",
            gather_all_diagnostics(file_states, global_diagnostics));
    }

} // namespace Fern