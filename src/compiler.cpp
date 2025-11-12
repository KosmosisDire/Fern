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
#include "hlir/hlir.hpp"
#include "hlir/bound_to_hlir.hpp"
// #include "hlir/mem2reg.hpp"

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

namespace Fern
{

    std::unique_ptr<CompiledModule> Compiler::compile(const std::vector<SourceFile> &source_files)
    {
        if (source_files.empty())
        {
            return std::make_unique<CompiledModule>();
        }

        std::vector<std::string> all_errors;
        std::vector<FileCompilationState> file_states(source_files.size());

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

            if (lexer.has_errors())
            {
                for (const auto &error : lexer.get_diagnostics())
                {
                    state.errors.push_back(state.file.filename + " - Lexer: " + error.message);
                }
                continue;
            }

            state.tokens = std::make_unique<TokenStream>(std::move(tokens));
            state.parser = std::make_unique<Parser>(*state.tokens);
            state.ast = state.parser->parse();

            if (!state.ast)
            {
                state.errors.push_back(state.file.filename + ": Invalid AST");
                continue;
            }

            for (const auto &error : state.parser->getErrors())
            {
                state.errors.push_back(state.file.filename + " - Parser: " +
                                       error.location.start.to_string() + ": " + error.message);
            }

            if (print_ast)
            {
                LOG_INFO("\nAST for " + state.file.filename + ":\n", LogCategory::COMPILER);
                AstPrinter printer;
                std::cout << printer.get_string(state.ast) << "\n";
            }

            state.parse_complete = true;
        }

        // Collect parsing errors
        for (const auto &state : file_states)
        {
            all_errors.insert(all_errors.end(), state.errors.begin(), state.errors.end());
        }

        if (!all_errors.empty())
        {
            LOG_HEADER("Parsing errors encountered", LogCategory::COMPILER);
            for (const auto &error : all_errors)
            {
                LOG_ERROR(error, LogCategory::COMPILER);
            }
            return std::make_unique<CompiledModule>(all_errors);
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

            for (const auto &error : builder.get_errors())
            {
                state.errors.push_back(state.file.filename + " - Declaration: " + error);
            }

            if (print_symbols)
            {
                LOG_INFO("\nLocal Symbol Table for " + state.file.filename + ":\n", LogCategory::COMPILER);
                LOG_INFO(state.symbolTable->to_string() + "\n", LogCategory::COMPILER);
            }

            state.symbols_complete = true;
        }

        // Collect symbol building errors
        for (const auto &state : file_states)
        {
            all_errors.insert(all_errors.end(), state.errors.begin(), state.errors.end());
        }

        if (!all_errors.empty())
        {
            LOG_HEADER("Symbol building errors encountered", LogCategory::COMPILER);
            for (const auto &error : all_errors)
            {
                LOG_ERROR(error, LogCategory::COMPILER);
            }
            return std::make_unique<CompiledModule>(all_errors);
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

            // Report conflicts as errors
            for (const auto &conflict : conflicts)
            {
                all_errors.push_back(state.file.filename + " - " + conflict);
            }
        }

        // Check for merge conflicts
        if (!all_errors.empty())
        {
            LOG_HEADER("Symbol merge conflicts", LogCategory::COMPILER);
            for (const auto &error : all_errors)
            {
                LOG_ERROR(error, LogCategory::COMPILER);
            }
            return std::make_unique<CompiledModule>(all_errors);
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

            // Create binder and bind the AST
            state.boundTreeBuilder = std::make_unique<BoundTreeBuilder>(*global_symbols.get());
            state.boundTree = state.boundTreeBuilder->bind(state.ast);
            // resolver_visitor.visit(state.boundTree);

            if (!state.boundTree)
            {
                state.errors.push_back(state.file.filename + ": Invalid Bound Tree");
                continue;
            }

            // for (const auto &error : binder.)
            // {
            //     state.errors.push_back(state.file.filename + " - Binding: " + error);
            // }

            if (print_ast)
            {
                LOG_INFO("\nBound Tree for " + state.file.filename + ":\n", LogCategory::COMPILER);
                BoundTreePrinter printer;
                printer.visit(state.boundTree);
            }

            state.parse_complete = true;
        }

        // Collect errors
        for (const auto &state : file_states)
        {
            all_errors.insert(all_errors.end(), state.errors.begin(), state.errors.end());
        }

        if (!all_errors.empty())
        {
            LOG_HEADER("Binding errors encountered", LogCategory::COMPILER);
            for (const auto &error : all_errors)
            {
                LOG_ERROR(error, LogCategory::COMPILER);
            }
            return std::make_unique<CompiledModule>(all_errors);
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
        for (const auto &state : file_states)
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

                for (const auto &error : resolver.get_errors())
                {
                    all_errors.push_back(state.file.filename + " - " + error);
                }
            }
        }

        if (print_symbols)
        {
            LOG_INFO("\nGlobal Symbol Table after Type Resolution:\n", LogCategory::COMPILER);
            LOG_INFO(global_symbols->to_string(), LogCategory::COMPILER);
        }

        if (!all_errors.empty())
        {
            LOG_HEADER("Type resolution errors", LogCategory::COMPILER);
            for (const auto &error : all_errors)
            {
                LOG_ERROR(error, LogCategory::COMPILER);
            }

            return std::make_unique<CompiledModule>(all_errors);
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
            all_errors.push_back("LLVM code generation error: " + std::string(e.what()));
        }

        if (!all_errors.empty())
        {
            LOG_HEADER("Code generation errors", LogCategory::COMPILER);
            for (const auto &error : all_errors)
            {
                LOG_ERROR(error, LogCategory::COMPILER);
            }
            return std::make_unique<CompiledModule>(all_errors);
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
            all_errors);
    }

} // namespace Fern