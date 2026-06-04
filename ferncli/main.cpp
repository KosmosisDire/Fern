#include <logger.hpp>

#include <fern.hpp>
#include <ast/ast.hpp>
#include <semantic/fhir/fmt.hpp>
#include <semantic/symbol/symbol.hpp>
#include <flir/fmt.hpp>

#include <algorithm>
#include <cstdint>
#include <memory>
#include <string_view>
#include <vector>

#include "test_runner.hpp"

#ifdef _MSC_VER
#include <crtdbg.h>
#endif

struct CompileArgs
{
    std::vector<std::string_view> files;
    std::vector<std::string_view> dumpPaths;
};

// Splits the run arguments into source files and the --dump whitelist.
static bool parse_compile_args(int argc, char* argv[], int firstArg, CompileArgs& out)
{
    for (int i = firstArg; i < argc; ++i)
    {
        std::string_view arg{argv[i]};
        if (arg == "--dump")
        {
            if (i + 1 >= argc)
            {
                LOG(LogChannel::General) << "--dump requires a file argument\n";
                return false;
            }
            out.dumpPaths.push_back(argv[++i]);
        }
        else
        {
            out.files.push_back(arg);
        }
    }
    return true;
}

static std::unique_ptr<Fern::Compilation> run_compile(const std::vector<std::string_view>& files)
{
    auto compilation = std::make_unique<Fern::Compilation>();
    for (std::string_view file : files)
        compilation->add_file(file);
    compilation->compile();
    return compilation;
}

// Dumps the debug output (AST, symbols, FHIR, FLIR) for the whitelisted files.
// An empty whitelist dumps nothing.
static void dump_debug(const Fern::Compilation& compilation, const std::vector<std::string_view>& dumpPaths)
{
    // Resolve the dump whitelist to file ids by matching the source paths given
    // on the command line.
    std::vector<uint32_t> dumpFiles;
    for (const auto& unit : compilation.get_units())
    {
        if (std::find(dumpPaths.begin(), dumpPaths.end(), unit->sourceFile->path()) != dumpPaths.end())
            dumpFiles.push_back(unit->sourceFile->file_id());
    }

    if (dumpFiles.empty()) return;

    auto file_skipped = [&](uint32_t fileId)
    {
        return std::find(dumpFiles.begin(), dumpFiles.end(), fileId) == dumpFiles.end();
    };

    auto method_skipped = [&](Fern::MethodSymbol* sym)
    {
        return !sym || !sym->syntax || file_skipped(sym->syntax->span.fileId);
    };

    for (const auto& unit : compilation.get_units())
    {
        if (file_skipped(unit->sourceFile->file_id())) continue;
        LOG(LogChannel::Debug) << Fern::AstDebugFormatter::format(unit->ast) << "\n";
    }

    LOG(LogChannel::Debug) << compilation.semantic().format(dumpFiles) << "\n";

    LOG(LogChannel::Debug) << "---- FHIR ----\n";
    for (auto* method : compilation.semantic().methods)
    {
        if (method_skipped(method->symbol)) continue;
        LOG(LogChannel::Debug) << Fern::FhirDebugFormatter::format(method) << "\n";
    }

    LOG(LogChannel::Debug) << "---- FLIR ----\n";
    for (auto* method : compilation.flir().methods)
    {
        if (method_skipped(method->symbol)) continue;
        LOG(LogChannel::Debug) << Fern::FlirDebugFormatter::format(method) << "\n";
    }
}

static void print_diagnostics(const Fern::Compilation& compilation)
{
    for (const auto& diag : compilation.diag.get_diagnostics())
    {
        auto filePath = diag.location.fileId >= 0 && diag.location.fileId < (int)compilation.get_units().size()
                            ? compilation.get_units()[diag.location.fileId]->sourceFile->path()
                            : "UnknownFile";
        LOG(LogChannel::General) << diag.format(filePath) << "\n";
    }
}

int main(int argc, char* argv[])
{
#ifdef _MSC_VER
    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_WARN, _CRTDBG_FILE_STDERR);
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif

    if (argc < 2)
    {
        LOG(LogChannel::General) << "Usage: fern run <file>... [--dump <file>]...\n";
        LOG(LogChannel::General) << "       fern test <folder> [--include <file>]...\n";
        return 1;
    }

    std::string_view first{argv[1]};
    if (first == "run")
    {
        if (argc < 3)
        {
            LOG(LogChannel::General) << "Usage: fern run <file>... [--dump <file>]...\n";
            return 1;
        }

        CompileArgs args;
        if (!parse_compile_args(argc, argv, 2, args))
            return 1;

        auto compilation = run_compile(args.files);
        dump_debug(*compilation, args.dumpPaths);
        print_diagnostics(*compilation);
        return 0;
    }

    if (first == "test")
    {
        if (argc < 3)
        {
            LOG(LogChannel::General) << "Usage: fern test <folder> [--include <file>]...\n";
            return 1;
        }
        std::string folder = argv[2];
        std::vector<std::string> includes;
        for (int i = 3; i < argc; ++i)
        {
            std::string_view arg{argv[i]};
            if (arg == "--include")
            {
                if (i + 1 >= argc)
                {
                    LOG(LogChannel::General) << "--include requires a file argument\n";
                    return 1;
                }
                includes.emplace_back(argv[++i]);
            }
            else
            {
                LOG(LogChannel::General) << "Unknown argument: " << arg << "\n";
                return 1;
            }
        }
        Logger::disable(LogChannel::Debug);
        Fern::TestRunner runner;
        return runner.run(folder, includes);
    }

    LOG(LogChannel::General) << "Unknown subcommand: " << first << "\n";
    LOG(LogChannel::General) << "Usage: fern run <file>... [--dump <file>]...\n";
    LOG(LogChannel::General) << "       fern test <folder> [--include <file>]...\n";
    return 1;
}
