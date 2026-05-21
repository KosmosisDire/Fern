#include <logger.hpp>

#include <fern.hpp>
#include <semantic/fhir/fmt.hpp>
#include <flir/fmt.hpp>

#include <string_view>

#include "test_runner.hpp"

#ifdef _MSC_VER
#include <crtdbg.h>
#endif

static int run_compile(int argc, char* argv[], int firstFile)
{
    Fern::Compilation compilation;

    for (int i = firstFile; i < argc; ++i)
    {
        compilation.add_file(argv[i]);
    }

    compilation.compile();

    for (const auto& unit : compilation.get_units())
    {
        Fern::TokenWalker walker(unit->tokens);
        LOG(LogChannel::Debug) << Fern::AstDebugFormatter::format(unit->ast) << "\n";
    }

    LOG(LogChannel::Debug) << compilation.semantic().format() << "\n";

    LOG(LogChannel::Debug) << "---- FHIR ----\n";
    for (auto* method : compilation.semantic().methods)
    {
        LOG(LogChannel::Debug) << Fern::FhirDebugFormatter::format(method) << "\n";
    }

    LOG(LogChannel::Debug) << "---- FLIR ----\n";
    for (auto* method : compilation.flir().methods)
    {
        LOG(LogChannel::Debug) << Fern::FlirDebugFormatter::format(method) << "\n";
    }

    for (const auto& diag : compilation.diag.get_diagnostics())
    {
        auto filePath = diag.location.fileId >= 0 && diag.location.fileId < (int)compilation.get_units().size()
                            ? compilation.get_units()[diag.location.fileId]->sourceFile->path()
                            : "UnknownFile";
        LOG(LogChannel::General) << diag.format(filePath) << "\n";
    }

    return 0;
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
        LOG(LogChannel::General) << "Usage: fern run <file>...\n";
        LOG(LogChannel::General) << "       fern test <folder> [--include <file>]...\n";
        return 1;
    }

    std::string_view first{argv[1]};
    if (first == "run")
    {
        if (argc < 3)
        {
            LOG(LogChannel::General) << "Usage: fern run <file>...\n";
            return 1;
        }
        return run_compile(argc, argv, 2);
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
    LOG(LogChannel::General) << "Usage: fern run <file>...\n";
    LOG(LogChannel::General) << "       fern test <folder> [--include <file>]...\n";
    return 1;
}
