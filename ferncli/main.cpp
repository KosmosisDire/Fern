#include <logger.hpp>

#include <fern.hpp>
#include <crtdbg.h>

int main(int argc, char* argv[])
{
    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_WARN, _CRTDBG_FILE_STDERR);
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);

    if (argc < 2)
    {
        LOG(LogChannel::Debug) << "Usage: fern <file>\n";
        return 1;
    }

    Fern::Compilation compilation;

    for (int i = 1; i < argc; ++i)
    {
        compilation.add_file(argv[i]);
    }

    compilation.compile();

    for (const auto& unit : compilation.get_units())
    {
        Fern::TokenWalker walker(unit->tokens);
        LOG(LogChannel::Debug) << walker.format() << "\n";
        LOG(LogChannel::Debug) << Fern::AstDebugFormatter::format(unit->ast) << "\n";
    }

    LOG(LogChannel::Debug) << "---- Annotated AST ----\n";
    for (const auto& unit : compilation.get_units())
    {
        LOG(LogChannel::Debug) << Fern::AnnotatedAstFormatter::format(unit->ast, compilation.semantic().bindings) << "\n";
    }

    LOG(LogChannel::Debug) << compilation.semantic().format() << "\n";

    return 0;
}

