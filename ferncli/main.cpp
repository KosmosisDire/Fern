#include <logger.hpp>

#include <fern.hpp>


int main(int argc, char* argv[])
{
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

    for (const auto& unit : compilation.units())
    {
        Fern::TokenWalker walker(unit.tokens);
        LOG(LogChannel::Debug) << walker.format() << "\n";
        LOG(LogChannel::Debug) << Fern::AstDebugFormatter::format(unit.ast) << "\n";
    }

    LOG(LogChannel::Debug) << compilation.declaration_table().format() << "\n";

    return 0;
}

