#include <iostream>
#include <fstream>
#include <sstream>

#include <logger.hpp>
#include <source/file.hpp>
#include <fern.hpp>

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        LOG(LogChannel::Debug) << "Usage: fern <file>\n";
        return 1;
    }

    std::ifstream file(argv[1]);
    if (!file)
    {
        LOG(LogChannel::Debug) << "Error: Could not open file '" << argv[1] << "'\n";
        return 1;
    }

    std::ostringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();

    Fern::SourceFile sourceFile(source, argv[1], 0);

    Fern::Lexer lexer(sourceFile);
    std::vector<Fern::Token> tokens = lexer.tokenize();

    Fern::TokenWalker tokenWalker(tokens);
    std::cout << tokenWalker.format() << "\n";

    Fern::AstArena astArena;
    Fern::Parser parser(tokenWalker, astArena);
    auto ast = parser.parse();

    std::cout << Fern::AstDebugFormatter::format(ast) << "\n";

    return 0;
}
