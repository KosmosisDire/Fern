#include "compiler.hpp"
#include "test_runner.hpp"
#include "parser/lexer.hpp"
#include "parser/parser.hpp"
#include "common/logger.hpp"
#include "common/file_utils.hpp"
#include "ast/ast.hpp"
#include <fstream>
#include <sstream>
#include <iostream>
#include <cstring>
#include <algorithm>
#include <filesystem>

using namespace Fern;

std::string read_file(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + filename);
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

enum class CompileMode {
    Run,
    Build
};

struct CommandLineArgs {
    CompileMode mode = CompileMode::Run;
    std::vector<std::string> source_files;
    std::string output_file;
    bool show_help = false;
    bool run_tests = false;
    std::string test_dir = "tests";
};

void show_help(const std::string& program_name) {
    std::cout << "Fern Programming Language Compiler\n\n";
    std::cout << "Usage:\n";
    std::cout << "  " << program_name << " run <source files>              Compile and run via JIT\n";
    std::cout << "  " << program_name << " build <source files> [options]  Compile to object file\n";
    std::cout << "\nOptions:\n";
    std::cout << "  --help, -h              Show this help message\n";
    std::cout << "  -o, --output <file>     Specify output file (build mode only)\n";
    #ifdef FERN_DEBUG
    std::cout << "  --test, -t [dir]        Run tests in the specified directory (default: tests)\n";
    #endif
    std::cout << "\nWildcard Support:\n";
    std::cout << "  *       Matches any sequence of characters\n";
    std::cout << "  ?       Matches any single character\n";
    std::cout << "\nExamples:\n";
    std::cout << "  " << program_name << " run main.fn\n";
    std::cout << "  " << program_name << " run runtime/std.fn main.fn\n";
    std::cout << "  " << program_name << " run *.fn                  # Run all .fn files in current dir\n";
    std::cout << "  " << program_name << " run runtime/*.fn          # Run all .fn files in runtime/\n";
    std::cout << "  " << program_name << " build main.fn\n";
    std::cout << "  " << program_name << " build main.fn -o program.o\n";
    std::cout << "  " << program_name << " build lib.fn utils.fn --output mylib.o\n";
    std::cout << "  " << program_name << " build src/*.fn -o output.o   # Build all .fn files in src/\n";
}

std::string get_default_output_name(const std::string& first_source_file) {
    std::filesystem::path source_path(first_source_file);
    std::string base_name = source_path.stem().string();
    return base_name + ".o";
}

CommandLineArgs parse_args(int argc, char* argv[]) {
    CommandLineArgs args;

    if (argc < 2) {
        #ifdef FERN_DEBUG
        args.mode = CompileMode::Run;
        args.source_files = {"minimal.fn", "runtime/string.fn"};
        #else
        args.show_help = true;
        #endif
        return args;
    }

    std::string first_arg = argv[1];

    if (first_arg == "--help" || first_arg == "-h") {
        args.show_help = true;
        return args;
    }

    if (first_arg == "--test" || first_arg == "-t") {
        args.run_tests = true;
        if (argc > 2) {
            args.test_dir = argv[2];
        }
        return args;
    }

    int arg_index = 1;
    if (first_arg == "run") {
        args.mode = CompileMode::Run;
        arg_index = 2;
    } else if (first_arg == "build") {
        args.mode = CompileMode::Build;
        arg_index = 2;
    } else {
        args.mode = CompileMode::Run;
        arg_index = 1;
    }

    for (int i = arg_index; i < argc; i++) {
        std::string arg = argv[i];

        if (arg == "-o" || arg == "--output") {
            if (i + 1 < argc) {
                args.output_file = argv[i + 1];
                i++;
            } else {
                std::cerr << "Error: " << arg << " requires a file path\n";
                args.show_help = true;
                return args;
            }
        } else {
            auto expanded = expand_path_pattern(arg);
            if (expanded.empty()) {
                std::cerr << "Warning: No files matched pattern: " << arg << std::endl;
            }
            args.source_files.insert(args.source_files.end(), expanded.begin(), expanded.end());
        }
    }

    if (args.source_files.empty() && !args.show_help) {
        std::cerr << "Error: No source files specified\n";
        args.show_help = true;
    }

    if (args.mode == CompileMode::Build && args.output_file.empty() && !args.source_files.empty()) {
        args.output_file = get_default_output_name(args.source_files[0]);
    }

    return args;
}

int main(int argc, char* argv[])
{
    Logger& logger = Logger::get_instance();
    logger.initialize();
    #ifdef FERN_DEBUG
        logger.set_console_level(LogLevel::TRACE);
    #else
        logger.set_console_level(LogLevel::NONE);
    #endif

    CommandLineArgs args = parse_args(argc, argv);

    if (args.show_help) {
        show_help(argv[0]);
        return 0;
    }

    if (args.run_tests) {
        TestRunner runner;
        auto results = runner.run_all_tests(args.test_dir);
        runner.print_summary(results);
        auto benchmark = runner.run_compile_benchmark(args.test_dir, 10);
        runner.print_benchmark(benchmark);

        bool all_passed = std::all_of(results.begin(), results.end(),
            [](const TestResult& r) { return r.status == TestStatus::Passed; });
        return all_passed ? 0 : 1;
    }

    Compiler compiler;
    #ifdef FERN_DEBUG
        compiler.set_print_ast(true);
        compiler.set_print_symbols(true);
        compiler.set_print_flir(true);
    #endif

    std::vector<SourceFile> source_files;
    for (const auto& filename : args.source_files)
    {
        try
        {
            auto source = read_file(filename);
            source_files.push_back({filename, source});
        }
        catch (const std::exception& e)
        {
            std::cerr << "Error: " << e.what() << std::endl;
            return 1;
        }
    }

    if (source_files.empty())
    {
        std::cerr << "Error: No source files to compile" << std::endl;
        return 1;
    }

    auto result = compiler.compile(source_files);

    if (!result || !result->is_valid())
    {
        std::cerr << "\nCompilation failed with " << result->error_count() << " error(s):\n" << std::endl;
        const auto& errors = result->get_diagnostics();
        if (errors.empty())
        {
            std::cerr << "  (No error details available)" << std::endl;
        }
        else
        {
            for (const auto& error : errors)
            {
                std::cerr << "  " << result->format_diagnostic(error) << std::endl;
            }
        }
        return 1;
    }

    if (args.mode == CompileMode::Build)
    {
        #ifdef FERN_DEBUG
            result->dump_ir();
        #endif

        if (result->write_object_file(args.output_file))
        {
            std::cout << "Compiled successfully to: " << args.output_file << std::endl;
            return 0;
        }
        else
        {
            std::cerr << "Failed to write object file: " << args.output_file << std::endl;
            return 1;
        }
    }
    else
    {
        #ifdef FERN_DEBUG
            result->dump_ir();
        #endif

        auto ret = result->execute_jit<float>("Main").value_or(-1.0f);
        std::cout << "\n";
        std::cout << "______________________________\n\n";
        std::cout << "Program returned: " << ret << std::endl;
        return static_cast<int>(ret);
    }
}
