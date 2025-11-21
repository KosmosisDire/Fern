#include "test_runner.hpp"
#include "compiler.hpp"
#include "common/logger.hpp"
#include <filesystem>
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>

namespace fs = std::filesystem;

namespace Fern {

static std::string read_file(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + filename);
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

TestRunner::TestRunner() {
}

TestResult TestRunner::run_single_test(const std::string& test_file) {
    fs::path path(test_file);
    TestResult result(path.filename().string());

    try {
        // Create a compiler instance for this test
        Compiler compiler;
        compiler.set_print_ast(false);
        compiler.set_print_symbols(false);
        compiler.set_print_hlir(false);

        // Read and compile the test file
        std::string source = read_file(test_file);
        std::vector<SourceFile> source_files = {{test_file, source}};
        
        std::string stdlib = read_file("runtime/print.fn");
        source_files.push_back({"runtime/print.fn", stdlib});

        auto compile_result = compiler.compile(source_files);

        if (!compile_result || !compile_result->is_valid()) {
            result.compile_failed = true;
            if (compile_result) {
                std::stringstream ss;
                for (const auto& error : compile_result->get_diagnostics()) {
                    ss << error.message << "; ";
                }
                result.error_message = ss.str();
            } else {
                result.error_message = "No compilation result returned";
            }
            return result;
        }

        // Try to execute the test
        auto jit_result = compile_result->execute_jit<float>("Main");

        if (jit_result.has_value()) {
            result.return_value = jit_result.value();
            // Consider test passed if it returns a non-negative value
            // (negative values often indicate errors in convention)
            result.passed = (result.return_value >= 0.0f);
        } else {
            result.crashed = true;
            result.error_message = "JIT execution failed or Main not found";
        }

    } catch (const std::exception& e) {
        result.crashed = true;
        result.error_message = std::string("Exception: ") + e.what();
    } catch (...) {
        result.crashed = true;
        result.error_message = "Unknown exception occurred";
    }

    return result;
}

std::vector<TestResult> TestRunner::run_all_tests(const std::string& test_dir) {
    std::vector<TestResult> results;
    std::vector<std::string> test_files;

    // Collect all .fn files in the test directory
    try {
        for (const auto& entry : fs::directory_iterator(test_dir)) {
            if (entry.is_regular_file() && entry.path().extension() == ".fn") {
                test_files.push_back(entry.path().string());
            }
        }
    } catch (const std::exception& e) {
        std::cerr << "Error scanning test directory: " << e.what() << std::endl;
        return results;
    }

    // Sort test files by name for consistent ordering
    std::sort(test_files.begin(), test_files.end());

    std::cout << "Running " << test_files.size() << " tests from " << test_dir << "...\n" << std::endl;

    // Run each test
    int test_num = 0;
    for (const auto& test_file : test_files) {
        test_num++;
        std::cout << "[" << test_num << "/" << test_files.size() << "] Running "
                  << fs::path(test_file).filename().string() << "... " << std::flush;

        TestResult result = run_single_test(test_file);
        results.push_back(result);

        // Print immediate result
        if (result.passed) {
            std::cout << "PASS (returned " << result.return_value << ")" << std::endl;
        } else if (result.crashed) {
            std::cout << "CRASH: " << result.error_message << std::endl;
        } else if (result.compile_failed) {
            std::cout << "COMPILE FAILED: " << result.error_message << std::endl;
        } else {
            std::cout << "FAIL (returned " << result.return_value << ")" << std::endl;
        }
    }

    return results;
}

void TestRunner::print_summary(const std::vector<TestResult>& results) {
    int passed = 0;
    int failed = 0;
    int crashed = 0;
    int compile_failed = 0;

    std::cout << "\n========================================" << std::endl;
    std::cout << "TEST SUMMARY" << std::endl;
    std::cout << "========================================" << std::endl;

    for (const auto& result : results) {
        if (result.passed) {
            passed++;
        } else if (result.crashed) {
            crashed++;
            std::cout << "CRASH: " << result.test_name << " - " << result.error_message << std::endl;
        } else if (result.compile_failed) {
            compile_failed++;
            std::cout << "COMPILE FAILED: " << result.test_name << std::endl;
            if (!result.error_message.empty()) {
                std::cout << "  " << result.error_message << std::endl;
            }
        } else {
            failed++;
            std::cout << "FAIL: " << result.test_name << " (returned " << result.return_value << ")" << std::endl;
        }
    }

    std::cout << "\nTotal tests: " << results.size() << std::endl;
    std::cout << "Passed: " << passed << std::endl;
    std::cout << "Failed: " << failed << std::endl;
    std::cout << "Crashed: " << crashed << std::endl;
    std::cout << "Compile failed: " << compile_failed << std::endl;
    std::cout << "========================================" << std::endl;
}

} // namespace Fern
