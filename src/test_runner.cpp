#include "test_runner.hpp"
#include "compiler.hpp"
#include "common/logger.hpp"
#include <filesystem>
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <cmath>

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

static bool extract_expected_value(const std::string& source, float& out_value) {
    // Look for "-- Expected: value" pattern
    size_t pos = source.find("-- Expected:");
    if (pos == std::string::npos) {
        return false;
    }

    try {
        std::string value_str = source.substr(pos + 12); // Skip "-- Expected:"
        // Find the first number (may have leading spaces)
        size_t start = value_str.find_first_not_of(" \t");
        if (start == std::string::npos) {
            return false;
        }

        // Extract float value (including negative numbers and decimals)
        size_t end = start;
        if (value_str[end] == '-') {
            end++;
        }
        while (end < value_str.length() &&
               (std::isdigit(value_str[end]) || value_str[end] == '.')) {
            end++;
        }

        out_value = std::stof(value_str.substr(start, end - start));
        return true;
    } catch (...) {
        return false;
    }
}

static bool values_equal(float a, float b, float epsilon = 1e-5f) {
    return std::abs(a - b) < epsilon;
}

TestRunner::TestRunner() {
}

TestResult TestRunner::run_single_test(const std::string& test_file) {
    fs::path path(test_file);
    TestResult result(path.filename().string(), test_file);

    try {
        // Read the test file
        std::string source = read_file(test_file);

        // Extract expected value from comments
        if (!extract_expected_value(source, result.expected_value)) {
            result.compile_failed = true;
            result.error_message = "No expected value found (use '-- Expected: value')";
            return result;
        }

        // Create a compiler instance for this test
        Compiler compiler;
        compiler.set_print_ast(false);
        compiler.set_print_symbols(false);
        compiler.set_print_hlir(false);

        // Compile the test file with stdlib
        std::vector<SourceFile> source_files = {{test_file, source}};

        std::vector<std::string> stdlib_files = {
            "runtime/string.fn",
            "runtime/stringtools.fn",
            "runtime/file.fn",
        };

        for (const auto& lib_file : stdlib_files) {
            std::string lib_source = read_file(lib_file);
            source_files.push_back({lib_file, lib_source});
        }

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

        // Execute the test
        auto jit_result = compile_result->execute_jit<float>("Main");

        if (jit_result.has_value()) {
            result.actual_value = jit_result.value();
            result.passed = values_equal(result.actual_value, result.expected_value);
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

void TestRunner::collect_test_files(const std::string& dir, std::vector<std::string>& test_files) {
    try {
        for (const auto& entry : fs::recursive_directory_iterator(dir)) {
            if (entry.is_regular_file() && entry.path().extension() == ".fn") {
                test_files.push_back(entry.path().string());
            }
        }
    } catch (const std::exception& e) {
        std::cerr << "Error scanning test directory: " << e.what() << std::endl;
    }
}

std::vector<TestResult> TestRunner::run_all_tests(const std::string& test_dir) {
    std::vector<TestResult> results;
    std::vector<std::string> test_files;

    // Collect all .fn files recursively
    collect_test_files(test_dir, test_files);

    // Sort test files by name for consistent ordering
    std::sort(test_files.begin(), test_files.end());

    if (test_files.empty()) {
        std::cerr << "No test files found in " << test_dir << std::endl;
        return results;
    }

    std::cout << "Running " << test_files.size() << " tests...\n" << std::endl;

    // Disable logging during test execution to only show test runner output
    Logger& logger = Logger::get_instance();
    logger.set_console_level(LogLevel::NONE);

    // Run each test
    int test_num = 0;
    for (const auto& test_file : test_files) {
        test_num++;

        // Create relative path for display
        fs::path rel_path = fs::relative(test_file, test_dir);
        std::cout << "[" << test_num << "/" << test_files.size() << "] " << rel_path.string() << "... " << std::flush;

        TestResult result = run_single_test(test_file);
        results.push_back(result);

        // Print immediate result
        if (result.passed) {
            std::cout << "PASS" << std::endl;
        } else if (result.crashed) {
            std::cout << "CRASH: " << result.error_message << std::endl;
        } else if (result.compile_failed) {
            std::cout << "COMPILE FAILED: " << result.error_message << std::endl;
        } else {
            std::cout << "FAIL (expected " << result.expected_value
                      << ", got " << result.actual_value << ")" << std::endl;
        }
    }

    // Restore logging to normal level (INFO is default)
    logger.set_console_level(LogLevel::INFO);

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
            std::cout << "CRASH: " << result.test_name << std::endl;
            std::cout << "  " << result.error_message << std::endl;
        } else if (result.compile_failed) {
            compile_failed++;
            std::cout << "COMPILE FAILED: " << result.test_name << std::endl;
            if (!result.error_message.empty()) {
                std::cout << "  " << result.error_message << std::endl;
            }
        } else {
            failed++;
            std::cout << "FAIL: " << result.test_name << std::endl;
            std::cout << "  Expected: " << result.expected_value << ", Got: " << result.actual_value << std::endl;
        }
    }

    std::cout << "\nTotal: " << results.size() << " | Passed: " << passed
              << " | Failed: " << failed << " | Crashed: " << crashed
              << " | Compile Failed: " << compile_failed << std::endl;
    std::cout << "========================================" << std::endl;
}

} // namespace Fern