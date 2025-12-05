#pragma once

#include <string>
#include <vector>

namespace Fern {

struct TestResult {
    std::string test_name;
    std::string test_path;
    bool passed;
    bool crashed;
    bool compile_failed;
    float expected_value;
    float actual_value;
    std::string error_message;

    TestResult(const std::string& name, const std::string& path = "")
        : test_name(name), test_path(path), passed(false), crashed(false),
          compile_failed(false), expected_value(0.0f), actual_value(0.0f) {}
};

class TestRunner {
public:
    TestRunner();

    // Run all tests in the specified directory (recursively)
    std::vector<TestResult> run_all_tests(const std::string& test_dir);

    // Print summary of test results
    void print_summary(const std::vector<TestResult>& results);

private:
    TestResult run_single_test(const std::string& test_file);
    void collect_test_files(const std::string& dir, std::vector<std::string>& test_files);
};

} // namespace Fern
