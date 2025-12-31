#pragma once

#include "backend/backend.hpp"
#include <string>
#include <vector>

namespace Fern {

enum class TestStatus { Passed, Failed, CompileFailed, Crashed };

struct TestResult {
    std::string name;
    std::string path;
    TestStatus status = TestStatus::CompileFailed;
    float expected = 0;
    float actual = 0;
    std::string error;
};

struct BenchmarkResult {
    size_t total_lines = 0;
    int iterations = 0;
    int successful = 0;
    double total_seconds = 0;
};

class TestRunner {
public:
    explicit TestRunner(BackendType backend) : m_backend_type(backend) {}

    std::vector<TestResult> run_all_tests(const std::string& test_dir);
    BenchmarkResult run_compile_benchmark(const std::string& test_dir, int iterations = 10);

    static void print_summary(const std::vector<TestResult>& results);
    static void print_benchmark(const BenchmarkResult& result);

private:
    TestResult run_single_test(const std::string& test_file);

    BackendType m_backend_type;
};

} // namespace Fern
