#pragma once

#include <string>
#include <string_view>
#include <vector>

namespace Fern
{

struct TestHeader
{
    std::string description;
    std::string expected;
    std::vector<std::string> expectedCodes;
    bool expectsCodes = false;
};

struct TestResult
{
    std::string path;
    TestHeader header;
    bool passed = false;
    std::string actual;
    std::vector<std::string> actualCodes;
    std::vector<std::string> diagnostics;
};

class TestRunner
{
public:
    int run(std::string_view folderPath, const std::vector<std::string>& includes);

private:
    static std::vector<std::string> find_test_files(std::string_view folderPath);
    static TestHeader parse_header(std::string_view source);
    static TestResult run_test(const std::string& path, const std::vector<std::string>& includes);
    static void print_failure(const TestResult& result);
    static void print_summary(int passed, int failed);
};

}
