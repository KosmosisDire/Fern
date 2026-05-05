#include "test_runner.hpp"

#include <algorithm>
#include <cctype>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <sstream>

#include <fern.hpp>

namespace Fern
{

namespace fs = std::filesystem;

#pragma region File Search

std::vector<std::string> TestRunner::find_test_files(std::string_view folderPath)
{
    std::vector<std::string> files;
    fs::path root{folderPath};
    if (!fs::exists(root) || !fs::is_directory(root))
    {
        return files;
    }

    for (const auto& entry : fs::recursive_directory_iterator(root))
    {
        if (entry.is_regular_file() && entry.path().extension() == ".fn")
        {
            auto p = entry.path().generic_string();
            files.push_back(std::move(p));
        }
    }
    std::sort(files.begin(), files.end());
    return files;
}

#pragma region Header

static std::string_view trim(std::string_view s)
{
    size_t start = 0;
    while (start < s.size() && std::isspace(static_cast<unsigned char>(s[start])))
    {
        ++start;
    }
    size_t end = s.size();
    while (end > start && std::isspace(static_cast<unsigned char>(s[end - 1])))
    {
        --end;
    }
    return s.substr(start, end - start);
}

static bool iequals(std::string_view a, std::string_view b)
{
    if (a.size() != b.size())
    {
        return false;
    }
    for (size_t i = 0; i < a.size(); ++i)
    {
        if (std::tolower(static_cast<unsigned char>(a[i])) != std::tolower(static_cast<unsigned char>(b[i])))
        {
            return false;
        }
    }
    return true;
}

static bool starts_with_icase(std::string_view s, std::string_view prefix)
{
    if (s.size() < prefix.size())
    {
        return false;
    }
    for (size_t i = 0; i < prefix.size(); ++i)
    {
        if (std::tolower(static_cast<unsigned char>(s[i])) != std::tolower(static_cast<unsigned char>(prefix[i])))
        {
            return false;
        }
    }
    return true;
}

TestHeader TestRunner::parse_header(std::string_view source)
{
    TestHeader header;

    size_t open = source.find("---");
    if (open == std::string_view::npos)
    {
        return header;
    }
    size_t bodyStart = open + 3;
    size_t close = source.find("---", bodyStart);
    if (close == std::string_view::npos)
    {
        return header;
    }

    std::string_view body = source.substr(bodyStart, close - bodyStart);

    size_t pos = 0;
    while (pos < body.size())
    {
        size_t lineEnd = body.find('\n', pos);
        if (lineEnd == std::string_view::npos)
        {
            lineEnd = body.size();
        }
        std::string_view line = trim(body.substr(pos, lineEnd - pos));
        pos = lineEnd + 1;

        size_t colon = line.find(':');
        if (colon == std::string_view::npos)
        {
            continue;
        }
        std::string_view key = trim(line.substr(0, colon));
        std::string_view value = trim(line.substr(colon + 1));

        if (iequals(key, "Description"))
        {
            header.description.assign(value);
        }
        else if (iequals(key, "Expected"))
        {
            header.expected.assign(value);
        }
    }

    if (starts_with_icase(header.expected, "compile error"))
    {
        header.expectsError = true;
    }
    else if (starts_with_icase(header.expected, "compile warning"))
    {
        header.expectsWarning = true;
    }

    return header;
}

#pragma region Execution

TestResult TestRunner::run_test(const std::string& path, const std::vector<std::string>& includes)
{
    TestResult result;
    result.path = path;

    std::ifstream file{path};
    if (!file)
    {
        result.actual = "could not open file";
        return result;
    }

    std::ostringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();

    result.header = parse_header(source);

    Compilation compilation;
    for (const auto& include : includes)
    {
        compilation.add_file(include);
    }
    compilation.add_source(std::move(source), path);
    compilation.compile();

    bool hasError = false;
    bool hasWarning = false;
    for (const auto& diag : compilation.diag.get_diagnostics())
    {
        auto filePath = diag.location.fileId >= 0 && diag.location.fileId < (int)compilation.get_units().size()
                            ? compilation.get_units()[diag.location.fileId]->sourceFile->path()
                            : std::string_view{path};
        result.diagnostics.push_back(diag.format(filePath));

        if (diag.severity == Diagnostic::Severity::Error)
        {
            hasError = true;
        }
        else if (diag.severity == Diagnostic::Severity::Warning)
        {
            hasWarning = true;
        }
    }

    if (hasError)
    {
        result.actual = "compile error";
    }
    else if (hasWarning)
    {
        result.actual = "compile warning";
    }
    else
    {
        result.actual = "success";
    }

    if (result.header.expectsError)
    {
        result.passed = hasError;
    }
    else if (result.header.expectsWarning)
    {
        result.passed = hasWarning && !hasError;
    }
    else
    {
        result.passed = !hasError;
    }

    return result;
}

#pragma region Reporting

void TestRunner::print_failure(const TestResult& result)
{
    std::cout << std::format("FAIL {}\n", result.path);
    if (!result.header.description.empty())
    {
        std::cout << std::format("  Description: {}\n", result.header.description);
    }
    if (!result.header.expected.empty())
    {
        std::cout << std::format("  Expected:    {}\n", result.header.expected);
    }
    std::cout << std::format("  Actual:      {}\n", result.actual);
    if (!result.diagnostics.empty())
    {
        std::cout << "  Diagnostics:\n";
        for (const auto& d : result.diagnostics)
        {
            std::cout << std::format("    {}\n", d);
        }
    }
    std::cout << "\n";
}

void TestRunner::print_summary(int passed, int failed)
{
    int total = passed + failed;
    std::cout << std::format("\n{} passed, {} failed, {} total\n", passed, failed, total);
}

#pragma region Entry

int TestRunner::run(std::string_view folderPath, const std::vector<std::string>& includes)
{
    auto files = find_test_files(folderPath);
    if (files.empty())
    {
        std::cout << std::format("No .fn test files found under {}\n", folderPath);
        return 1;
    }

    int passed = 0;
    int failed = 0;
    for (const auto& path : files)
    {
        TestResult result = run_test(path, includes);
        if (result.passed)
        {
            ++passed;
        }
        else
        {
            ++failed;
            print_failure(result);
        }
    }

    print_summary(passed, failed);
    return failed == 0 ? 0 : 1;
}

}
