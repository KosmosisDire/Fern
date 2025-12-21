#include "test_runner.hpp"
#include "compiler.hpp"
#include "common/logger.hpp"
#include <filesystem>
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <cmath>
#include <chrono>
#include <iomanip>

namespace fs = std::filesystem;

namespace Fern {

static std::string read_file(const std::string& path) {
    std::ifstream file(path);
    if (!file) throw std::runtime_error("Could not open: " + path);
    std::stringstream buf;
    buf << file.rdbuf();
    return buf.str();
}

static size_t count_lines(const std::string& s) {
    return s.empty() ? 0 : 1 + std::count(s.begin(), s.end(), '\n');
}

static bool extract_expected(const std::string& source, float& out) {
    auto pos = source.find("-- Expected:");
    if (pos == std::string::npos) return false;
    try {
        out = std::stof(source.substr(pos + 12));
        return true;
    } catch (...) {
        return false;
    }
}

static std::vector<std::string> collect_files(const std::string& dir) {
    std::vector<std::string> files;
    for (const auto& e : fs::recursive_directory_iterator(dir)) {
        if (e.is_regular_file() && e.path().extension() == ".fn")
            files.push_back(e.path().string());
    }
    std::sort(files.begin(), files.end());
    return files;
}

static std::vector<SourceFile> load_stdlib() {
    std::vector<SourceFile> files;
    for (const auto& path : {"runtime/string.fn", "runtime/stringtools.fn", "runtime/file.fn"}) {
        files.push_back({path, read_file(path)});
    }
    return files;
}

TestResult TestRunner::run_single_test(const std::string& test_file) {
    TestResult r;
    r.name = fs::path(test_file).filename().string();
    r.path = test_file;

    try {
        std::string source = read_file(test_file);
        if (!extract_expected(source, r.expected)) {
            r.error = "No '-- Expected:' comment";
            return r;
        }

        Compiler compiler;
        compiler.set_print_ast(false);
        compiler.set_print_symbols(false);
        compiler.set_print_flir(false);

        std::vector<SourceFile> sources = {{test_file, source}};
        for (auto& f : load_stdlib()) sources.push_back(f);

        auto result = compiler.compile(sources);
        if (!result || !result->is_valid()) {
            if (result) {
                for (const auto& d : result->get_diagnostics())
                    r.error += d.message + "; ";
            }
            return r;
        }

        auto jit = result->execute_jit<float>("Main");
        if (jit) {
            r.actual = *jit;
            r.status = (std::abs(r.actual - r.expected) < 1e-5f)
                ? TestStatus::Passed : TestStatus::Failed;
        } else {
            r.status = TestStatus::Crashed;
            r.error = "JIT failed";
        }
    } catch (const std::exception& e) {
        r.status = TestStatus::Crashed;
        r.error = e.what();
    }
    return r;
}

std::vector<TestResult> TestRunner::run_all_tests(const std::string& test_dir) {
    auto files = collect_files(test_dir);
    if (files.empty()) {
        std::cerr << "No test files in " << test_dir << std::endl;
        return {};
    }

    std::cout << "Running " << files.size() << " tests...\n" << std::endl;

    Logger::get_instance().set_console_level(LogLevel::NONE);

    std::vector<TestResult> results;
    for (size_t i = 0; i < files.size(); i++) {
        auto rel = fs::relative(files[i], test_dir).string();
        std::cout << "[" << i+1 << "/" << files.size() << "] " << rel << "... " << std::flush;

        auto r = run_single_test(files[i]);
        results.push_back(r);

        switch (r.status) {
            case TestStatus::Passed: std::cout << "PASS\n"; break;
            case TestStatus::Failed: std::cout << "FAIL (" << r.expected << " != " << r.actual << ")\n"; break;
            case TestStatus::CompileFailed: std::cout << "COMPILE: " << r.error << "\n"; break;
            case TestStatus::Crashed: std::cout << "CRASH: " << r.error << "\n"; break;
        }
    }

    Logger::get_instance().set_console_level(LogLevel::INFO);
    return results;
}

BenchmarkResult TestRunner::run_compile_benchmark(const std::string& test_dir, int iterations) {
    auto files = collect_files(test_dir);
    if (files.empty()) {
        std::cerr << "No test files in " << test_dir << std::endl;
        return {};
    }

    // Pre-load all sources
    auto stdlib = load_stdlib();
    size_t stdlib_lines = 0;
    for (auto& f : stdlib) stdlib_lines += count_lines(f.source);

    std::vector<std::pair<std::string, std::string>> test_sources;
    size_t test_lines = 0;
    for (const auto& path : files) {
        auto src = read_file(path);
        test_lines += count_lines(src);
        test_sources.push_back({path, src});
    }

    size_t total_lines = (stdlib_lines + test_lines) * iterations;
    std::cout << "Benchmarking " << files.size() << " files x " << iterations << " iterations\n";

    Logger::get_instance().set_console_level(LogLevel::NONE);

    int ok = 0;
    auto start = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < iterations; i++) {
        std::cout << "\r" << (i+1) << "/" << iterations << std::flush;
        for (const auto& [path, src] : test_sources) {
            std::vector<SourceFile> sources = {{path, src}};
            for (auto& f : stdlib) sources.push_back(f);

            Compiler c;
            c.set_print_ast(false);
            c.set_print_symbols(false);
            c.set_print_flir(false);
            if (auto r = c.compile(sources); r && r->is_valid()) ok++;
        }
    }
    auto end = std::chrono::high_resolution_clock::now();
    double secs = std::chrono::duration<double>(end - start).count();

    Logger::get_instance().set_console_level(LogLevel::INFO);
    std::cout << "\r";

    int total_compiles = iterations * (int)files.size();
    return {total_lines, total_compiles, ok, secs};
}

void TestRunner::print_summary(const std::vector<TestResult>& results) {
    int pass = 0, fail = 0, crash = 0, compile = 0;
    for (const auto& r : results) {
        switch (r.status) {
            case TestStatus::Passed: pass++; break;
            case TestStatus::Failed: fail++; break;
            case TestStatus::Crashed: crash++; break;
            case TestStatus::CompileFailed: compile++; break;
        }
    }
    std::cout << "\nTotal: " << results.size() << " | Pass: " << pass
              << " | Fail: " << fail << " | Crash: " << crash
              << " | Compile: " << compile << std::endl;
}

void TestRunner::print_benchmark(const BenchmarkResult& r) {
    double avg_ms = r.iterations > 0 ? (r.total_seconds * 1000.0) / r.iterations : 0;
    double lps = r.total_seconds > 0 ? (double(r.total_lines) * r.successful) / r.total_seconds : 0;
    std::cout << std::fixed
              << r.successful << "/" << r.iterations << " succeeded | "
              << std::setprecision(0) << avg_ms << "ms/compile | "
              << lps << " lines/sec" << std::endl;
}

} // namespace Fern
