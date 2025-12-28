#pragma once

#include <iostream>
#include <fstream>
#include <string>
#include <memory>
#include <mutex>
#include <chrono>
#include <iomanip>
#include <sstream>
#include <vector>
#include <functional>

namespace Fern
{
    enum class LogLevel
    {
        TRACE = 1,
        DEBUG = 2,
        INFO = 3,
        WARN = 4,
        ERROR = 5,
        FATAL = 6,
        NONE = 7
    };

    // Flag-style enum for categories (can be combined with | operator)
    enum class LogCategory : uint32_t
    {
        NONE = 0,
        GENERAL = 1 << 0,  // 0x001
        COMPILER = 1 << 1, // 0x002
        CODEGEN = 1 << 2,  // 0x004
        JIT = 1 << 3,      // 0x008
        TEST = 1 << 4,     // 0x010
        AST = 1 << 5,      // 0x020
        SEMANTIC = 1 << 6, // 0x040
        PARSER = 1 << 7,   // 0x080
        MEMORY = 1 << 8,   // 0x100
        ALL = 0xFFFFFFFF   // All categories enabled
    };

    // Bitwise operators for LogCategory
    inline LogCategory operator|(LogCategory a, LogCategory b)
    {
        return static_cast<LogCategory>(static_cast<uint32_t>(a) | static_cast<uint32_t>(b));
    }

    inline LogCategory operator&(LogCategory a, LogCategory b)
    {
        return static_cast<LogCategory>(static_cast<uint32_t>(a) & static_cast<uint32_t>(b));
    }

    inline LogCategory operator~(LogCategory a)
    {
        return static_cast<LogCategory>(~static_cast<uint32_t>(a));
    }

    inline bool has_category(LogCategory flags, LogCategory category)
    {
        return (flags & category) != LogCategory::NONE;
    }

    // ANSI color codes for consistent formatting
    namespace Colors
    {
        const std::string RESET = "\033[0m";
        const std::string BOLD = "\033[1m";
        const std::string DIM = "\033[2m";
        const std::string RED = "\033[31m";
        const std::string GREEN = "\033[32m";
        const std::string YELLOW = "\033[33m";
        const std::string BLUE = "\033[34m";
        const std::string MAGENTA = "\033[35m";
        const std::string CYAN = "\033[36m";
        const std::string WHITE = "\033[37m";
        const std::string GRAY = "\033[90m";
        const std::string BRIGHT_RED = "\033[91m";
        const std::string BRIGHT_GREEN = "\033[92m";
        const std::string BRIGHT_YELLOW = "\033[93m";
        const std::string BRIGHT_BLUE = "\033[94m";
        const std::string BRIGHT_MAGENTA = "\033[95m";
        const std::string BRIGHT_CYAN = "\033[96m";
    }

    class Logger
    {
    private:
        static std::unique_ptr<Logger> instance;
        static std::mutex instance_mutex;

        std::ofstream log_file_;
        std::mutex log_mutex_;
        LogLevel min_console_level_;
        LogLevel min_file_level_;
        LogCategory enabled_categories_;
        bool initialized_;
        bool colors_enabled_;
        bool test_mode_;
        std::string current_context_;

        // String capture mode
        bool string_capture_mode_;
        std::stringstream captured_output_;

        Logger() : min_console_level_(LogLevel::INFO), min_file_level_(LogLevel::TRACE),
                   enabled_categories_(LogCategory::ALL), initialized_(false),
                   colors_enabled_(true), test_mode_(false), string_capture_mode_(false) {}

        std::string get_timestamp() const;
        std::string level_to_string(LogLevel level) const;
        std::string category_to_string(LogCategory category) const;
        std::string get_color_code(LogLevel level) const;
        std::string get_category_color(LogCategory category) const;
        std::string get_reset_color() const;
        bool should_log(LogLevel level, LogCategory category, bool to_console) const;

    public:
        static Logger &get_instance();

        // Initialize the logger with a log file path
        bool initialize(const std::string &log_file_path = "");

        // Configuration methods
        void set_console_level(LogLevel level) { min_console_level_ = level; }
        void set_file_level(LogLevel level) { min_file_level_ = level; }
        void set_enabled_categories(LogCategory categories) { enabled_categories_ = categories; }
        void enable_category(LogCategory category) { enabled_categories_ = enabled_categories_ | category; }
        void disable_category(LogCategory category) { enabled_categories_ = enabled_categories_ & ~category; }
        void set_colors_enabled(bool enabled) { colors_enabled_ = enabled; }
        void set_test_mode(bool test_mode) { test_mode_ = test_mode; }
        void set_context(const std::string &context) { current_context_ = context; }

        // Core logging function
        void log(LogLevel level, LogCategory category, const std::string &message);
        void log(LogLevel level, const std::string &message) { log(level, LogCategory::GENERAL, message); }

        // Convenience methods for different log levels
        void trace(const std::string &message, LogCategory category = LogCategory::GENERAL);
        void debug(const std::string &message, LogCategory category = LogCategory::GENERAL);
        void info(const std::string &message, LogCategory category = LogCategory::GENERAL);
        void warn(const std::string &message, LogCategory category = LogCategory::GENERAL);
        void error(const std::string &message, LogCategory category = LogCategory::GENERAL);
        void fatal(const std::string &message, LogCategory category = LogCategory::GENERAL);

        // Special formatting helpers
        void header(const std::string &title, LogCategory category = LogCategory::GENERAL);
        void subheader(const std::string &title, LogCategory category = LogCategory::GENERAL);
        void separator(char character = '=', int length = 80, LogCategory category = LogCategory::GENERAL);
        void blank_line();

        // Test-specific helpers
        void test_suite_start(const std::string &suite_name);
        void test_suite_end(const std::string &suite_name, int passed, int total);
        void test_result(const std::string &test_name, bool passed, const std::string &message = "");

        // Progress indicators
        void progress(const std::string &operation, LogCategory category = LogCategory::GENERAL);
        void step(const std::string &step_name, LogCategory category = LogCategory::GENERAL);

        // String capture mode for testing/debugging
        void begin_string_capture();
        std::string end_string_capture();
        bool is_string_capture_active() const { return string_capture_mode_; }

        // Flush all outputs
        void flush();

        // Explicit shutdown method for graceful cleanup
        void shutdown();

        // Cleanup
        ~Logger();
    };

// Convenience macros for easier logging
#define LOG_TRACE(msg, ...) Logger::get_instance().trace(msg, ##__VA_ARGS__)
#define LOG_DEBUG(msg, ...) Logger::get_instance().debug(msg, ##__VA_ARGS__)
#define LOG_INFO(msg, ...) Logger::get_instance().info(msg, ##__VA_ARGS__)
#define LOG_WARN(msg, ...) Logger::get_instance().warn(msg, ##__VA_ARGS__)
#define LOG_ERROR(msg, ...) Logger::get_instance().error(msg, ##__VA_ARGS__)
#define LOG_FATAL(msg, ...) Logger::get_instance().fatal(msg, ##__VA_ARGS__)

// Formatting helpers
#define LOG_HEADER(title, ...) Logger::get_instance().header(title, ##__VA_ARGS__)
#define LOG_SUBHEADER(title, ...) Logger::get_instance().subheader(title, ##__VA_ARGS__)
#define LOG_SEPARATOR(...) Logger::get_instance().separator(__VA_ARGS__)
#define LOG_PROGRESS(operation, ...) Logger::get_instance().progress(operation, ##__VA_ARGS__)
#define LOG_BLANK() Logger::get_instance().blank_line()

// test-specific macros
#define LOG_TEST_SUITE_START(suite_name) Logger::get_instance().test_suite_start(suite_name)
#define LOG_TEST_SUITE_END(suite_name, passed, total) Logger::get_instance().test_suite_end(suite_name, passed, total)
#define LOG_TEST_RESULT(test_name, passed, message) Logger::get_instance().test_result(test_name, passed, message)

// String capture macros
#define LOG_BEGIN_CAPTURE() Logger::get_instance().begin_string_capture()
#define LOG_END_CAPTURE() Logger::get_instance().end_string_capture()

} // namespace Fern
