#include "common/logger.hpp"
#include <filesystem>

namespace Fern {

std::unique_ptr<Logger> Logger::instance = nullptr;
std::mutex Logger::instance_mutex;

Logger& Logger::get_instance() {
    std::lock_guard<std::mutex> lock(instance_mutex);
    if (!instance) {
        instance = std::unique_ptr<Logger>(new Logger());
    }
    return *instance;
}

bool Logger::initialize(const std::string& log_file_path) {
    std::lock_guard<std::mutex> lock(log_mutex_);
    
    if (!log_file_path.empty()) {
        // Create directory if it doesn't exist
        auto parent_path = std::filesystem::path(log_file_path).parent_path();
        if (!parent_path.empty()) {
            std::filesystem::create_directories(parent_path);
        }
        
        // Open log file
        log_file_.open(log_file_path, std::ios::out | std::ios::app);
        if (!log_file_.is_open()) {
            std::cerr << "Failed to open log file: " << log_file_path << std::endl;
            return false;
        }
        
        // Log initialization
        log_file_ << "\n" << std::string(80, '=') << "\n";
        log_file_ << "LOGGER INITIALIZED AT " << get_timestamp() << "\n";
        log_file_ << std::string(80, '=') << "\n\n";
        log_file_.flush();
    }
    
    initialized_ = true;
    return true;
}

std::string Logger::get_timestamp() const {
    auto now = std::chrono::system_clock::now();
    auto time_t = std::chrono::system_clock::to_time_t(now);
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        now.time_since_epoch()) % 1000;
    
    std::stringstream ss;
#ifdef _WIN32
    // Use localtime_s on Windows to avoid deprecation warning
    std::tm tm_buf;
    localtime_s(&tm_buf, &time_t);
    ss << std::put_time(&tm_buf, "%Y-%m-%d %H:%M:%S");
#else
    // Use localtime_r on POSIX systems
    std::tm tm_buf;
    localtime_r(&time_t, &tm_buf);
    ss << std::put_time(&tm_buf, "%Y-%m-%d %H:%M:%S");
#endif
    ss << "." << std::setfill('0') << std::setw(3) << ms.count();
    return ss.str();
}

std::string Logger::level_to_string(LogLevel level) const {
    switch (level) {
        case LogLevel::TRACE: return "TRACE";
        case LogLevel::DEBUG: return "DEBUG";
        case LogLevel::INFO:  return "INFO ";
        case LogLevel::WARN:  return "WARN ";
        case LogLevel::ERROR:   return "ERROR";
        case LogLevel::FATAL: return "FATAL";
        default: return "UNKNOWN";
    }
}

std::string Logger::category_to_string(LogCategory category) const {
    switch (category) {
        case LogCategory::GENERAL:  return "GENERAL";
        case LogCategory::COMPILER: return "COMPILER";
        case LogCategory::CODEGEN:  return "CODEGEN";
        case LogCategory::JIT:      return "JIT";
        case LogCategory::TEST:     return "TEST";
        case LogCategory::AST:      return "AST";
        case LogCategory::SEMANTIC: return "SEMANTIC";
        case LogCategory::PARSER:   return "PARSER";
        case LogCategory::MEMORY:   return "MEMORY";
        default: return "UNKNOWN";
    }
}

std::string Logger::get_color_code(LogLevel level) const {
    if (!colors_enabled_) return "";
    
    switch (level) {
        case LogLevel::TRACE: return Colors::WHITE;
        case LogLevel::DEBUG: return Colors::CYAN;
        case LogLevel::INFO:  return Colors::GREEN;
        case LogLevel::WARN:  return Colors::YELLOW;
        case LogLevel::ERROR:   return Colors::RED;
        case LogLevel::FATAL: return Colors::BRIGHT_RED + Colors::BOLD;
        default: return "";
    }
}

std::string Logger::get_category_color(LogCategory category) const {
    if (!colors_enabled_) return "";
    
    switch (category) {
        case LogCategory::GENERAL:  return Colors::WHITE;
        case LogCategory::COMPILER: return Colors::BLUE;
        case LogCategory::CODEGEN:  return Colors::MAGENTA;
        case LogCategory::JIT:      return Colors::GRAY;
        case LogCategory::TEST:     return Colors::BRIGHT_CYAN;
        case LogCategory::AST:      return Colors::BRIGHT_BLUE;
        case LogCategory::SEMANTIC: return Colors::BRIGHT_MAGENTA;
        case LogCategory::PARSER:   return Colors::BRIGHT_GREEN;
        case LogCategory::MEMORY:   return Colors::BRIGHT_YELLOW;
        default: return "";
    }
}

std::string Logger::get_reset_color() const {
    if (!colors_enabled_) return "";
    return Colors::RESET;
}

bool Logger::should_log(LogLevel level, LogCategory category, bool to_console) const {
    LogLevel min_level = to_console ? min_console_level_ : min_file_level_;
    return level >= min_level && has_category(enabled_categories_, category);
}

void Logger::log(LogLevel level, LogCategory category, const std::string& message) {
    std::lock_guard<std::mutex> lock(log_mutex_);
    
    std::string timestamp = get_timestamp();
    std::string level_str = level_to_string(level);
    std::string category_str = category_to_string(category);
    std::string context_str = current_context_.empty() ? "" : "[" + current_context_ + "] ";
    
    // If string capture mode is active, capture to string instead of normal output
    if (string_capture_mode_ && should_log(level, category, true)) {
        captured_output_ << message << "\n";
        return; // Don't output to console/file during capture
    }
    
    // Log to file if initialized and level is sufficient
    if (initialized_ && should_log(level, category, false)) {
        log_file_ << timestamp << " " << level_str << " [" << category_str << "] " 
                  << context_str << message << "\n";
        log_file_.flush();
    }
    
    // Log to console if level is sufficient
    if (should_log(level, category, true)) {
        std::string level_color = get_color_code(level);
        std::string category_color = get_category_color(category);
        std::string reset = get_reset_color();
        
        std::ostream& output = (level >= LogLevel::ERROR) ? std::cerr : std::cout;
        
        if (test_mode_) {
            // Simplified output for tests
            output << level_color << message << reset << std::endl;
        } else {
            // Full format for normal operation
            output << level_color << level_str << reset << " " 
                   << category_color << "[" << category_str << "]" << reset << " "
                   << context_str << message << std::endl;
        }
    }
}

// Convenience methods for different log levels
void Logger::trace(const std::string& message, LogCategory category) {
    log(LogLevel::TRACE, category, message);
}

void Logger::debug(const std::string& message, LogCategory category) {
    log(LogLevel::DEBUG, category, message);
}

void Logger::info(const std::string& message, LogCategory category) {
    log(LogLevel::INFO, category, message);
}

void Logger::warn(const std::string& message, LogCategory category) {
    log(LogLevel::WARN, category, message);
}

void Logger::error(const std::string& message, LogCategory category) {
    log(LogLevel::ERROR, category, message);
}

void Logger::fatal(const std::string& message, LogCategory category) {
    log(LogLevel::FATAL, category, message);
}

// Special formatting helpers
void Logger::header(const std::string& title, LogCategory category)
{
    if (!should_log(LogLevel::INFO, category, true)) return;
    std::string separator_line(10, '=');
    
    std::cout << "\n" << Colors::BOLD + separator_line + " " + title + " " + separator_line + Colors::RESET << "\n\n";
}

void Logger::subheader(const std::string& title, LogCategory category)
{
    if (!should_log(LogLevel::INFO, category, true)) return;
    std::string separator_line(5, '-');
    
    std::cout << "\n\t\t" << Colors::DIM << separator_line + " " + title + " " + separator_line << Colors::RESET << "\n\n";
}

void Logger::separator(char character, int length, LogCategory category)
{
    if (!should_log(LogLevel::INFO, category, true)) return;
    length += 4 + category_to_string(category).length() + 2;
    std::cout << Colors::DIM << std::string(length, character) << Colors::RESET << "\n";
}

void Logger::blank_line()
{
    std::lock_guard<std::mutex> lock(log_mutex_);
    std::cout << std::endl;
    if (initialized_) {
        log_file_ << "\n";
        log_file_.flush();
    }
}

// Test-specific helpers
void Logger::test_suite_start(const std::string& suite_name) {
    std::lock_guard<std::mutex> lock(log_mutex_);
    bool old_test_mode = test_mode_;
    test_mode_ = true;
    
    std::string color = colors_enabled_ ? Colors::BLUE + Colors::BOLD : "";
    std::string reset = get_reset_color();
    
    std::cout << color << "Running test suite: " << suite_name << reset << std::endl;
    
    test_mode_ = old_test_mode;
}

void Logger::test_suite_end(const std::string& suite_name, int passed, int total) {
    std::lock_guard<std::mutex> lock(log_mutex_);
    bool old_test_mode = test_mode_;
    test_mode_ = true;
    
    std::string color = (passed == total) ? Colors::GREEN : Colors::RED;
    std::string reset = get_reset_color();
    
    if (colors_enabled_) {
        std::cout << color << "Results: " << passed << "/" << total << " tests passed" << reset << std::endl;
    } else {
        std::cout << "Results: " << passed << "/" << total << " tests passed" << std::endl;
    }
    
    test_mode_ = old_test_mode;
}

void Logger::test_result(const std::string& test_name, bool passed, const std::string& message) {
    std::lock_guard<std::mutex> lock(log_mutex_);
    bool old_test_mode = test_mode_;
    test_mode_ = true;
    
    std::string status_color = passed ? Colors::GREEN + Colors::BOLD : Colors::RED + Colors::BOLD;
    std::string status_text = passed ? "✓ PASS" : "✗ FAIL";
    std::string reset = get_reset_color();
    
    if (colors_enabled_) {
        std::cout << Colors::DIM << "Running: " << reset << test_name << " ... "
                  << status_color << status_text << reset;
        if (!passed && !message.empty()) {
            std::cout << ": " << Colors::RED << message << reset;
        }
        std::cout << std::endl;
    } else {
        std::cout << "Running: " << test_name << " ... " << status_text;
        if (!passed && !message.empty()) {
            std::cout << ": " << message;
        }
        std::cout << std::endl;
    }
    
    test_mode_ = old_test_mode;
}

// Progress indicators
void Logger::progress(const std::string& operation, LogCategory category) {
    std::string progress_indicator = colors_enabled_ ? Colors::CYAN + "🔄 " + Colors::RESET : "-> ";
    log(LogLevel::INFO, category, progress_indicator + operation + "...");
}

void Logger::step(const std::string& step_name, LogCategory category) {
    std::string step_indicator = colors_enabled_ ? Colors::BLUE + "  ▶ " + Colors::RESET : "  * ";
    log(LogLevel::INFO, category, step_indicator + step_name);
}

void Logger::flush() {
    std::lock_guard<std::mutex> lock(log_mutex_);
    if (initialized_) {
        log_file_.flush();
    }
    std::cout.flush();
    std::cerr.flush();
}

// String capture methods
void Logger::begin_string_capture() {
    std::lock_guard<std::mutex> lock(log_mutex_);
    string_capture_mode_ = true;
    captured_output_.str("");  // Clear any previous capture
    captured_output_.clear();  // Clear error flags
}

std::string Logger::end_string_capture() {
    std::lock_guard<std::mutex> lock(log_mutex_);
    string_capture_mode_ = false;
    return captured_output_.str();
}

void Logger::shutdown() {
    std::lock_guard<std::mutex> lock(log_mutex_);
    if (initialized_ && log_file_.is_open()) {
        log_file_ << "\n" << std::string(80, '=') << "\n";
        log_file_ << "LOGGER EXPLICIT SHUTDOWN AT " << get_timestamp() << "\n";
        log_file_ << std::string(80, '=') << "\n\n";
        log_file_.flush();
        log_file_.close();
        initialized_ = false;
    }
}

Logger::~Logger() {
    // If shutdown() wasn't called explicitly, do cleanup
    if (initialized_ && log_file_.is_open()) {
        log_file_ << "\n" << std::string(80, '=') << "\n";
        log_file_ << "LOGGER DESTRUCTOR SHUTDOWN AT " << get_timestamp() << "\n";
        log_file_ << std::string(80, '=') << "\n\n";
        log_file_.close();
    }
}

} // namespace Fern
