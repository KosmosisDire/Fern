#pragma once

#include "source_location.hpp"
#include <string>
#include <vector>

namespace Fern
{
    struct Diagnostic
    {
        enum class Severity
        {
            Debug,
            Info,
            Warning,
            Error,
            Fatal // should cause compilation to stop
        };

        Severity severity;
        std::string system_name;
        std::string message;
        SourceRange location;

        Diagnostic(Severity sev, const std::string &msg, const SourceRange &loc,
                   const std::string &sys_name)
            : severity(sev), message(msg), location(loc), system_name(sys_name) {}


        std::string severity_string() const
        {
            switch (severity)
            {
            case Severity::Debug:
                return "Debug";
            case Severity::Info:
                return "Info";
            case Severity::Warning:
                return "Warning";
            case Severity::Error:
                return "Error";
            case Severity::Fatal:
                return "Fatal";
            default:
                return "Unknown";
            }
        }

        std::string to_string() const
        {
            return "[" + system_name + "] " + severity_string() +
                   "(" + location.start.to_string() + "): " + message;
        }

        std::string to_string(std::string_view filename) const
        {
            return std::string(filename) + ":" + std::to_string(location.start.line) +
                   ":" + std::to_string(location.start.column) + ": " +
                   severity_string() + ": " + message;
        }
    };

    class DiagnosticSystem
    {
    public:
        DiagnosticSystem(const std::string& system_name);
        DiagnosticSystem(const std::string& system_name, std::vector<Diagnostic> diagnostics);
        void report(const Diagnostic& diag);

        void debug(const std::string& msg, const SourceRange& loc);
        void info(const std::string& msg, const SourceRange& loc);
        void warn(const std::string& msg, const SourceRange& loc);
        void error(const std::string& msg, const SourceRange& loc);
        void fatal(const std::string& msg, const SourceRange& loc);

        void clear_diagnostics();
        const std::vector<Diagnostic>& get_diagnostics() const;
        const bool has_errors() const;
        const size_t error_count() const;
    private:
        std::vector<Diagnostic> diagnostics;
        std::string system_name;
    };
}