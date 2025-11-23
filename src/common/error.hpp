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


        std::string to_string() const
        {
            std::string sev_str;
            switch (severity)
            {
            case Severity::Debug:
                sev_str = "Debug";
                break;
            case Severity::Info:
                sev_str = "Info";
                break;
            case Severity::Warning:
                sev_str = "Warning";
                break;
            case Severity::Error:
                sev_str = "Error";
                break;
            case Severity::Fatal:
                sev_str = "Fatal";
                break;
            }

            return "[" + system_name + "] " + sev_str + "(" + location.start.to_string() + ")" + ": " + message;
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
        std::vector<Diagnostic> diagnostics_;
        std::string system_name_;
    };
}