#pragma once

#include "source/span.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace Fern
{



struct Diagnostic
{
    enum class Severity
    {
        Information,
        Warning,
        Error
    };

    Severity severity;
    std::string systemName;
    std::string message;
    Span location;

    Diagnostic(Severity sev, std::string_view msg, const Span& loc, std::string_view sysName)
        : severity(sev)
        , message(msg)
        , location(loc)
        , systemName(sysName)
    {
    }

    std::string_view severity_string() const
    {
        switch (severity)
        {
            case Severity::Information:
                return "Info";
            case Severity::Warning:
                return "Warning";
            case Severity::Error:
                return "Error";
        }
        return "Unknown";
    }

    std::string format() const
    {
        return "[" + systemName + "] " + std::string(severity_string()) +
               "(" + location.format() + "): " + message;
    }

    std::string format(std::string_view filename) const
    {
        return std::string(filename) + ":" + std::to_string(location.startLine + 1) +
               ":" + std::to_string(location.startColumn + 1) + ": " +
               std::string(severity_string()) + ": " + message;
    }
};



class DiagnosticSystem
{
public:
    DiagnosticSystem(std::string_view sysName)
        : systemName(sysName)
    {
    }

    DiagnosticSystem(std::string_view sysName, std::vector<Diagnostic> diags)
        : systemName(sysName)
        , diagnostics(std::move(diags))
    {
    }

    void report(const Diagnostic& diag)
    {
        diagnostics.push_back(diag);
    }

    void info(std::string_view msg, const Span& loc)
    {
        diagnostics.emplace_back(Diagnostic::Severity::Information, msg, loc, systemName);
    }

    void warn(std::string_view msg, const Span& loc)
    {
        diagnostics.emplace_back(Diagnostic::Severity::Warning, msg, loc, systemName);
    }

    void error(std::string_view msg, const Span& loc)
    {
        diagnostics.emplace_back(Diagnostic::Severity::Error, msg, loc, systemName);
    }

    void clear()
    {
        diagnostics.clear();
    }

    const std::vector<Diagnostic>& get_diagnostics() const
    {
        return diagnostics;
    }

    bool has_errors() const
    {
        for (const auto& diag : diagnostics)
        {
            if (diag.severity == Diagnostic::Severity::Error)
            {
                return true;
            }
        }
        return false;
    }

    size_t error_count() const
    {
        size_t count = 0;
        for (const auto& diag : diagnostics)
        {
            if (diag.severity == Diagnostic::Severity::Error)
            {
                count++;
            }
        }
        return count;
    }

private:
    std::string systemName;
    std::vector<Diagnostic> diagnostics;
};


}
