#pragma once

#include <source/span.hpp>
#include <format>
#include <string>
#include <string_view>
#include <vector>

namespace Fern
{

#pragma region Code

enum class DiagnosticCode : int
{
    #define DIAG(name, id, sev, fmt) name = id,
    #include <common/diagnostic_codes.def>
    #undef DIAG
};

inline std::string format_id(DiagnosticCode code)
{
    return std::format("FN{:04}", static_cast<int>(code));
}

#pragma region Diagnostic

struct Diagnostic
{
    enum class Severity
    {
        Information,
        Warning,
        Error
    };

    DiagnosticCode code;
    Severity severity;
    Span location;
    std::string message;

    Diagnostic(DiagnosticCode c, Severity sev, std::string msg, const Span& loc)
        : code(c)
        , severity(sev)
        , location(loc)
        , message(std::move(msg))
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

    std::string format(std::string_view filename) const
    {
        return std::format("{}:{}:{}: {} {}: {}",
            filename,
            location.startLine + 1,
            location.startColumn + 1,
            severity_string(),
            format_id(code),
            message);
    }
};

#pragma region Lookup

inline std::string_view format_string_for(DiagnosticCode code)
{
    switch (code)
    {
        #define DIAG(name, id, sev, fmt) case DiagnosticCode::name: return fmt;
        #include <common/diagnostic_codes.def>
        #undef DIAG
    }
    return "";
}

inline Diagnostic::Severity severity_for(DiagnosticCode code)
{
    switch (code)
    {
        #define DIAG(name, id, sev, fmt) case DiagnosticCode::name: return Diagnostic::Severity::sev;
        #include <common/diagnostic_codes.def>
        #undef DIAG
    }
    return Diagnostic::Severity::Error;
}

#pragma region Diagnostics

class Diagnostics
{
public:
    Diagnostics() = default;

    template <typename... Args>
    void report(DiagnosticCode code, const Span& loc, const Args&... args)
    {
        std::string msg = std::vformat(format_string_for(code), std::make_format_args(args...));
        diagnostics.emplace_back(code, severity_for(code), std::move(msg), loc);
    }

    void clear()
    {
        diagnostics.clear();
    }

    size_t checkpoint() const
    {
        return diagnostics.size();
    }

    void restore(size_t cp)
    {
        if (cp < diagnostics.size())
        {
            diagnostics.erase(diagnostics.begin() + cp, diagnostics.end());
        }
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
    std::vector<Diagnostic> diagnostics;
};

}
