#include "error.hpp"

namespace Fern
{
    DiagnosticSystem::DiagnosticSystem(const std::string& system_name)
        : system_name_(system_name)
    {
    }

    DiagnosticSystem::DiagnosticSystem(const std::string& system_name, std::vector<Diagnostic> diagnostics) : system_name_(system_name)
    {
        for (const auto& diag : diagnostics)
        {
            diagnostics_.push_back(diag);
        }
    }

    void DiagnosticSystem::report(const Diagnostic& diag)
    {
        diagnostics_.push_back(diag);
    }

    void DiagnosticSystem::debug(const std::string& msg, const SourceRange& loc)
    {
        report(Diagnostic(Diagnostic::Severity::Debug, msg, loc, system_name_));
    }

    void DiagnosticSystem::info(const std::string& msg, const SourceRange& loc)
    {
        report(Diagnostic(Diagnostic::Severity::Info, msg, loc, system_name_));
    }

    void DiagnosticSystem::warn(const std::string& msg, const SourceRange& loc)
    {
        report(Diagnostic(Diagnostic::Severity::Warning, msg, loc, system_name_));
    }

    void DiagnosticSystem::error(const std::string& msg, const SourceRange& loc)
    {
        report(Diagnostic(Diagnostic::Severity::Error, msg, loc, system_name_));
    }

    void DiagnosticSystem::fatal(const std::string& msg, const SourceRange& loc)
    {
        report(Diagnostic(Diagnostic::Severity::Fatal, msg, loc, system_name_));
    }

    void DiagnosticSystem::clear_diagnostics()
    {
        diagnostics_.clear();
    }

    const std::vector<Diagnostic>& DiagnosticSystem::get_diagnostics() const
    {
        return diagnostics_;
    }

    const bool DiagnosticSystem::has_errors() const
    {
        for (const auto& diag : diagnostics_)
        {
            if (diag.severity == Diagnostic::Severity::Error ||
                diag.severity == Diagnostic::Severity::Fatal)
            {
                return true;
            }
        }
        return false;
    }

    const size_t DiagnosticSystem::error_count() const
    {
        size_t count = 0;
        for (const auto& diag : diagnostics_)
        {
            if (diag.severity == Diagnostic::Severity::Error ||
                diag.severity == Diagnostic::Severity::Fatal)
            {
                count++;
            }
        }
        return count;
    }
}
