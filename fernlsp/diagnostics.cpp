#include "diagnostics.hpp"

std::vector<lsp::Diagnostic> map_diagnostics(const std::vector<Fern::Diagnostic>& diagnostics)
{
    std::vector<lsp::Diagnostic> result;

    for (const auto& diag : diagnostics)
    {
        lsp::Diagnostic lspDiag;
        lspDiag.message = diag.message;
        lspDiag.source = "fern";

        lspDiag.range.start.line = diag.location.startLine;
        lspDiag.range.start.character = diag.location.startColumn;
        lspDiag.range.end.line = diag.location.endLine;
        lspDiag.range.end.character = diag.location.endColumn;

        switch (diag.severity)
        {
            case Fern::Diagnostic::Severity::Error:
                lspDiag.severity = lsp::DiagnosticSeverity::Error;
                break;
            case Fern::Diagnostic::Severity::Warning:
                lspDiag.severity = lsp::DiagnosticSeverity::Warning;
                break;
            case Fern::Diagnostic::Severity::Information:
                lspDiag.severity = lsp::DiagnosticSeverity::Information;
                break;
        }

        result.push_back(std::move(lspDiag));
    }

    return result;
}

void publish_diagnostics(
    lsp::MessageHandler& handler,
    const lsp::FileUri& uri,
    std::vector<lsp::Diagnostic> diagnostics)
{
    lsp::PublishDiagnosticsParams params;
    params.uri = uri;
    params.diagnostics = std::move(diagnostics);
    handler.sendNotification<lsp::notifications::TextDocument_PublishDiagnostics>(std::move(params));
}
