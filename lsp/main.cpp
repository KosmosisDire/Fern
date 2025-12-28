#include <lsp/connection.h>
#include <lsp/messagehandler.h>
#include <lsp/messages.h>
#include <lsp/io/standardio.h>
#include "common/logger.hpp"
#include "common/file_utils.hpp"

#include "compiler.hpp"
#include "common/source_database.hpp"

#include <string>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <filesystem>

using namespace Fern;

#pragma region Document State

struct DocumentState {
    std::string uri;
    std::string path;
    std::string content;
    int version = 0;
};

class DocumentManager {
public:
    void open(const lsp::FileUri& uri, const std::string& content, int version) {
        std::string uri_str = uri.toString();
        std::string path = std::string(uri.path());
        documents_[uri_str] = {uri_str, path, content, version};
    }

    void update(const std::string& uri, const std::string& content, int version) {
        if (auto it = documents_.find(uri); it != documents_.end()) {
            it->second.content = content;
            it->second.version = version;
        }
    }

    void close(const std::string& uri) {
        documents_.erase(uri);
    }

    const DocumentState* get(const std::string& uri) const {
        if (auto it = documents_.find(uri); it != documents_.end()) {
            return &it->second;
        }
        return nullptr;
    }

private:
    std::unordered_map<std::string, DocumentState> documents_;
};

#pragma region Workspace Configuration

struct WorkspaceConfig {
    std::filesystem::path root_path;
    std::vector<std::string> include_patterns;
};

#pragma region Helpers

std::string read_file(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) return "";
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

std::string uri_to_path(const lsp::FileUri& uri) {
    return std::string(uri.path());
}

std::string uri_to_string(const lsp::FileUri& uri) {
    return uri.toString();
}

std::string get_text_from_change(const lsp::TextDocumentContentChangeEvent& change) {
    return std::visit([](const auto& c) -> std::string {
        return c.text;
    }, change);
}

#pragma region Diagnostics

std::vector<lsp::Diagnostic> compile_and_get_diagnostics(
    DocumentManager& docs,
    const std::string& uri,
    const WorkspaceConfig& config
) {
    std::vector<lsp::Diagnostic> lsp_diagnostics;

    const auto* doc = docs.get(uri);
    if (!doc) return lsp_diagnostics;

    Compiler compiler;
    compiler.set_print_ast(false);
    compiler.set_print_symbols(false);
    compiler.set_print_flir(false);

    std::vector<SourceFile> sources;
    std::unordered_set<std::string> added_paths;

    auto include_files = expand_path_patterns(config.include_patterns, config.root_path);
    const std::string& current_file_path = doc->path;

    std::filesystem::path cur_fs_path(current_file_path);
    if (std::filesystem::exists(cur_fs_path)) {
        added_paths.insert(std::filesystem::canonical(cur_fs_path).string());
    }

    for (const auto& include_path : include_files) {
        std::filesystem::path inc_fs_path(include_path);
        if (!std::filesystem::exists(inc_fs_path)) continue;

        std::string canonical = std::filesystem::canonical(inc_fs_path).string();
        if (added_paths.count(canonical)) continue;
        added_paths.insert(canonical);

        std::string content = read_file(include_path);
        if (!content.empty()) {
            sources.push_back({include_path, content});
        }
    }

    sources.push_back({current_file_path, doc->content});

    auto result = compiler.compile(sources);

    if (result) {
        for (const auto& diag : result->get_diagnostics()) {
            lsp::Diagnostic lsp_diag;
            lsp_diag.message = diag.message;
            lsp_diag.range.start.line = static_cast<lsp::uint>(diag.location.start.line > 0 ? diag.location.start.line - 1 : 0);
            lsp_diag.range.start.character = static_cast<lsp::uint>(diag.location.start.column > 0 ? diag.location.start.column - 1 : 0);
            lsp_diag.range.end.line = lsp_diag.range.start.line;
            lsp_diag.range.end.character = lsp_diag.range.start.character + static_cast<lsp::uint>(diag.location.width);

            switch (diag.severity) {
                case Diagnostic::Severity::Error:
                case Diagnostic::Severity::Fatal:
                    lsp_diag.severity = lsp::DiagnosticSeverity::Error;
                    break;
                case Diagnostic::Severity::Warning:
                    lsp_diag.severity = lsp::DiagnosticSeverity::Warning;
                    break;
                case Diagnostic::Severity::Info:
                    lsp_diag.severity = lsp::DiagnosticSeverity::Information;
                    break;
                default:
                    lsp_diag.severity = lsp::DiagnosticSeverity::Hint;
                    break;
            }

            lsp_diag.source = "fern";
            lsp_diagnostics.push_back(std::move(lsp_diag));
        }
    }

    return lsp_diagnostics;
}

#pragma region Main

int main() {
    Logger& logger = Logger::get_instance();
    logger.initialize();
    logger.set_console_level(LogLevel::NONE);

    auto connection = lsp::Connection{lsp::io::standardIO()};
    auto handler = lsp::MessageHandler{connection};
    DocumentManager documents;
    WorkspaceConfig workspace_config;

    bool running = true;

    handler.add<lsp::requests::Initialize>(
        [&](lsp::requests::Initialize::Params&& params) {
            if (!params.rootUri.isNull()) {
                workspace_config.root_path = uri_to_path(params.rootUri.value());
            } else if (params.rootPath && !params.rootPath->isNull()) {
                workspace_config.root_path = params.rootPath->value();
            }

            if (params.initializationOptions) {
                const auto& opts = *params.initializationOptions;
                if (opts.isObject()) {
                    const auto& obj = opts.object();
                    auto it = obj.find("includes");
                    if (it != obj.end() && it->second.isArray()) {
                        for (const auto& item : it->second.array()) {
                            if (item.isString()) {
                                workspace_config.include_patterns.push_back(item.string());
                            }
                        }
                    }
                }
            }

            lsp::requests::Initialize::Result result;

            lsp::TextDocumentSyncOptions sync_options;
            sync_options.openClose = true;
            sync_options.change = lsp::TextDocumentSyncKind::Full;
            result.capabilities.textDocumentSync = sync_options;

            result.capabilities.hoverProvider = true;
            result.capabilities.definitionProvider = true;
            result.capabilities.documentSymbolProvider = true;

            lsp::InitializeResultServerInfo server_info;
            server_info.name = "FernLSP";
            server_info.version = "0.1.0";
            result.serverInfo = server_info;

            return result;
        });

    handler.add<lsp::notifications::Initialized>(
        [&](lsp::notifications::Initialized::Params&&) {
        });

    handler.add<lsp::requests::Shutdown>(
        [&]() {
            running = false;
            return lsp::requests::Shutdown::Result{};
        });

    handler.add<lsp::notifications::Exit>(
        [&]() {
            running = false;
        });

    handler.add("fern/updateIncludes", [&](lsp::json::Any&& params) -> lsp::json::Any {
        workspace_config.include_patterns.clear();
        if (params.isObject()) {
            const auto& obj = params.object();
            auto it = obj.find("includes");
            if (it != obj.end() && it->second.isArray()) {
                for (const auto& item : it->second.array()) {
                    if (item.isString()) {
                        workspace_config.include_patterns.push_back(item.string());
                    }
                }
            }
        }
        return lsp::json::Any{};
    });

    handler.add<lsp::notifications::TextDocument_DidOpen>(
        [&](lsp::notifications::TextDocument_DidOpen::Params&& params) {
            std::string uri_str = uri_to_string(params.textDocument.uri);
            documents.open(
                params.textDocument.uri,
                params.textDocument.text,
                params.textDocument.version
            );

            auto diagnostics = compile_and_get_diagnostics(documents, uri_str, workspace_config);

            lsp::PublishDiagnosticsParams diag_params;
            diag_params.uri = params.textDocument.uri;
            diag_params.diagnostics = std::move(diagnostics);
            handler.sendNotification<lsp::notifications::TextDocument_PublishDiagnostics>(std::move(diag_params));
        });

    handler.add<lsp::notifications::TextDocument_DidChange>(
        [&](lsp::notifications::TextDocument_DidChange::Params&& params) {
            if (!params.contentChanges.empty()) {
                std::string uri_str = uri_to_string(params.textDocument.uri);
                std::string new_text = get_text_from_change(params.contentChanges.back());

                documents.update(
                    uri_str,
                    new_text,
                    params.textDocument.version
                );

                auto diagnostics = compile_and_get_diagnostics(documents, uri_str, workspace_config);

                lsp::PublishDiagnosticsParams diag_params;
                diag_params.uri = params.textDocument.uri;
                diag_params.diagnostics = std::move(diagnostics);
                handler.sendNotification<lsp::notifications::TextDocument_PublishDiagnostics>(std::move(diag_params));
            }
        });

    handler.add<lsp::notifications::TextDocument_DidClose>(
        [&](lsp::notifications::TextDocument_DidClose::Params&& params) {
            std::string uri_str = uri_to_string(params.textDocument.uri);
            documents.close(uri_str);

            lsp::PublishDiagnosticsParams diag_params;
            diag_params.uri = params.textDocument.uri;
            diag_params.diagnostics = {};
            handler.sendNotification<lsp::notifications::TextDocument_PublishDiagnostics>(std::move(diag_params));
        });

    handler.add<lsp::requests::TextDocument_Hover>(
        [&](lsp::requests::TextDocument_Hover::Params&& params) -> lsp::requests::TextDocument_Hover::Result {
            (void)params;
            // TODO: Implement hover using symbol table lookup
            return nullptr;
        });

    handler.add<lsp::requests::TextDocument_Definition>(
        [&](lsp::requests::TextDocument_Definition::Params&& params) -> lsp::requests::TextDocument_Definition::Result {
            (void)params;
            // TODO: Implement go-to-definition using symbol resolution
            return nullptr;
        });

    handler.add<lsp::requests::TextDocument_DocumentSymbol>(
        [&](lsp::requests::TextDocument_DocumentSymbol::Params&& params) -> lsp::requests::TextDocument_DocumentSymbol::Result {
            (void)params;
            // TODO: Implement document symbols from symbol table
            return std::vector<lsp::DocumentSymbol>{};
        });

    while (running) {
        handler.processIncomingMessages();
    }

    return 0;
}
