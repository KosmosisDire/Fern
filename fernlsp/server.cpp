#include "server.hpp"
#include "analysis.hpp"
#include "diagnostics.hpp"

#include <lsp/messages.h>

#include <variant>

static std::string get_change_text(const lsp::TextDocumentContentChangeEvent& change)
{
    return std::visit([](const auto& c) -> std::string { return c.text; }, change);
}

FernServer::FernServer(lsp::Connection& connection)
    : handler(connection)
{
    register_handlers();
}

void FernServer::run()
{
    while (running)
    {
        handler.processIncomingMessages();
    }
}

void FernServer::register_handlers()
{
    handler.add<lsp::requests::Initialize>(
        [this](lsp::requests::Initialize::Params&& params)
        {
            if (!params.rootUri.isNull())
            {
                rootPath = uri_to_path(params.rootUri.value());
            }

            if (params.initializationOptions)
            {
                const auto& opts = *params.initializationOptions;
                if (opts.isObject())
                {
                    auto it = opts.object().find("includes");
                    if (it != opts.object().end() && it->second.isArray())
                    {
                        for (const auto& item : it->second.array())
                        {
                            if (item.isString())
                            {
                                includeFiles.push_back(item.string());
                            }
                        }
                    }
                }
            }

            lsp::requests::Initialize::Result result;

            lsp::TextDocumentSyncOptions syncOptions;
            syncOptions.openClose = true;
            syncOptions.change = lsp::TextDocumentSyncKind::Full;
            result.capabilities.textDocumentSync = syncOptions;

            result.serverInfo = lsp::InitializeResultServerInfo{
                .name = "FernLSP",
                .version = "0.1.0"
            };

            return result;
        });

    handler.add<lsp::notifications::Initialized>(
        [](lsp::notifications::Initialized::Params&&) {});

    handler.add<lsp::requests::Shutdown>(
        [this]()
        {
            running = false;
            return lsp::requests::Shutdown::Result{};
        });

    handler.add<lsp::notifications::Exit>(
        [this]() { running = false; });

    handler.add<lsp::notifications::TextDocument_DidOpen>(
        [this](lsp::notifications::TextDocument_DidOpen::Params&& params)
        {
            std::string uriStr = params.textDocument.uri.toString();
            std::string path = uri_to_path(params.textDocument.uri);

            documents[uriStr] = DocumentState{
                .uri = uriStr,
                .content = params.textDocument.text,
                .version = params.textDocument.version
            };

            auto compilation = compile_document(documents[uriStr], path, includeFiles, rootPath);
            publish_diagnostics(handler, params.textDocument.uri, map_diagnostics(compilation->get_diagnostics()));
        });

    handler.add<lsp::notifications::TextDocument_DidChange>(
        [this](lsp::notifications::TextDocument_DidChange::Params&& params)
        {
            if (params.contentChanges.empty())
            {
                return;
            }

            std::string uriStr = params.textDocument.uri.toString();
            auto it = documents.find(uriStr);
            if (it == documents.end())
            {
                return;
            }

            it->second.content = get_change_text(params.contentChanges.back());
            it->second.version = params.textDocument.version;

            std::string path = uri_to_path(params.textDocument.uri);
            auto compilation = compile_document(it->second, path, includeFiles, rootPath);
            publish_diagnostics(handler, params.textDocument.uri, map_diagnostics(compilation->get_diagnostics()));
        });

    handler.add<lsp::notifications::TextDocument_DidClose>(
        [this](lsp::notifications::TextDocument_DidClose::Params&& params)
        {
            std::string uriStr = params.textDocument.uri.toString();
            documents.erase(uriStr);

            publish_diagnostics(handler, params.textDocument.uri, {});
        });
}
