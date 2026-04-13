#pragma once

#include <lsp/messagehandler.h>
#include <lsp/messages.h>

#include <common/diagnostic.hpp>

#include <vector>

std::vector<lsp::Diagnostic> map_diagnostics(const std::vector<Fern::Diagnostic>& diagnostics);

void publish_diagnostics(
    lsp::MessageHandler& handler,
    const lsp::FileUri& uri,
    std::vector<lsp::Diagnostic> diagnostics);
