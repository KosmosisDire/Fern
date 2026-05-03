#pragma once

#include "document.hpp"

#include <lsp/messages.h>

#include <cstdint>
#include <optional>

std::optional<lsp::Hover> compute_hover(
    const DocumentState& doc,
    uint32_t line,
    uint32_t col);

std::optional<lsp::Hover> compute_ast_debug_hover(
    const DocumentState& doc,
    uint32_t line,
    uint32_t col);

std::optional<lsp::Hover> compute_fhir_debug_hover(
    const DocumentState& doc,
    uint32_t line,
    uint32_t col);
