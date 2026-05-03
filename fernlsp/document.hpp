#pragma once

#include <lsp/fileuri.h>

#include <common/compilation.hpp>

#include <cstdint>
#include <memory>
#include <string>

std::string uri_to_path(const lsp::FileUri& uri);

struct DocumentState
{
    std::string uri;
    std::string content;
    int version = 0;
    std::unique_ptr<Fern::Compilation> compilation;
    uint32_t fileId = 0;
};
