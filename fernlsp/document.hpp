#pragma once

#include <lsp/fileuri.h>

#include <string>

std::string uri_to_path(const lsp::FileUri& uri);

struct DocumentState
{
    std::string uri;
    std::string content;
    int version = 0;
};
