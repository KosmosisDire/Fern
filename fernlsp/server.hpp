#pragma once

#include "document.hpp"

#include <lsp/connection.h>
#include <lsp/messagehandler.h>

#include <unordered_map>
#include <vector>

class FernServer
{
public:
    FernServer(lsp::Connection& connection);

    void run();

    lsp::MessageHandler handler;
    std::unordered_map<std::string, DocumentState> documents;
    std::vector<std::string> includeFiles;
    std::string rootPath;

private:
    bool running = true;

    void register_handlers();
};
