#include "document.hpp"

std::string uri_to_path(const lsp::FileUri& uri)
{
    return std::string(uri.path());
}
