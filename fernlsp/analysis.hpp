#pragma once

#include "document.hpp"

#include <string_view>
#include <vector>
#include <string>

void compile_document(
    DocumentState& doc,
    std::string_view path,
    const std::vector<std::string>& includeFiles,
    std::string_view rootPath);
