#pragma once

#include "document.hpp"

#include <common/compilation.hpp>

#include <memory>
#include <string_view>
#include <vector>

std::unique_ptr<Fern::Compilation> compile_document(
    const DocumentState& doc,
    std::string_view path,
    const std::vector<std::string>& includeFiles,
    std::string_view rootPath);
