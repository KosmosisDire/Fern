#pragma once

#include <string>
#include <vector>
#include <filesystem>

namespace Fern {

bool matches_wildcard_pattern(const std::string& filename, const std::string& pattern);

std::vector<std::string> expand_path_pattern(const std::string& pattern, const std::filesystem::path& base_path = ".");

std::vector<std::string> expand_path_patterns(const std::vector<std::string>& patterns, const std::filesystem::path& base_path = ".");

} // namespace Fern
