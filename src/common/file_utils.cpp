#include "file_utils.hpp"
#include <algorithm>

namespace Fern {

bool matches_wildcard_pattern(const std::string& filename, const std::string& pattern) {
    size_t pattern_idx = 0;
    size_t name_idx = 0;
    size_t star_idx = std::string::npos;
    size_t match_idx = 0;

    while (name_idx < filename.length()) {
        if (pattern_idx < pattern.length() &&
            (pattern[pattern_idx] == filename[name_idx] || pattern[pattern_idx] == '?')) {
            pattern_idx++;
            name_idx++;
        }
        else if (pattern_idx < pattern.length() && pattern[pattern_idx] == '*') {
            star_idx = pattern_idx;
            match_idx = name_idx;
            pattern_idx++;
        }
        else if (star_idx != std::string::npos) {
            pattern_idx = star_idx + 1;
            match_idx++;
            name_idx = match_idx;
        }
        else {
            return false;
        }
    }

    while (pattern_idx < pattern.length() && pattern[pattern_idx] == '*') {
        pattern_idx++;
    }

    return pattern_idx == pattern.length();
}

std::vector<std::string> expand_path_pattern(const std::string& pattern, const std::filesystem::path& base_path) {
    std::vector<std::string> result;

    std::filesystem::path full_pattern;
    if (std::filesystem::path(pattern).is_absolute()) {
        full_pattern = pattern;
    } else {
        full_pattern = base_path / pattern;
    }

    if (pattern.find('*') == std::string::npos && pattern.find('?') == std::string::npos) {
        if (std::filesystem::exists(full_pattern)) {
            result.push_back(full_pattern.string());
        }
        return result;
    }

    std::filesystem::path dir_path = full_pattern.parent_path();
    std::string filename_pattern = full_pattern.filename().string();

    if (dir_path.empty()) {
        dir_path = base_path;
    }

    if (!std::filesystem::exists(dir_path) || !std::filesystem::is_directory(dir_path)) {
        return result;
    }

    try {
        for (const auto& entry : std::filesystem::directory_iterator(dir_path)) {
            if (entry.is_regular_file()) {
                std::string filename = entry.path().filename().string();
                if (matches_wildcard_pattern(filename, filename_pattern)) {
                    result.push_back(entry.path().string());
                }
            }
        }
    } catch (const std::filesystem::filesystem_error&) {
    }

    std::sort(result.begin(), result.end());
    return result;
}

std::vector<std::string> expand_path_patterns(const std::vector<std::string>& patterns, const std::filesystem::path& base_path) {
    std::vector<std::string> result;
    for (const auto& pattern : patterns) {
        auto expanded = expand_path_pattern(pattern, base_path);
        result.insert(result.end(), expanded.begin(), expanded.end());
    }
    return result;
}

} // namespace Fern
