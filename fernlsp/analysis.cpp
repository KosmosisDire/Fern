#include "analysis.hpp"

#include <filesystem>

std::unique_ptr<Fern::Compilation> compile_document(
    const DocumentState& doc,
    std::string_view path,
    const std::vector<std::string>& includeFiles,
    std::string_view rootPath)
{
    auto compilation = std::make_unique<Fern::Compilation>();

    std::string docCanonical;
    if (std::filesystem::exists(std::string(path)))
    {
        docCanonical = std::filesystem::canonical(std::string(path)).string();
    }

    for (const auto& include : includeFiles)
    {
        auto includePath = std::filesystem::path(rootPath) / include;
        if (!std::filesystem::exists(includePath))
        {
            continue;
        }

        auto canonical = std::filesystem::canonical(includePath).string();
        if (canonical == docCanonical)
        {
            continue;
        }

        compilation->add_file(canonical);
    }

    compilation->add_source(doc.content, path);
    compilation->compile();

    return compilation;
}
