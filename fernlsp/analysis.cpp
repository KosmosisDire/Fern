#include "analysis.hpp"

#include <filesystem>

void compile_document(
    DocumentState& doc,
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
    doc.fileId = static_cast<uint32_t>(compilation->get_units().size() - 1);
    compilation->compile();

    doc.compilation = std::move(compilation);
}
