#pragma once

#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>
#include <optional>

namespace Fern
{
    class SourceDatabase
    {
    public:
        virtual ~SourceDatabase() = default;

        virtual int add_file(std::string path, std::string contents) = 0;
        virtual std::string_view get_contents(int file_id) const = 0;
        virtual std::string_view get_path(int file_id) const = 0;
        virtual std::optional<int> find_file(std::string_view path) const = 0;
        virtual size_t file_count() const = 0;
        virtual bool is_valid_file_id(int file_id) const = 0;
    };

#pragma region SimpleSourceDatabase

    class SimpleSourceDatabase : public SourceDatabase
    {
    public:
        int add_file(std::string path, std::string contents) override
        {
            int id = static_cast<int>(files_.size());
            path_to_id_[path] = id;
            files_.push_back({std::move(path), std::move(contents)});
            return id;
        }

        std::string_view get_contents(int file_id) const override
        {
            if (!is_valid_file_id(file_id))
                return {};
            return files_[file_id].contents;
        }

        std::string_view get_path(int file_id) const override
        {
            if (!is_valid_file_id(file_id))
                return {};
            return files_[file_id].path;
        }

        std::optional<int> find_file(std::string_view path) const override
        {
            auto it = path_to_id_.find(std::string(path));
            if (it != path_to_id_.end())
                return it->second;
            return std::nullopt;
        }

        size_t file_count() const override
        {
            return files_.size();
        }

        bool is_valid_file_id(int file_id) const override
        {
            return file_id >= 0 && static_cast<size_t>(file_id) < files_.size();
        }

    private:
        struct FileEntry
        {
            std::string path;
            std::string contents;
        };

        std::vector<FileEntry> files_;
        std::unordered_map<std::string, int> path_to_id_;
    };

} // namespace Fern
