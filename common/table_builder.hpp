#pragma once

#include <string>
#include <vector>
#include <sstream>
#include <iomanip>


class TableBuilder
{
public:
    void add_row(std::vector<std::string> row)
    {
        if (row.size() > columnWidths.size())
        {
            columnWidths.resize(row.size(), 0);
        }

        for (size_t i = 0; i < row.size(); ++i)
        {
            if (row[i].size() > columnWidths[i])
            {
                columnWidths[i] = row[i].size();
            }
        }

        rows.push_back(std::move(row));
    }

    std::string build() const
    {
        std::ostringstream ss;

        for (const auto& row : rows)
        {
            for (size_t i = 0; i < row.size(); ++i)
            {
                ss << std::left << std::setw(static_cast<int>(columnWidths[i] + 2)) << row[i];
            }
            ss << "\n";
        }

        return ss.str();
    }

private:
    std::vector<std::vector<std::string>> rows;
    std::vector<size_t> columnWidths;
};
