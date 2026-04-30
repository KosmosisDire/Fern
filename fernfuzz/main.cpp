#include <cstdint>
#include <cstring>
#include <format>
#include <string>

#include <common/compilation.hpp>

namespace
{

constexpr uint8_t fileSeparator[] = {0xFF, 0x00, 0xFF, 0x00};
constexpr size_t fileSeparatorSize = sizeof(fileSeparator);

void add_chunk(Fern::Compilation& compilation, const uint8_t* data, size_t size, int index)
{
    std::string content(reinterpret_cast<const char*>(data), size);
    std::string name = std::format("fuzz_{}.fn", index);
    compilation.add_source(std::move(content), name);
}

}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size)
{
    Fern::Compilation compilation;

    size_t start = 0;
    int fileIndex = 0;

    for (size_t i = 0; i + fileSeparatorSize <= size; ++i)
    {
        if (std::memcmp(data + i, fileSeparator, fileSeparatorSize) == 0)
        {
            add_chunk(compilation, data + start, i - start, fileIndex++);
            start = i + fileSeparatorSize;
            i = start - 1;
        }
    }

    add_chunk(compilation, data + start, size - start, fileIndex);

    compilation.compile();

    return 0;
}
