#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <format>
#include <string>

#include <ast/formatdbg.hpp>
#include <common/compilation.hpp>
#include <semantic/fhir/fmt.hpp>


namespace
{

constexpr uint8_t fileSeparator[] = {0xFF, 0x00, 0xFF, 0x00};
constexpr size_t fileSeparatorSize = sizeof(fileSeparator);

// XOR'd with the size of every formatter result so the optimizer cannot drop
// the format calls as dead. We exercise the formatters for libFuzzer coverage
// and ASan, but the strings themselves are not needed.
volatile size_t fuzzSink = 0;

void add_chunk(Fern::Compilation& compilation, const uint8_t* data, size_t size, int index)
{
    std::string content(reinterpret_cast<const char*>(data), size);
    std::string name = std::format("fuzz_{}.fn", index);
    compilation.add_source(std::move(content), name);
}

void exercise_formatters(Fern::Compilation& compilation)
{
    for (const auto& unit : compilation.get_units())
    {
        fuzzSink ^= Fern::AstDebugFormatter::format(unit->ast).size();
    }

    fuzzSink ^= compilation.semantic().format().size();

    for (auto* method : compilation.semantic().methods)
    {
        fuzzSink ^= Fern::FhirDebugFormatter::format(method).size();
    }

    for (const auto& diag : compilation.diag.get_diagnostics())
    {
        fuzzSink ^= diag.format("fuzz").size();
    }
}

}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size)
{
    try
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

        exercise_formatters(compilation);

        return 0;
    }
    catch (const std::exception& e)
    {
        std::fprintf(stderr, "FUZZ EXCEPTION: %s\n", e.what());
        std::fflush(stderr);
        std::_Exit(77);
    }
    catch (...)
    {
        std::fprintf(stderr, "FUZZ EXCEPTION: unknown\n");
        std::fflush(stderr);
        std::_Exit(77);
    }
}
