// Silence MSVC's "use fopen_s" deprecation warnings. The fixed name fopen
// call below has no path injection concerns the secure variant would
// protect against.
#ifndef _CRT_SECURE_NO_WARNINGS
    #define _CRT_SECURE_NO_WARNINGS 1
#endif

#include <cerrno>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <format>
#include <string>

#if defined(_WIN32)
    #include <io.h>
#else
    #include <unistd.h>
#endif

#include <common/compilation.hpp>

// ASan, UBSan, and LSan default to abort_on_error=1, which on Windows
// triggers int 3 (STATUS_BREAKPOINT). libFuzzer's fork mode does not
// recognize that exit code as a crash, so it propagates the code without
// saving an artifact. exitcode=77 matches libFuzzer's error_exitcode so
// fork mode captures crashes correctly.
//
// LSan runs at process exit. To find a leak, run with -runs=N (a finite
// run count) so the process exits and LSan dumps stacks. Set
// detect_leaks=0 to silence it during long fuzzing campaigns.
//
// The dynamic ASan runtime DLL on Windows resolves these via GetProcAddress
// against the main executable, so they must be explicitly exported.
#if defined(_WIN32)
    #define FERN_FUZZ_SAN_EXPORT __declspec(dllexport)
#else
    #define FERN_FUZZ_SAN_EXPORT __attribute__((visibility("default")))
#endif

extern "C" FERN_FUZZ_SAN_EXPORT const char* __asan_default_options()
{
    // exitcode=77 matches libFuzzer's error_exitcode. abort_on_error=0 keeps
    // the process from int 3 ing on detected errors. quarantine_size_mb caps
    // how much freed memory ASan pins for use after free detection so long
    // fuzz runs do not exhaust the heap through accumulated quarantine
    // pressure.
    return "abort_on_error=0:exitcode=77:quarantine_size_mb=32:max_redzone=64";
}

extern "C" FERN_FUZZ_SAN_EXPORT const char* __ubsan_default_options()
{
    return "abort_on_error=0:exitcode=77";
}

extern "C" FERN_FUZZ_SAN_EXPORT const char* __lsan_default_options()
{
    return "exitcode=77";
}


namespace
{

constexpr uint8_t fileSeparator[] = {0xFF, 0x00, 0xFF, 0x00};
constexpr size_t fileSeparatorSize = sizeof(fileSeparator);

// Long lived handle for last_input.bin so each iteration only pays for a
// rewind + write + truncate, not a full open/close. Set to nullptr if the
// initial open fails so subsequent saves silently no op.
FILE* lastInputFile = nullptr;

void add_chunk(Fern::Compilation& compilation, const uint8_t* data, size_t size, int index)
{
    std::string content(reinterpret_cast<const char*>(data), size);
    std::string name = std::format("fuzz_{}.fn", index);
    compilation.add_source(std::move(content), name);
}

void save_last_input(const uint8_t* data, size_t size)
{
    if (!lastInputFile) return;

    std::fseek(lastInputFile, 0, SEEK_SET);
    std::fwrite(data, 1, size, lastInputFile);

    // Truncate to the exact input size so a smaller input does not leave
    // leftover bytes from a prior larger one. A silent failure here means
    // last_input.bin can carry stale tail bytes from earlier iterations,
    // which will confuse any attempt to reproduce a crash.
#if defined(_WIN32)
    int rc = _chsize_s(_fileno(lastInputFile), static_cast<long long>(size));
#else
    int rc = ftruncate(fileno(lastInputFile), static_cast<off_t>(size));
#endif
    if (rc != 0)
    {
        std::fprintf(stderr, "save_last_input: truncate to %zu failed (rc=%d, errno=%d)\n",
                     size, rc, errno);
        std::fflush(stderr);
    }
}

}

extern "C" FERN_FUZZ_SAN_EXPORT int LLVMFuzzerInitialize(int*, char***)
{
    lastInputFile = std::fopen("last_input.bin", "wb");
    if (lastInputFile)
    {
        // Disable stdio buffering so writes go straight to the OS. The OS
        // page cache still buffers the data, but a process abort no longer
        // leaves the most recent input stuck in stdio's userspace buffer.
        std::setvbuf(lastInputFile, nullptr, _IONBF, 0);
    }
    return 0;
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size)
{
    save_last_input(data, size);

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
