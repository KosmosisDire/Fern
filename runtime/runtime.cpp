#include <cstdio>
#include <cstring>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#define FN __declspec(dllexport)
#else
#include <locale.h>
#define FN
#endif

namespace {
    // Initialization helper that runs before main()
    struct RuntimeInit {
        RuntimeInit() {
#ifdef _WIN32
            // Set console code pages to UTF-8
            SetConsoleOutputCP(CP_UTF8);
            SetConsoleCP(CP_UTF8);

            // Enable virtual terminal processing for ANSI escape sequences
            HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
            if (hOut != INVALID_HANDLE_VALUE) {
                DWORD mode = 0;
                if (GetConsoleMode(hOut, &mode)) {
                    SetConsoleMode(hOut, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
                }
            }
#else
            // Set locale to support UTF-8 on Unix-like systems
            setlocale(LC_ALL, "");
#endif
        }
    };

    // Static instance ensures initialization happens before main()
    static RuntimeInit runtime_init;
}

extern "C"
{

FN void fn_get_input(char* buffer, int max_size) {
    if (buffer == nullptr || max_size <= 0) {
        return;
    }

    if (std::fgets(buffer, max_size, stdin) != nullptr) {
        // Remove trailing newline if present (handles both \n and \r\n)
        std::size_t len = std::strlen(buffer);
        while (len > 0 && (buffer[len - 1] == '\n' || buffer[len - 1] == '\r')) {
            buffer[--len] = '\0';
        }
    }
}

FN void fn_print(const char* s) {
    if (s != nullptr) {
        std::fputs(s, stdout);
        std::fflush(stdout);  // Ensure immediate output for Unicode characters
    }
}

}