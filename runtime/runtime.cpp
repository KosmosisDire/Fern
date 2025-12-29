#include <cstdio>
#include <cstring>
#include <cstdint>
#include <ctime>

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

FN uint64_t fn_time_now()
{
    return static_cast<uint64_t>(time(nullptr));
}



struct LargeStructI
{
    int data[16];
};

struct LargeStructF
{
    float data[16];
};

struct MidStructI
{
    int x;
    int y;
    int z;
    int w;
};

struct MidStructF
{
    float x;
    float y;
    float z;
    float w;
};

struct SmallStructI
{
    int x;
    int y;
};

struct SmallStructF
{
    float x;
    float y;
};

struct TinyStructI
{
    int x;
};

struct TinyStructF
{
    float x;
};

struct MixedStruct
{
    int a;
    float b;
    double c;
    char d;
};

struct NestedStruct
{
    SmallStructI s1;
    SmallStructF s2;
    TinyStructI s3;
    TinyStructF s4;
};

struct NestedStruct2
{
    TinyStructI s1;
    TinyStructF s2;
};


FN void fn_a_abi_test1()
{
    // Empty function for ABI testing
}

FN void fn_a_abi_test2(int a, float b, double c, char d)
{
    // Empty function for ABI testing
}

FN void fn_a_abi_test3(int* arr, int size)
{
    // Empty function for ABI testing
}

FN LargeStructI fn_a_abi_test4()
{
    LargeStructI ls = {};
    for (int i = 0; i < 16; ++i) {
        ls.data[i] = i;
    }
    return ls;
}

FN MidStructF fn_a_abi_test5(float x, float y, float z, float w)
{
    MidStructF ms = { x, y, z, w };
    return ms;
}

FN MixedStruct fn_a_abi_test6(int a, float b, double c, char d)
{
    MixedStruct ms = { a, b, c, d };
    return ms;
}

FN NestedStruct fn_a_abi_test7(SmallStructI s1, SmallStructF s2, TinyStructI s3, TinyStructF s4)
{
    NestedStruct ns = { s1, s2, s3, s4 };
    return ns;
}

FN NestedStruct2 fn_a_abi_test8(TinyStructI s1, TinyStructF s2)
{
    NestedStruct2 ns = { s1, s2 };
    return ns;
}

FN void fn_a_abi_test9(TinyStructI s1)
{
    // Empty function for ABI testing
}

FN void fn_a_abi_test10(MidStructI s1)
{
    // Empty function for ABI testing
}

}