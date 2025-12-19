#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define FN __declspec(dllexport)
#else
#define FN
#endif

extern "C"
{

FN void fn_get_input(char* buffer, int max_size) {
    if (buffer == NULL || max_size <= 0) {
        return;
    }
    
    if (fgets(buffer, max_size, stdin) != NULL) {
        // Remove trailing newline if present
        size_t len = strlen(buffer);
        if (len > 0 && buffer[len - 1] == '\n') {
            buffer[len - 1] = '\0';
        }
    }
}

FN void fn_print(const char* s) {
    if (s != NULL) {
        fputs(s, stdout);
    }
}

}