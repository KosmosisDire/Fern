# FetchFastFloat.cmake - Fetch the fast_float header-only library
#
# fast_float provides a locale-independent, exception-free std::from_chars for
# floating point. We use it because libc++ (Emscripten, older AppleClang)
# deletes std::from_chars(double&), which would otherwise force a parallel
# strtod path in the literal binder. fast_float gives one uniform code path
# on every toolchain.
#
# fast_float's own CMakeLists adds /permissive- to its INTERFACE target when
# MSVC is true, which breaks clang++ on Windows (CMake reports MSVC=TRUE for
# clang targeting the MSVC ABI, but the GCC frontend rejects the flag). Since
# fast_float is header-only, we skip its CMakeLists entirely (SOURCE_SUBDIR
# points at a directory that does not exist) and define a minimal INTERFACE
# target ourselves. Same pattern BuildLibffi.cmake uses for libffi.
#
# Outputs:
#   fast_float (INTERFACE target) - header-only, link via target_link_libraries

# Respect a consumer that has already fetched or installed fast_float.
if(TARGET fast_float)
    return()
endif()

include(FetchContent)

set(FAST_FLOAT_VERSION "v6.1.6")

FetchContent_Declare(
    fast_float
    GIT_REPOSITORY https://github.com/fastfloat/fast_float.git
    GIT_TAG        ${FAST_FLOAT_VERSION}
    SOURCE_SUBDIR  _fern_skip_cmake
)

FetchContent_MakeAvailable(fast_float)

add_library(fast_float INTERFACE)
target_include_directories(fast_float INTERFACE
    ${fast_float_SOURCE_DIR}/include
)
