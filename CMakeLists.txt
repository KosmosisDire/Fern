cmake_minimum_required(VERSION 3.18)

# Detect whether Fern is the top-level project. Embedded consumers
# (add_subdirectory) should not get our developer tools (CLI/LSP) by default,
# nor inherit our strict toolchain gates.
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    set(_fern_default_dev_tools ON)
else()
    set(_fern_default_dev_tools OFF)
endif()

option(FERN_BUILD_LLVM   "Build with LLVM backend"     ON)
option(FERN_BUILD_LIBFFI "Build with libffi support"   ON)
option(FERN_BUILD_CLI    "Build the Fern CLI"          ${_fern_default_dev_tools})
option(FERN_BUILD_LSP    "Build the Fern LSP server"   ${_fern_default_dev_tools})
option(FERN_BUILD_FUZZER "Build the libFuzzer harness" OFF)

# ---------------------------------------------------------------------------
# Pre-project gates: only enforced when building the LSP, which is the only
# component with strict toolchain requirements (lsp-framework breaks on
# libstdc++ < 12, and we want Clang+Ninja for our developer workflow).
# Embedded consumers building only fernlib (and optionally ferncli) skip these.
# ---------------------------------------------------------------------------
if(FERN_BUILD_LSP)
    if(NOT CMAKE_GENERATOR MATCHES "Ninja")
        message(FATAL_ERROR
            "Fern's LSP build requires the Ninja generator, but CMake was invoked with '${CMAKE_GENERATOR}'.\n"
            "Install Ninja:\n"
            "    Windows (winget):       winget install Ninja-build.Ninja\n"
            "    Debian/Ubuntu (apt):    sudo apt install ninja-build\n"
            "    Other:                  https://ninja-build.org/\n"
            "Then reconfigure using a Fern preset, or set -DFERN_BUILD_LSP=OFF to skip the LSP.")
    endif()

    find_program(NINJA_EXECUTABLE ninja)
    if(NOT NINJA_EXECUTABLE)
        message(FATAL_ERROR
            "Fern's LSP build requires Ninja, but the 'ninja' executable was not found on PATH.\n"
            "Install Ninja:\n"
            "    Windows (winget):       winget install Ninja-build.Ninja\n"
            "    Debian/Ubuntu (apt):    sudo apt install ninja-build\n"
            "    Other:                  https://ninja-build.org/\n"
            "Or set -DFERN_BUILD_LSP=OFF to skip the LSP.")
    endif()

    foreach(_lang IN ITEMS CXX C)
        if(_lang STREQUAL "C")
            set(_env CC)
        else()
            set(_env CXX)
        endif()

        set(_compiler "${CMAKE_${_lang}_COMPILER}")
        if(NOT _compiler AND DEFINED ENV{${_env}})
            set(_compiler "$ENV{${_env}}")
        endif()

        if(NOT _compiler)
            continue()
        endif()

        if(IS_ABSOLUTE "${_compiler}")
            if(EXISTS "${_compiler}")
                continue()
            endif()
        else()
            find_program(_fern_${_lang}_path "${_compiler}")
            if(_fern_${_lang}_path)
                continue()
            endif()
        endif()

        message(FATAL_ERROR
            "Fern's LSP build requires Clang 19 or newer, but the configured ${_lang} compiler "
            "'${_compiler}' was not found.\n"
            "Install Clang:\n"
            "    Windows (winget):       winget install LLVM.LLVM\n"
            "    Debian/Ubuntu (apt):    sudo apt install clang-19\n"
            "                            (if unavailable on your distro, use the LLVM apt repo:\n"
            "                                 wget https://apt.llvm.org/llvm.sh && chmod +x llvm.sh && sudo ./llvm.sh 19)\n"
            "    Other:                  https://releases.llvm.org/\n"
            "Or set -DFERN_BUILD_LSP=OFF to skip the LSP.")
    endforeach()
endif()

project(Fern C CXX)

# ---------------------------------------------------------------------------
# Compiler version gate (applies to all builds). Sets a sane minimum for the
# C++20 features fernlib uses while staying permissive enough for embedding.
# ---------------------------------------------------------------------------
if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS 19.30)
        message(FATAL_ERROR
            "Fern requires MSVC 19.30 (Visual Studio 2022 17.0) or newer, "
            "but found MSVC ${CMAKE_CXX_COMPILER_VERSION}.")
    endif()
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS 11)
        message(FATAL_ERROR
            "Fern requires GCC 11 or newer, but found GCC ${CMAKE_CXX_COMPILER_VERSION}.")
    endif()
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")  # matches Clang and AppleClang
    if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS 14)
        message(FATAL_ERROR
            "Fern requires Clang or AppleClang 14 or newer, "
            "but found ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}.")
    endif()
else()
    message(FATAL_ERROR
        "Fern supports MSVC, GCC, or Clang. "
        "Detected '${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}'.")
endif()

# ---------------------------------------------------------------------------
# Strict toolchain gates: only when building the LSP.
# ---------------------------------------------------------------------------
if(FERN_BUILD_LSP)
    if(NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        message(FATAL_ERROR
            "Fern's LSP build requires Clang 19 or newer, but the detected C++ compiler is "
            "'${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}'.\n"
            "Install Clang:\n"
            "    Windows (winget):       winget install LLVM.LLVM\n"
            "    Debian/Ubuntu (apt):    sudo apt install clang-19\n"
            "                            (Ubuntu 22.04 needs apt.llvm.org first)\n"
            "    Other:                  https://releases.llvm.org/\n"
            "Or set -DFERN_BUILD_LSP=OFF to skip the LSP.")
    endif()

    if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS 19)
        message(FATAL_ERROR
            "Fern's LSP build requires Clang 19 or newer, but found Clang ${CMAKE_CXX_COMPILER_VERSION}.\n"
            "Or set -DFERN_BUILD_LSP=OFF to skip the LSP.")
    endif()

    include(CheckCXXSourceCompiles)
    check_cxx_source_compiles("
        #include <version>
        #if defined(__GLIBCXX__) && (!defined(_GLIBCXX_RELEASE) || _GLIBCXX_RELEASE < 12)
        #error \"libstdc++ too old\"
        #endif
        int main() {}
    " FERN_LIBSTDCXX_OK)

    if(NOT FERN_LIBSTDCXX_OK)
        message(FATAL_ERROR
            "Fern's LSP build requires libstdc++ 12 or newer (lsp-framework breaks on libstdc++ 11).\n"
            "    Debian/Ubuntu (apt):    sudo apt install libstdc++-13-dev\n"
            "                            (on Ubuntu 22.04 add the toolchain PPA first:\n"
            "                                 sudo add-apt-repository ppa:ubuntu-toolchain-r/test && sudo apt update)\n"
            "Or set -DFERN_BUILD_LSP=OFF to skip the LSP.")
    endif()
endif()

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_compile_definitions(FERN_DEBUG)
endif()

# ---------------------------------------------------------------------------
# Fuzzer toolchain. libFuzzer is part of Clang's compiler-rt and is enabled
# by passing -fsanitize=fuzzer. We pair it with AddressSanitizer and
# UndefinedBehaviorSanitizer so memory errors and UB surface as crashes.
# Coverage and sanitizer compile flags are applied globally so fernlib gets
# instrumented; the libFuzzer entry point (-fsanitize=fuzzer link flag) is
# added only on the fernfuzz target so it does not clash with other mains.
# ---------------------------------------------------------------------------
if(FERN_BUILD_FUZZER)
    if(NOT CMAKE_CXX_COMPILER_ID MATCHES "Clang")
        message(FATAL_ERROR
            "FERN_BUILD_FUZZER requires Clang (libFuzzer ships with Clang's compiler-rt), "
            "but the detected C++ compiler is "
            "'${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}'.\n"
            "Set CC and CXX to clang and clang++ before configuring, "
            "or set -DFERN_BUILD_FUZZER=OFF.")
    endif()

    if(MSVC)
        message(FATAL_ERROR
            "FERN_BUILD_FUZZER requires the GCC style Clang frontend (clang++), "
            "not clang-cl. Reconfigure with clang.exe / clang++.exe as CC / CXX.")
    endif()

    # libFuzzer's prebuilt runtime on Windows (clang_rt.fuzzer-x86_64.lib) is
    # built against the static release MSVC CRT (/MT). The project's prebuilt
    # LLVM uses the dynamic CRT (/MD), so the fuzzer cannot link both at
    # once. The fuzzer targets the front end (lexer, parser, binder), which
    # does not need LLVM, so disable LLVM and libffi here to avoid the
    # conflict.
    if(WIN32)
        set(FERN_BUILD_LLVM   OFF CACHE BOOL "" FORCE)
        set(FERN_BUILD_LIBFFI OFF CACHE BOOL "" FORCE)
        message(STATUS
            "FERN_BUILD_FUZZER: forced FERN_BUILD_LLVM=OFF and "
            "FERN_BUILD_LIBFFI=OFF on Windows; libFuzzer requires the static "
            "CRT and the prebuilt LLVM uses the dynamic CRT")
    endif()

    # libFuzzer's prebuilt runtime is built against the static release CRT
    # (/MT). Under CMake policy CMP0091 NEW (default since 3.15), CRT
    # selection is driven by CMAKE_MSVC_RUNTIME_LIBRARY independently of
    # the build type, so we can force /MT here while still letting users
    # choose Debug / RelWithDebInfo / Release for the optimization level.
    if(WIN32)
        set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded" CACHE STRING "" FORCE)
    endif()

    # Default to RelWithDebInfo for fast fuzzing. Users who want full debug
    # info (no inlining, all variables visible in the debugger) can configure
    # with -DCMAKE_BUILD_TYPE=Debug; this still produces a /MT binary so it
    # links against libFuzzer correctly.
    if(WIN32 AND NOT CMAKE_BUILD_TYPE)
        set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "" FORCE)
        message(STATUS
            "FERN_BUILD_FUZZER: defaulted CMAKE_BUILD_TYPE=RelWithDebInfo. "
            "Use -DCMAKE_BUILD_TYPE=Debug for easier debugging.")
    endif()

    # Keep internal assertions on for fuzzing even when not in a Debug build.
    add_compile_definitions(FERN_DEBUG)

    # Optimization level comes from CMAKE_BUILD_TYPE (Debug = -O0, RelWithDebInfo
    # = -O2, Release = -O3). We only add the sanitizer + debug info flags
    # explicitly so they apply regardless of build type.
    add_compile_options(
        -fsanitize=fuzzer-no-link,address
        -fno-omit-frame-pointer
        -g
    )
    add_link_options(-fsanitize=address)
endif()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

if(FERN_BUILD_LLVM)
    include(cmake/FetchLLVM.cmake)
    find_package(LLVM CONFIG)
    if(LLVM_FOUND)
        set(LLVM_AVAILABLE TRUE)
        if(WIN32)
            add_link_options("LINKER:/ignore:4099")
        endif()

        if(UNIX AND NOT APPLE AND NOT TARGET zstd::libzstd_shared)
            message(FATAL_ERROR
                "LLVM requires zstd, but it is not installed.\n"
                "    Debian/Ubuntu (apt):    sudo apt install libzstd-dev")
        endif()
    else()
        set(LLVM_AVAILABLE FALSE)
    endif()
else()
    set(LLVM_AVAILABLE FALSE)
endif()

if(FERN_BUILD_LIBFFI)
    include(FetchContent)
    FetchContent_Declare(
        libffi
        GIT_REPOSITORY https://github.com/libffi/libffi.git
        GIT_TAG        v3.5.2
    )
    # libffi/libffi has no top-level CMakeLists, so MakeAvailable just
    # populates the source tree (no add_subdirectory) and we drive the build
    # ourselves via BuildLibffi.cmake.
    FetchContent_MakeAvailable(libffi)
    include(cmake/BuildLibffi.cmake)
else()
    set(LIBFFI_AVAILABLE FALSE)
endif()

add_subdirectory(fernlib)

if(FERN_BUILD_CLI)
    add_subdirectory(ferncli)
endif()

if(FERN_BUILD_LSP)
    include(FetchContent)

    FetchContent_Declare(
        lsp-framework
        GIT_REPOSITORY https://github.com/leon-bckl/lsp-framework.git
        GIT_TAG        1.3.0
    )

    set(LSP_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(lsp-framework)

    add_subdirectory(fernlsp)
endif()

if(FERN_BUILD_FUZZER)
    add_subdirectory(fernfuzz)
endif()
