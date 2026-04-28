cmake_minimum_required(VERSION 3.18)

if(NOT CMAKE_GENERATOR MATCHES "Ninja")
    message(FATAL_ERROR
        "Fern requires the Ninja generator, but CMake was invoked with '${CMAKE_GENERATOR}'.\n"
        "Install Ninja:\n"
        "    Windows (winget):       winget install Ninja-build.Ninja\n"
        "    Debian/Ubuntu (apt):    sudo apt install ninja-build\n"
        "    Other:                  https://ninja-build.org/\n"
        "Then reconfigure using a Fern preset, e.g.:\n"
        "    cmake --preset windows-debug\n"
        "    cmake --preset linux-debug")
endif()

find_program(NINJA_EXECUTABLE ninja)
if(NOT NINJA_EXECUTABLE)
    message(FATAL_ERROR
        "Fern requires Ninja, but the 'ninja' executable was not found on PATH.\n"
        "Install Ninja:\n"
        "    Windows (winget):       winget install Ninja-build.Ninja\n"
        "    Debian/Ubuntu (apt):    sudo apt install ninja-build\n"
        "    Other:                  https://ninja-build.org/\n"
        "Then reconfigure using a Fern preset, e.g.:\n"
        "    cmake --preset windows-debug\n"
        "    cmake --preset linux-debug")
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
        "Fern requires Clang 19 or newer, but the configured ${_lang} compiler "
        "'${_compiler}' was not found.\n"
        "Install Clang:\n"
        "    Windows (winget):       winget install LLVM.LLVM\n"
        "    Debian/Ubuntu (apt):    sudo apt install clang-19\n"
        "                            (if unavailable on your distro, use the LLVM apt repo:\n"
        "                                 wget https://apt.llvm.org/llvm.sh && chmod +x llvm.sh && sudo ./llvm.sh 19)\n"
        "    Other:                  https://releases.llvm.org/\n")
endforeach()

project(Fern C CXX)

if(NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    message(FATAL_ERROR
        "Fern requires Clang 19 or newer, but the detected C++ compiler is "
        "'${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}' (${CMAKE_CXX_COMPILER}).\n"
        "Install Clang:\n"
        "    Windows (winget):       winget install LLVM.LLVM\n"
        "    Debian/Ubuntu (apt):    sudo apt install clang-19\n"
        "                            (if unavailable on your distro, use the LLVM apt repo:\n"
        "                                 wget https://apt.llvm.org/llvm.sh && chmod +x llvm.sh && sudo ./llvm.sh 19)\n"
        "    Other:                  https://releases.llvm.org/\n"
        "Then reconfigure using a Fern preset, e.g.:\n"
        "    cmake --preset windows-debug\n"
        "    cmake --preset linux-debug\n")
endif()

if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS 19)
    message(FATAL_ERROR
        "Fern requires Clang 19 or newer, but found Clang ${CMAKE_CXX_COMPILER_VERSION} "
        "at '${CMAKE_CXX_COMPILER}'.\n"
        "Upgrade Clang:\n"
        "    Windows (winget):       winget upgrade LLVM.LLVM\n"
        "    Debian/Ubuntu (apt):    sudo apt install clang-19\n"
        "                            (if unavailable on your distro, use the LLVM apt repo:\n"
        "                                 wget https://apt.llvm.org/llvm.sh && chmod +x llvm.sh && sudo ./llvm.sh 19)\n"
        "    Other:                  https://releases.llvm.org/\n"
        "Delete the existing build directory, then reconfigure with a Fern preset, e.g.:\n"
        "    cmake --preset windows-debug\n"
        "    cmake --preset linux-debug")
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
        "Fern requires libstdc++ 12 or newer (lsp-framework breaks on libstdc++ 11).\n"
        "    Debian/Ubuntu (apt):    sudo apt install libstdc++-13-dev\n"
        "                            (on Ubuntu 22.04 add the toolchain PPA first:\n"
        "                                 sudo add-apt-repository ppa:ubuntu-toolchain-r/test && sudo apt update)")
endif()

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_compile_definitions(FERN_DEBUG)
endif()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

option(FERN_BUILD_LLVM "Build with LLVM backend" ON)
option(FERN_BUILD_LIBFFI "Build with libffi support" ON)

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
        GIT_REPOSITORY https://github.com/chrysante/ffi.git
        GIT_TAG        main
    )
    set(BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)
    set(FFI_USE_SHARED_LIBS OFF CACHE BOOL "" FORCE)

    FetchContent_GetProperties(libffi)
    if(NOT libffi_POPULATED)
        FetchContent_Populate(libffi)
        file(READ "${libffi_SOURCE_DIR}/CMakeLists.txt" FFI_CMAKE)
        string(REPLACE
            "CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL \"x64\""
            "CMAKE_HOST_SYSTEM_PROCESSOR MATCHES \"x64|AMD64\""
            FFI_CMAKE "${FFI_CMAKE}")
        file(WRITE "${libffi_SOURCE_DIR}/CMakeLists.txt" "${FFI_CMAKE}")
        add_subdirectory(${libffi_SOURCE_DIR} ${libffi_BINARY_DIR} EXCLUDE_FROM_ALL)
    endif()
    set(LIBFFI_AVAILABLE TRUE)
else()
    set(LIBFFI_AVAILABLE FALSE)
endif()

add_subdirectory(fernlib)
add_subdirectory(ferncli)

# LSP Server

option(FERN_BUILD_LSP "Build the Fern LSP server" ON)

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
