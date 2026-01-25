cmake_minimum_required(VERSION 3.18)
project(Fern C CXX)

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
