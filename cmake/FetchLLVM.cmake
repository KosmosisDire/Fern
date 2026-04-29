# FetchLLVM.cmake - Automatically download and configure prebuilt LLVM

# If the consumer has already configured LLVM (e.g. when embedding Fern via
# add_subdirectory and pointing at their own LLVM install), respect that.
if(LLVM_DIR)
    return()
endif()

set(LLVM_VERSION "20.1.8")
set(LLVM_DEPS_DIR "${CMAKE_BINARY_DIR}/_deps")
set(LLVM_PACKAGE_NAME "")
set(LLVM_ARCHIVE_NAME "")
set(LLVM_DOWNLOAD_URL "")

# Pick the right prebuilt for this (OS, arch). Sources:
#   Windows: vovkos/llvm-package-windows  - has Debug + static-CRT variants
#   Linux/macOS: official llvm/llvm-project releases
if(WIN32 AND CMAKE_SIZEOF_VOID_P EQUAL 8)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(LLVM_PACKAGE_NAME "llvm-${LLVM_VERSION}-windows-amd64-msvc17-msvcrt-dbg")
    else()
        set(LLVM_PACKAGE_NAME "llvm-${LLVM_VERSION}-windows-amd64-msvc17-msvcrt")
    endif()
    set(LLVM_ARCHIVE_NAME "${LLVM_PACKAGE_NAME}.7z")
    set(LLVM_DOWNLOAD_URL "https://github.com/vovkos/llvm-package-windows/releases/download/llvm-${LLVM_VERSION}/${LLVM_ARCHIVE_NAME}")
elseif(APPLE AND CMAKE_SYSTEM_PROCESSOR MATCHES "arm64|aarch64")
    set(LLVM_PACKAGE_NAME "LLVM-${LLVM_VERSION}-macOS-ARM64")
    set(LLVM_ARCHIVE_NAME "${LLVM_PACKAGE_NAME}.tar.xz")
    set(LLVM_DOWNLOAD_URL "https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VERSION}/${LLVM_ARCHIVE_NAME}")
elseif(UNIX AND NOT APPLE AND CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|amd64|AMD64")
    set(LLVM_PACKAGE_NAME "LLVM-${LLVM_VERSION}-Linux-X64")
    set(LLVM_ARCHIVE_NAME "${LLVM_PACKAGE_NAME}.tar.xz")
    set(LLVM_DOWNLOAD_URL "https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VERSION}/${LLVM_ARCHIVE_NAME}")
elseif(UNIX AND NOT APPLE AND CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64")
    set(LLVM_PACKAGE_NAME "LLVM-${LLVM_VERSION}-Linux-ARM64")
    set(LLVM_ARCHIVE_NAME "${LLVM_PACKAGE_NAME}.tar.xz")
    set(LLVM_DOWNLOAD_URL "https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VERSION}/${LLVM_ARCHIVE_NAME}")
endif()

if(NOT LLVM_PACKAGE_NAME)
    message(STATUS "FetchLLVM: no prebuilt available for this platform/arch; "
                   "falling back to find_package(LLVM). Set LLVM_DIR if needed.")
    return()
endif()

set(LLVM_INSTALL_DIR "${LLVM_DEPS_DIR}/${LLVM_PACKAGE_NAME}")
set(LLVM_CMAKE_DIR "${LLVM_INSTALL_DIR}/lib/cmake/llvm")

if(NOT EXISTS "${LLVM_CMAKE_DIR}/LLVMConfig.cmake")
    message(STATUS "LLVM not found, downloading prebuilt LLVM ${LLVM_VERSION}...")

    set(LLVM_ARCHIVE_PATH "${LLVM_DEPS_DIR}/${LLVM_ARCHIVE_NAME}")

    file(MAKE_DIRECTORY "${LLVM_DEPS_DIR}")

    if(NOT EXISTS "${LLVM_ARCHIVE_PATH}")
        message(STATUS "Downloading ${LLVM_DOWNLOAD_URL}...")
        file(DOWNLOAD
            "${LLVM_DOWNLOAD_URL}"
            "${LLVM_ARCHIVE_PATH}"
            SHOW_PROGRESS
            STATUS DOWNLOAD_STATUS
        )
        list(GET DOWNLOAD_STATUS 0 STATUS_CODE)
        if(NOT STATUS_CODE EQUAL 0)
            list(GET DOWNLOAD_STATUS 1 ERROR_MESSAGE)
            file(REMOVE "${LLVM_ARCHIVE_PATH}")
            message(FATAL_ERROR "Failed to download LLVM: ${ERROR_MESSAGE}")
        endif()
    endif()

    message(STATUS "Extracting LLVM to ${LLVM_DEPS_DIR}...")
    file(ARCHIVE_EXTRACT
        INPUT "${LLVM_ARCHIVE_PATH}"
        DESTINATION "${LLVM_DEPS_DIR}"
    )

    file(REMOVE "${LLVM_ARCHIVE_PATH}")

    if(NOT EXISTS "${LLVM_CMAKE_DIR}/LLVMConfig.cmake")
        message(FATAL_ERROR
            "LLVM extraction succeeded but LLVMConfig.cmake was not found at "
            "${LLVM_CMAKE_DIR}. The prebuilt archive layout may have changed.")
    endif()

    message(STATUS "LLVM ${LLVM_VERSION} installed to ${LLVM_INSTALL_DIR}")
else()
    message(STATUS "Using cached LLVM from ${LLVM_INSTALL_DIR}")
endif()

# The Linux/macOS LLVM tarballs ship a complete bundled libc++ at
# include/c++/v1/. When fernlib adds LLVM_INCLUDE_DIRS via -isystem the
# bundled libc++ shadows the consumer compiler's libc++ and produces the
# classic "<cstddef> didn't find libc++'s <stddef.h>" error. We don't need
# the bundled libc++ (the consumer compiler brings its own), so move it out
# of the include search path. Runs every configure for idempotence — also
# fixes already-cached LLVM dirs from before this patch.
if(EXISTS "${LLVM_INSTALL_DIR}/include/c++")
    file(RENAME
        "${LLVM_INSTALL_DIR}/include/c++"
        "${LLVM_INSTALL_DIR}/_unused_bundled_cxx_headers")
endif()

set(LLVM_DIR "${LLVM_CMAKE_DIR}" CACHE PATH "Path to LLVM CMake config")
