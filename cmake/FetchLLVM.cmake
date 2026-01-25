# FetchLLVM.cmake - Automatically download and configure prebuilt LLVM

set(LLVM_VERSION "20.1.8")
set(LLVM_DEPS_DIR "${CMAKE_BINARY_DIR}/_deps")

if(WIN32)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(LLVM_PACKAGE_NAME "llvm-${LLVM_VERSION}-windows-amd64-msvc17-msvcrt-dbg")
    else()
        set(LLVM_PACKAGE_NAME "llvm-${LLVM_VERSION}-windows-amd64-msvc17-msvcrt")
    endif()
    set(LLVM_ARCHIVE_NAME "${LLVM_PACKAGE_NAME}.7z")
    set(LLVM_DOWNLOAD_URL "https://github.com/vovkos/llvm-package-windows/releases/download/llvm-${LLVM_VERSION}/${LLVM_ARCHIVE_NAME}")
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
            message(FATAL_ERROR "LLVM extraction failed - LLVMConfig.cmake not found")
        endif()

        message(STATUS "LLVM ${LLVM_VERSION} installed to ${LLVM_INSTALL_DIR}")
    else()
        message(STATUS "Using cached LLVM from ${LLVM_INSTALL_DIR}")
    endif()

    set(LLVM_DIR "${LLVM_CMAKE_DIR}" CACHE PATH "Path to LLVM CMake config" FORCE)
endif()
