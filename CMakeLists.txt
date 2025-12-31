cmake_minimum_required(VERSION 3.18)
project(Fern C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_compile_definitions(FERN_DEBUG)
endif()

include_directories(include)

# ─────────────────────────────────────────────────────────────────────────────
# FernLib - Core Compiler Library (Backend-free)
# ─────────────────────────────────────────────────────────────────────────────

set(FERN_LIB_SOURCES
    # Parser
    src/parser/lexer.cpp
    src/parser/token_stream.cpp
    src/parser/parser.cpp

    # Semantic Analysis
    src/semantic/type.cpp
    src/semantic/symbol.cpp
    src/semantic/symbol_table.cpp
    src/semantic/type_system.cpp
    src/semantic/symbol_table_builder.cpp
    src/semantic/type_resolver.cpp
    src/semantic/syntax_validator.cpp
    src/semantic/type_topology.cpp
    src/semantic/symbol_table_validator.cpp
    src/semantic/semantic_validator.cpp

    # Binding
    src/binding/bound_tree_builder.cpp
    src/binding/conversion_inserter.cpp

    # FLIR
    src/flir/ir_type.cpp
    src/flir/bound_to_flir.cpp
    src/flir/abi/abi_rules.cpp
    src/flir/abi/abi_lowering_pass.cpp
    src/flir/abi/windows_x64.cpp

    # Common
    src/common/logger.cpp
    src/common/error.cpp
    src/common/file_utils.cpp

    # Core
    src/compiler.cpp
)

add_library(FernLib STATIC ${FERN_LIB_SOURCES})
target_include_directories(FernLib PUBLIC src include)

# ─────────────────────────────────────────────────────────────────────────────
# Backend Options
# ─────────────────────────────────────────────────────────────────────────────

option(FERN_BUILD_VM "Build the VM interpreter backend" ON)
option(FERN_BUILD_LLVM "Build the LLVM backend for native code generation" ON)

# ─────────────────────────────────────────────────────────────────────────────
# VM Backend (Optional)
# ─────────────────────────────────────────────────────────────────────────────

if(FERN_BUILD_VM)
    message(STATUS "VM backend enabled")
    set(VM_AVAILABLE TRUE)

    set(FERN_VM_SOURCES
        src/vm/vm.cpp
        src/vm/vm_backend.cpp
    )

    add_library(FernVM STATIC ${FERN_VM_SOURCES})
    target_include_directories(FernVM PUBLIC src include)
    target_link_libraries(FernVM PUBLIC FernLib)
    target_compile_definitions(FernVM PUBLIC FERN_VM_AVAILABLE)
else()
    message(STATUS "VM backend disabled by FERN_BUILD_VM=OFF")
    set(VM_AVAILABLE FALSE)
endif()

# ─────────────────────────────────────────────────────────────────────────────
# LLVM Backend (Optional)
# ─────────────────────────────────────────────────────────────────────────────

if(FERN_BUILD_LLVM)
    include(cmake/FetchLLVM.cmake)
    find_package(LLVM CONFIG)

    if(LLVM_FOUND)
        message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
        message(STATUS "LLVM Include Dirs: ${LLVM_INCLUDE_DIRS}")
        set(LLVM_AVAILABLE TRUE)
        if(WIN32)
            add_link_options("LINKER:/ignore:4099")
        endif()
    else()
        message(STATUS "LLVM not found - building without LLVM backend")
        set(LLVM_AVAILABLE FALSE)
    endif()
else()
    message(STATUS "LLVM backend disabled by FERN_BUILD_LLVM=OFF")
    set(LLVM_AVAILABLE FALSE)
endif()

if(LLVM_AVAILABLE)
    set(LLVM_LINK_COMPONENTS
        Core
        Support
        IRReader
        Target
        TargetParser
        CodeGen
        MC
        X86
        OrcJIT
        ExecutionEngine
        RuntimeDyld
        JITLink
        OrcTargetProcess
        Passes
        TransformUtils
        Analysis
        ScalarOpts
        InstCombine
        IPO
        Vectorize
    )

    if(UNIX AND NOT APPLE)
        find_library(LLVM_CORE_LIB LLVMCore PATHS ${LLVM_LIBRARY_DIR} NO_DEFAULT_PATH)
        if(NOT LLVM_CORE_LIB)
            message(STATUS "Using monolithic LLVM library")
            set(LLVM_LIBS LLVM)
        else()
            llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_LINK_COMPONENTS})
            message(STATUS "Using LLVM component libraries: ${LLVM_LIBS}")
        endif()
    else()
        llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_LINK_COMPONENTS})
        if(NOT LLVM_LIBS)
            message(STATUS "Using monolithic LLVM library")
            set(LLVM_LIBS LLVM)
        else()
            message(STATUS "Using LLVM component libraries: ${LLVM_LIBS}")
        endif()
    endif()

    set(FERN_LLVM_SOURCES
        src/llvm/llvm_codegen.cpp
        src/llvm/llvm_backend.cpp
        src/llvm/codegen_context.cpp
        src/llvm/jit.cpp
    )

    add_library(FernLLVM STATIC ${FERN_LLVM_SOURCES})
    target_include_directories(FernLLVM PUBLIC src include SYSTEM ${LLVM_INCLUDE_DIRS})
    target_link_libraries(FernLLVM PUBLIC FernLib ${LLVM_LIBS})
    target_compile_definitions(FernLLVM PUBLIC FERN_LLVM_AVAILABLE)

    if(VM_AVAILABLE)
        target_compile_definitions(FernLLVM PUBLIC FERN_VM_AVAILABLE)
    endif()
endif()

# ─────────────────────────────────────────────────────────────────────────────
# Backend Factory Library
# ─────────────────────────────────────────────────────────────────────────────

add_library(FernBackend STATIC
    src/backend/backend.cpp
)
target_include_directories(FernBackend PUBLIC src include)
target_link_libraries(FernBackend PUBLIC FernLib)

if(VM_AVAILABLE)
    target_link_libraries(FernBackend PUBLIC FernVM)
    target_compile_definitions(FernBackend PUBLIC FERN_VM_AVAILABLE)
endif()

if(LLVM_AVAILABLE)
    target_link_libraries(FernBackend PUBLIC FernLLVM)
    target_compile_definitions(FernBackend PUBLIC FERN_LLVM_AVAILABLE)
endif()

# ─────────────────────────────────────────────────────────────────────────────
# Fern CLI Executable
# ─────────────────────────────────────────────────────────────────────────────

set(RUNTIME_SOURCES
    runtime/runtime.cpp
)

add_executable(ferncli
    cli/main.cpp
    src/test_runner.cpp
    ${RUNTIME_SOURCES}
)

target_include_directories(ferncli PRIVATE src lib)
target_link_libraries(ferncli PRIVATE FernBackend)

if(VM_AVAILABLE)
    target_compile_definitions(ferncli PRIVATE FERN_VM_AVAILABLE)
endif()

if(LLVM_AVAILABLE)
    target_compile_definitions(ferncli PRIVATE FERN_LLVM_AVAILABLE)
endif()

# ─────────────────────────────────────────────────────────────────────────────
# Fern LSP Server
# ─────────────────────────────────────────────────────────────────────────────

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

    add_executable(fernlsp
        lsp/main.cpp
    )

    target_link_libraries(fernlsp PRIVATE FernLib lsp)
    target_include_directories(fernlsp PRIVATE src)
endif()
