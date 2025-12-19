cmake_minimum_required(VERSION 3.16)
project(Fern C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Define FERN_DEBUG macro for debug builds
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_compile_definitions(FERN_DEBUG)
endif()

# Try to find LLVM, but make it optional for basic functionality
find_package(LLVM CONFIG)

if(LLVM_FOUND)
    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
    message(STATUS "LLVM Include Dirs: ${LLVM_INCLUDE_DIRS}")
    include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
    set(LLVM_AVAILABLE TRUE)
else()
    message(STATUS "LLVM not found - building without JIT functionality")
    set(LLVM_AVAILABLE FALSE)
endif()

include_directories(include)

if(LLVM_AVAILABLE)
    set(LLVM_LINK_COMPONENTS
        # Core components
        Core
        Support
        IRReader
        
        # Target and code generation
        Target
        TargetParser
        CodeGen
        MC
        
        # Common targets only
        X86          # Intel/AMD processors
        
        # ORC JIT components
        OrcJIT
        ExecutionEngine
        RuntimeDyld
        JITLink
        OrcTargetProcess
        
        # Optimization passes
        Passes
        TransformUtils
        Analysis
        ScalarOpts
        InstCombine
        IPO
        Vectorize
    )

    # Check if we should use monolithic LLVM library
    if(UNIX AND NOT APPLE)
        # On Linux, check if component libraries exist
        find_library(LLVM_CORE_LIB LLVMCore PATHS ${LLVM_LIBRARY_DIR} NO_DEFAULT_PATH)
        if(NOT LLVM_CORE_LIB)
            # Use monolithic library
            message(STATUS "Using monolithic LLVM library")
            set(LLVM_LIBS LLVM)
        else()
            # Use component libraries
            
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
endif()

set(SOURCE_FILES
    # Parser Implementation
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
    
    # Binding
    src/binding/bound_tree_builder.cpp
    src/binding/conversion_inserter.cpp

    # HLIR
    src/hlir/bound_to_hlir.cpp
    # src/hlir/mem2reg.cpp

    # Code Generator
    src/codegen/llvm_ir_builder.cpp
    src/codegen/codegen_module.cpp
    src/codegen/codegen_function.cpp
    src/codegen/hlir_codegen.cpp

    # Runtime
    runtime/runtime.cpp
    
    # Common Utilities
    src/common/logger.cpp
    src/common/error.cpp

    src/compiler.cpp
    src/jit.cpp
    src/compiled_module.cpp
    src/test_runner.cpp
)

set(RUNTIME_FILES

)


# Main executable
add_executable(Fern ${SOURCE_FILES} ${RUNTIME_FILES} main.cpp)

if(LLVM_AVAILABLE)
    target_link_libraries(Fern PRIVATE ${LLVM_LIBS})
endif()

target_include_directories(Fern PRIVATE "src" "lib")
