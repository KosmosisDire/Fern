set(FERNLIB_SOURCES

    # global common
    ${CMAKE_SOURCE_DIR}/common/logger.cpp

    #common

    #source
    source/file.cpp
    source/span.cpp
    source/walker.cpp

    # token
    token/token.cpp
    token/walker.cpp

    # lexer
    lexer/lexer.cpp

    # parser
    parser/parser.cpp

)

add_library(fernlib STATIC ${FERNLIB_SOURCES})

target_include_directories(fernlib PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/common
)

set_target_properties(fernlib PROPERTIES
    VERSION 1.0.0
    SOVERSION 1
    CXX_STANDARD 20
)

if(LLVM_AVAILABLE)
    set(LLVM_LINK_COMPONENTS
        Core Support IRReader Target TargetParser CodeGen MC X86
        OrcJIT ExecutionEngine RuntimeDyld JITLink OrcTargetProcess
        Passes TransformUtils Analysis ScalarOpts InstCombine IPO Vectorize
    )

    if(UNIX AND NOT APPLE)
        find_library(LLVM_CORE_LIB LLVMCore PATHS ${LLVM_LIBRARY_DIR} NO_DEFAULT_PATH)
        if(NOT LLVM_CORE_LIB)
            set(LLVM_LIBS LLVM)
        else()
            llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_LINK_COMPONENTS})
        endif()
    else()
        llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_LINK_COMPONENTS})
        if(NOT LLVM_LIBS)
            set(LLVM_LIBS LLVM)
        endif()
    endif()

    target_include_directories(fernlib SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS})
    target_link_libraries(fernlib PUBLIC ${LLVM_LIBS})
    target_compile_definitions(fernlib PUBLIC FERN_LLVM_AVAILABLE)
endif()

if(LIBFFI_AVAILABLE)
    target_link_libraries(fernlib PUBLIC libffi)
    target_compile_definitions(fernlib PUBLIC FERN_LIBFFI_AVAILABLE FFI_STATIC_BUILD)
    target_compile_definitions(libffi PRIVATE FFI_STATIC_BUILD)
endif()
