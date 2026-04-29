set(FERNLIB_SOURCES

    # global common
    ${CMAKE_SOURCE_DIR}/common/logger.cpp
    # ${CMAKE_SOURCE_DIR}/common/source_mutator.cpp

    # common
    common/compilation.cpp

    # semantic
    semantic/symbol/symbol.cpp
    semantic/symbol/symbol_lookup.cpp
    semantic/symbol/symbol_fmt.cpp
    semantic/symbol/overload.cpp
    semantic/symbol/table.cpp
    semantic/context.cpp
    semantic/binder/binder.cpp
    semantic/binder/binder_attributes.cpp
    semantic/binder/binder_exprs.cpp
    semantic/binder/binder_literals.cpp
    semantic/binder/binder_calls.cpp
    semantic/binder/binder_initializers.cpp
    semantic/binder/binder_stmts.cpp
    semantic/binder/namespace_binder.cpp
    semantic/binder/type_binder.cpp
    semantic/binder/method_binder.cpp
    semantic/binder/block_binder.cpp
    semantic/binder/root_binder.cpp
    semantic/binder/binder_pipeline.cpp
    semantic/fhir/fhir_constant.cpp
    semantic/fhir/flow.cpp
    semantic/fhir/fmt.cpp

    # source
    source/file.cpp
    # source/fuzzer.cpp
    source/span.cpp
    source/walker.cpp

    # token
    # token/token.cpp
    token/walker.cpp

    # lexer
    lexer/lexer.cpp

    # parser
    parser/parser.cpp

)

add_library(fernlib STATIC ${FERNLIB_SOURCES})

target_include_directories(fernlib PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/semantic
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
    target_link_libraries(fernlib PUBLIC ffi_static)
    # FFI_BUILDING tells partic2 libffi's ffi.h to drop the dllimport decoration
    # since we're statically linking.
    target_compile_definitions(fernlib PUBLIC FERN_LIBFFI_AVAILABLE FFI_BUILDING)
endif()
