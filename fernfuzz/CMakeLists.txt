add_executable(fernfuzz main.cpp)

set_target_properties(fernfuzz PROPERTIES
    CXX_STANDARD 20
    RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
)

target_link_libraries(fernfuzz PRIVATE fernlib)

target_link_options(fernfuzz PRIVATE -fsanitize=fuzzer)

if(LLVM_AVAILABLE)
    target_compile_definitions(fernfuzz PRIVATE FERN_LLVM_AVAILABLE)
endif()

if(LIBFFI_AVAILABLE)
    target_compile_definitions(fernfuzz PRIVATE FERN_LIBFFI_AVAILABLE)
endif()

# fern.dict next to the binary for fernfuzz -dict=fern.dict
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/fern.dict
    ${PROJECT_BINARY_DIR}/bin/fern.dict
    COPYONLY
)
