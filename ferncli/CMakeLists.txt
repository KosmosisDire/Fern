set(FERNCLI_SOURCES
    main.cpp
)

add_executable(ferncli ${FERNCLI_SOURCES})

set_target_properties(ferncli PROPERTIES
    CXX_STANDARD 20
)

target_link_libraries(ferncli PRIVATE fernlib)

if(LLVM_AVAILABLE)
    target_compile_definitions(ferncli PRIVATE FERN_LLVM_AVAILABLE)
endif()

if(LIBFFI_AVAILABLE)
    target_compile_definitions(ferncli PRIVATE FERN_LIBFFI_AVAILABLE)
endif()

set_target_properties(ferncli PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)
