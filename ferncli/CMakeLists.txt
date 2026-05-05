set(FERNCLI_SOURCES
    main.cpp
    test_runner.cpp
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

add_custom_command(TARGET ferncli POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        $<TARGET_FILE:ferncli>
        ${CMAKE_SOURCE_DIR}/fern$<TARGET_FILE_SUFFIX:ferncli>
    COMMENT "Copying ferncli to project root as fern$<TARGET_FILE_SUFFIX:ferncli>"
)
