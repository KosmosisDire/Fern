set(FERNFUZZ_SOURCES
    main.cpp
)

add_executable(fernfuzz ${FERNFUZZ_SOURCES})

set_target_properties(fernfuzz PROPERTIES
    CXX_STANDARD 20
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

target_link_libraries(fernfuzz PRIVATE fernlib)

# libFuzzer's main is added by the fuzzer link flag. Keep this PRIVATE so it
# only lands on the fuzzer target. Compile side coverage and sanitizer
# instrumentation propagate from fernlib's PUBLIC options.
target_link_options(fernfuzz PRIVATE -fsanitize=fuzzer)

if(LLVM_AVAILABLE)
    target_compile_definitions(fernfuzz PRIVATE FERN_LLVM_AVAILABLE)
endif()

if(LIBFFI_AVAILABLE)
    target_compile_definitions(fernfuzz PRIVATE FERN_LIBFFI_AVAILABLE)
endif()

# Drop the token dictionary next to the binary so users can run
# fernfuzz -dict=fern.dict corpus/
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/fern.dict
    ${CMAKE_BINARY_DIR}/bin/fern.dict
    COPYONLY
)

# On Windows, Clang links ASan as a dynamic library, so the binary needs
# clang_rt.asan_dynamic-x86_64.dll on PATH at runtime. Locate it next to
# the compiler and copy it next to fernfuzz.exe so the binary is self
# contained.
if(WIN32)
    get_filename_component(_fern_clang_dir "${CMAKE_CXX_COMPILER}" DIRECTORY)
    file(GLOB _fern_asan_dll
        "${_fern_clang_dir}/../lib/clang/*/lib/windows/clang_rt.asan_dynamic-x86_64.dll"
        "${_fern_clang_dir}/clang_rt.asan_dynamic-x86_64.dll"
    )

    if(_fern_asan_dll)
        list(GET _fern_asan_dll 0 _fern_asan_dll_path)
        add_custom_command(TARGET fernfuzz POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                "${_fern_asan_dll_path}"
                "$<TARGET_FILE_DIR:fernfuzz>"
            COMMENT "Copying ${_fern_asan_dll_path} next to fernfuzz.exe")
    else()
        message(WARNING
            "fernfuzz: clang_rt.asan_dynamic-x86_64.dll not found near "
            "${CMAKE_CXX_COMPILER}. fernfuzz.exe will fail to start with "
            "STATUS_DLL_NOT_FOUND unless that DLL is on PATH.")
    endif()
endif()
