add_executable(fernlsp
    main.cpp
    document.hpp
    document.cpp
    server.hpp
    server.cpp
    analysis.hpp
    analysis.cpp
    diagnostics.hpp
    diagnostics.cpp
    hover.hpp
    hover.cpp
)

target_link_libraries(fernlsp PRIVATE fernlib lsp)

set_target_properties(fernlsp PROPERTIES
    CXX_STANDARD 20
    RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
)
