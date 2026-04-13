#include "server.hpp"

#include <lsp/connection.h>
#include <lsp/io/standardio.h>

int main()
{
    auto connection = lsp::Connection{lsp::io::standardIO()};
    FernServer server(connection);
    server.run();
    return 0;
}
