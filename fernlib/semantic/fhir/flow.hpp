#pragma once

#include "fhir.hpp"
#include <common/diagnostic.hpp>

namespace Fern
{

struct MethodSymbol;

class FlowAnalyzer
{
    Diagnostics& diag;

    bool check_block(FhirBlock* block);
    bool check_stmt(FhirStmt* stmt);
    bool check_if(FhirIfStmt* stmt);
    bool check_while(FhirWhileStmt* stmt);
    bool is_constant_true(FhirExpr* expr);
    bool is_constant_false(FhirExpr* expr);

    explicit FlowAnalyzer(Diagnostics& diag);

public:
    static void analyze(FhirMethod* method, Diagnostics& diag);
};

}
