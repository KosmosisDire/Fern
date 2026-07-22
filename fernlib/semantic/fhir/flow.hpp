#pragma once

#include "fhir.hpp"
#include <common/diagnostic.hpp>
#include <unordered_set>

namespace Fern
{

struct MethodSymbol;
struct NamedTypeSymbol;

class FlowAnalyzer
{
    // Definite assignment state along one control flow path
    struct State
    {
        std::unordered_set<const Symbol*> assigned;
        bool reachable = true;
    };

    Diagnostics& diag;
    NamedTypeSymbol* ctorType = nullptr;

    bool check_block(FhirBlock* block);
    bool check_stmt(FhirStmt* stmt);
    bool check_if(FhirIfStmt* stmt);
    bool check_while(FhirWhileStmt* stmt);
    bool is_constant_true(FhirExpr* expr);
    bool is_constant_false(FhirExpr* expr);

    void da_block(FhirBlock* block, State& state);
    void da_stmt(FhirStmt* stmt, State& state);
    void da_if(FhirIfStmt* stmt, State& state);
    void da_expr(FhirExpr* expr, State& state);
    void da_assign(FhirExpr* target, FhirExpr* value, State& state);
    void da_field_read(FhirFieldRefExpr* expr, State& state);
    void check_this_escape(FhirExpr* expr, State& state);
    void check_ctor_complete(State& state, const Span& span);
    static State join(State&& a, State&& b);

    explicit FlowAnalyzer(Diagnostics& diag);

public:
    static void analyze(FhirMethod* method, Diagnostics& diag);
};

}
