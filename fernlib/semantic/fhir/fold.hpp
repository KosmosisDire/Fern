#pragma once

#include "fhir.hpp"
#include <optional>
#include <unordered_map>

namespace Fern
{

class AllocArena;
struct LocalSymbol;

class FhirConstantFolder
{
    AllocArena& arena;
    std::unordered_map<LocalSymbol*, LiteralValue> constants;

    template<typename T>
    T* make();

    FhirExpr* fold_expr(FhirExpr* expr);
    void fold_stmt(FhirStmt* stmt);
    void fold_block(FhirBlock* block);
    void fold_if(FhirIfStmt* stmt);
    void fold_while(FhirWhileStmt* stmt);

    FhirConstantFolder(AllocArena& arena);

public:
    static void fold(FhirMethod* method, AllocArena& arena);
    static std::optional<int64_t> try_evaluate_constant_int(FhirExpr* expr);
    static std::optional<float> try_evaluate_constant_float(FhirExpr* expr);
    static std::optional<bool> try_evaluate_constant_bool(FhirExpr* expr);
};

}
