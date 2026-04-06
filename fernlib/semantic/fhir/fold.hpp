#pragma once

#include "fhir.hpp"
#include <optional>

namespace Fern
{

class AllocArena;

class FhirConstantFolder
{
    AllocArena& arena;

    template<typename T>
    T* make();

    FhirExpr* fold_expr(FhirExpr* expr);
    void fold_stmt(FhirStmt* stmt);
    void fold_block(FhirBlock* block);

    FhirConstantFolder(AllocArena& arena);

public:
    static void fold(FhirMethod* method, AllocArena& arena);
    static std::optional<int64_t> try_evaluate_constant_int(FhirExpr* expr);
    static std::optional<float> try_evaluate_constant_float(FhirExpr* expr);
    static std::optional<bool> try_evaluate_constant_bool(FhirExpr* expr);
};

}
