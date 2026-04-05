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

    bool try_fold_int_arithmetic(IntrinsicOp op, int64_t lhs, int64_t rhs, int64_t& out);
    bool try_fold_int_comparison(IntrinsicOp op, int64_t lhs, int64_t rhs, bool& out);
    bool try_fold_bool_logic(IntrinsicOp op, bool lhs, bool rhs, bool& out);
    FhirExpr* try_fold_intrinsic(FhirIntrinsicExpr* node);
    FhirExpr* fold_expr(FhirExpr* expr);
    void fold_stmt(FhirStmt* stmt);
    void fold_block(FhirBlock* block);

    FhirConstantFolder(AllocArena& arena);

public:
    static void fold(FhirMethod* method, AllocArena& arena);
    static std::optional<int64_t> try_evaluate_constant_int(FhirExpr* expr);
};

}
