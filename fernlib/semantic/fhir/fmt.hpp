#pragma once

#include "fhir.hpp"
#include <sstream>

namespace Fern
{

class FhirFormatter : public DefaultFhirVisitor
{
    std::ostringstream out;
    int indent = 0;

    void write_indent();
    void write_child(FhirNode* node);
    void write_args(const std::vector<FhirExpr*>& args);
    std::string method_label(MethodSymbol* method);

    static constexpr std::string_view op_symbol(IntrinsicOp op)
    {
        switch (op)
        {
            case IntrinsicOp::Add:          return "+";
            case IntrinsicOp::Sub:          return "-";
            case IntrinsicOp::Mul:          return "*";
            case IntrinsicOp::Div:          return "/";
            case IntrinsicOp::Greater:      return ">";
            case IntrinsicOp::Less:         return "<";
            case IntrinsicOp::GreaterEqual: return ">=";
            case IntrinsicOp::LessEqual:    return "<=";
            case IntrinsicOp::Equal:        return "==";
            case IntrinsicOp::NotEqual:     return "!=";
            case IntrinsicOp::And:          return "&&";
            case IntrinsicOp::Or:           return "||";
            case IntrinsicOp::Negative:     return "-";
            case IntrinsicOp::Positive:     return "+";
            case IntrinsicOp::Not:          return "!";
        }
    }

public:
    void visit(FhirLiteralExpr* node) override;
    void visit(FhirLocalRefExpr* node) override;
    void visit(FhirParamRefExpr* node) override;
    void visit(FhirFieldAccessExpr* node) override;
    void visit(FhirThisExpr* node) override;
    void visit(FhirIntrinsicExpr* node) override;
    void visit(FhirCallExpr* node) override;
    void visit(FhirMethodCallExpr* node) override;
    void visit(FhirObjectCreateExpr* node) override;
    void visit(FhirAssignExpr* node) override;
    void visit(FhirErrorExpr* node) override;

    void visit(FhirBlock* node) override;
    void visit(FhirVarDeclStmt* node) override;
    void visit(FhirExprStmt* node) override;
    void visit(FhirReturnStmt* node) override;
    void visit(FhirIfStmt* node) override;
    void visit(FhirWhileStmt* node) override;

    static std::string format(FhirMethod* method);
};

}
