#pragma once

#include "fhir.hpp"
#include <sstream>

namespace Fern
{

#pragma region Pretty Formatter

class FhirPrettyFormatter : public DefaultFhirVisitor
{
    std::ostringstream out;
    int indent = 0;

    void write_indent();
    void write_child(FhirNode* node);
    void write_child(FhirExpr* node);
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
    void visit(FhirFieldRefExpr* node) override;
    void visit(FhirThisExpr* node) override;
    void visit(FhirIntrinsicExpr* node) override;
    void visit(FhirCallExpr* node) override;
    void visit(FhirConstructionExpr* node) override;
    void visit(FhirAssignExpr* node) override;
    void visit(FhirCastExpr* node) override;
    void visit(FhirErrorExpr* node) override;
    void visit(FhirNamespaceRefExpr* node) override;
    void visit(FhirMethodGroupRefExpr* node) override;
    void visit(FhirMethodRefExpr* node) override;
    void visit(FhirTypeRef* node) override;

    void visit(FhirBlock* node) override;
    void visit(FhirVarDeclStmt* node) override;
    void visit(FhirExprStmt* node) override;
    void visit(FhirReturnStmt* node) override;
    void visit(FhirIfStmt* node) override;
    void visit(FhirWhileStmt* node) override;

    static std::string format(FhirMethod* method);
    static std::string format(FhirNode* node);
};

#pragma region Debug Formatter

class FhirDebugFormatter : public DefaultFhirVisitor
{
    std::ostringstream out;
    int indent = 0;
    bool suppressNextIndent = false;

    void write_indent();
    void open_block();
    void close_block();
    void begin_node(FhirNode* node);
    void begin_node(FhirNode* node, std::string_view extra);
    void write_child(std::string_view name, FhirNode* node, bool addComma = false);
    void write_field(std::string_view name, std::string_view value, bool addComma = false);

    template<typename T>
    void write_children(std::string_view name, const std::vector<T*>& nodes, bool addComma = false)
    {
        write_indent();
        out << name << ": [";
        if (!nodes.empty())
        {
            out << "\n";
            ++indent;
            for (auto* node : nodes)
            {
                if (node)
                {
                    node->accept(this);
                    out << "\n";
                }
            }
            --indent;
            write_indent();
        }
        out << "]";
        if (addComma) out << ",";
        out << "\n";
    }

    std::string type_attr(FhirExpr* expr);
    std::string symbol_label(Symbol* sym);
    std::string method_label(MethodSymbol* method);

public:
    void visit(FhirLiteralExpr* node) override;
    void visit(FhirLocalRefExpr* node) override;
    void visit(FhirParamRefExpr* node) override;
    void visit(FhirFieldRefExpr* node) override;
    void visit(FhirThisExpr* node) override;
    void visit(FhirIntrinsicExpr* node) override;
    void visit(FhirCallExpr* node) override;
    void visit(FhirConstructionExpr* node) override;
    void visit(FhirAssignExpr* node) override;
    void visit(FhirCastExpr* node) override;
    void visit(FhirErrorExpr* node) override;
    void visit(FhirNamespaceRefExpr* node) override;
    void visit(FhirMethodGroupRefExpr* node) override;
    void visit(FhirMethodRefExpr* node) override;
    void visit(FhirTypeRef* node) override;

    void visit(FhirBlock* node) override;
    void visit(FhirVarDeclStmt* node) override;
    void visit(FhirExprStmt* node) override;
    void visit(FhirReturnStmt* node) override;
    void visit(FhirIfStmt* node) override;
    void visit(FhirWhileStmt* node) override;

    static std::string format(FhirMethod* method);
    static std::string format(FhirNode* node);
};

}
