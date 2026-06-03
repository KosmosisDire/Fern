#pragma once

#include "flir.hpp"
#include <sstream>

namespace Fern
{

#pragma region Pretty Formatter

class FlirPrettyFormatter : public DefaultFlirVisitor
{
    std::ostringstream out;
    int indent = 0;

    void write_indent();
    void write_child(FlirNode* node);
    void write_child(FlirExpr* node);
    void write_args(const std::vector<FlirExpr*>& args);
    std::string method_label(MethodSymbol* method);
    std::string local_label(FlirLocal* local);

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
    void visit(FlirConst* node) override;
    void visit(FlirLoadLocal* node) override;
    void visit(FlirLoadField* node) override;
    void visit(FlirCall* node) override;
    void visit(FlirIntrinsic* node) override;
    void visit(FlirCast* node) override;
    void visit(FlirAlloc* node) override;
    void visit(FlirSequence* node) override;

    void visit(FlirBlock* node) override;
    void visit(FlirStoreLocal* node) override;
    void visit(FlirStoreField* node) override;
    void visit(FlirExprStmt* node) override;
    void visit(FlirIf* node) override;
    void visit(FlirLoop* node) override;
    void visit(FlirBreak* node) override;
    void visit(FlirReturn* node) override;

    static std::string format(FlirMethod* method);
    static std::string format(FlirNode* node);
};

#pragma region Debug Formatter

class FlirDebugFormatter : public DefaultFlirVisitor
{
    std::ostringstream out;
    int indent = 0;
    bool suppressNextIndent = false;

    void write_indent();
    void open_block();
    void close_block();
    void begin_node(FlirNode* node);
    void begin_node(FlirNode* node, std::string_view extra);
    void write_child(std::string_view name, FlirNode* node, bool addComma = false);
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

    std::string type_attr(FlirExpr* expr);
    std::string symbol_label(Symbol* sym);
    std::string method_label(MethodSymbol* method);
    std::string local_label(FlirLocal* local);

public:
    void visit(FlirConst* node) override;
    void visit(FlirLoadLocal* node) override;
    void visit(FlirLoadField* node) override;
    void visit(FlirCall* node) override;
    void visit(FlirIntrinsic* node) override;
    void visit(FlirCast* node) override;
    void visit(FlirAlloc* node) override;
    void visit(FlirSequence* node) override;

    void visit(FlirBlock* node) override;
    void visit(FlirStoreLocal* node) override;
    void visit(FlirStoreField* node) override;
    void visit(FlirExprStmt* node) override;
    void visit(FlirIf* node) override;
    void visit(FlirLoop* node) override;
    void visit(FlirBreak* node) override;
    void visit(FlirReturn* node) override;

    static std::string format(FlirMethod* method);
    static std::string format(FlirNode* node);
};

}
