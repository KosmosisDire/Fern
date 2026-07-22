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

public:
    void visit(FlirConst* node) override;
    void visit(FlirLocalAddr* node) override;
    void visit(FlirFieldAddr* node) override;
    void visit(FlirElemAddr* node) override;
    void visit(FlirLoad* node) override;
    void visit(FlirCall* node) override;
    void visit(FlirIntrinsic* node) override;
    void visit(FlirCast* node) override;
    void visit(FlirAlloc* node) override;
    void visit(FlirSequence* node) override;

    void visit(FlirBlock* node) override;
    void visit(FlirStore* node) override;
    void visit(FlirCopy* node) override;
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
    void visit(FlirLocalAddr* node) override;
    void visit(FlirFieldAddr* node) override;
    void visit(FlirElemAddr* node) override;
    void visit(FlirLoad* node) override;
    void visit(FlirCall* node) override;
    void visit(FlirIntrinsic* node) override;
    void visit(FlirCast* node) override;
    void visit(FlirAlloc* node) override;
    void visit(FlirSequence* node) override;

    void visit(FlirBlock* node) override;
    void visit(FlirStore* node) override;
    void visit(FlirCopy* node) override;
    void visit(FlirExprStmt* node) override;
    void visit(FlirIf* node) override;
    void visit(FlirLoop* node) override;
    void visit(FlirBreak* node) override;
    void visit(FlirReturn* node) override;

    static std::string format(FlirMethod* method);
    static std::string format(FlirNode* node);
};

}
