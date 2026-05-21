#pragma once

#include <cstdint>
#include <string_view>
#include <vector>

#include <source/span.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

struct BaseSyntax;
struct TypeSymbol;
struct MethodSymbol;
struct FieldSymbol;
#pragma region Forward Declarations

class FlirVisitor;

struct FlirNode;
struct FlirExpr;
struct FlirStmt;

struct FlirConst;
struct FlirLoadLocal;
struct FlirLoadField;
struct FlirCall;
struct FlirIntrinsic;
struct FlirCast;
struct FlirAlloc;
struct FlirSequence;

struct FlirBlock;
struct FlirAssign;
struct FlirExprStmt;
struct FlirIf;
struct FlirLoop;
struct FlirBreak;
struct FlirReturn;

struct FlirLocal;
struct FlirMethod;

#pragma region Visitor

class FlirVisitor
{
public:
    virtual ~FlirVisitor() = default;

    virtual void visit(FlirConst* node) = 0;
    virtual void visit(FlirLoadLocal* node) = 0;
    virtual void visit(FlirLoadField* node) = 0;
    virtual void visit(FlirCall* node) = 0;
    virtual void visit(FlirIntrinsic* node) = 0;
    virtual void visit(FlirCast* node) = 0;
    virtual void visit(FlirAlloc* node) = 0;
    virtual void visit(FlirSequence* node) = 0;

    virtual void visit(FlirBlock* node) = 0;
    virtual void visit(FlirAssign* node) = 0;
    virtual void visit(FlirExprStmt* node) = 0;
    virtual void visit(FlirIf* node) = 0;
    virtual void visit(FlirLoop* node) = 0;
    virtual void visit(FlirBreak* node) = 0;
    virtual void visit(FlirReturn* node) = 0;
};

#define FLIR_NODE(K, Base) \
    static constexpr int Kind = __LINE__; \
    std::string_view node_name() const override { return #K; } \
    void accept(FlirVisitor* visitor) override { visitor->visit(this); } \
    K() : Base(Kind) {}

#pragma region Base Nodes

struct FlirNode
{
private:
    int kind;

public:
    Span span;
    BaseSyntax* syntax = nullptr;

    FlirNode(int k) : kind(k) {}
    virtual ~FlirNode() = default;
    virtual std::string_view node_name() const = 0;
    virtual void accept(FlirVisitor* visitor) = 0;
    virtual void visit_children(FlirVisitor*) {}

    template<typename T>
    bool is() const { return kind == T::Kind; }

    template<typename T>
    T* as() { return is<T>() ? static_cast<T*>(this) : nullptr; }

    template<typename T>
    const T* as() const { return is<T>() ? static_cast<const T*>(this) : nullptr; }
};

struct FlirExpr : FlirNode
{
    TypeSymbol* type = nullptr;

    FlirExpr(int k) : FlirNode(k) {}
};

struct FlirStmt : FlirNode
{
    FlirStmt(int k) : FlirNode(k) {}
};

#pragma region Locals

struct FlirLocal
{
    std::string_view name;
    TypeSymbol* type = nullptr;
    int index = 0;
};

#pragma region Expressions

struct FlirConst : FlirExpr
{
    FLIR_NODE(FlirConst, FlirExpr)

    ConstantValue value;
};

struct FlirLoadLocal : FlirExpr
{
    FLIR_NODE(FlirLoadLocal, FlirExpr)

    FlirLocal* local = nullptr;
};

struct FlirLoadField : FlirExpr
{
    FLIR_NODE(FlirLoadField, FlirExpr)

    FlirExpr* base = nullptr;
    FieldSymbol* field = nullptr;

    void visit_children(FlirVisitor* v) override
    {
        if (base) base->accept(v);
    }
};

struct FlirCall : FlirExpr
{
    FLIR_NODE(FlirCall, FlirExpr)

    MethodSymbol* method = nullptr;
    FlirExpr* thisArg = nullptr;
    std::vector<FlirExpr*> args;

    void visit_children(FlirVisitor* v) override
    {
        if (thisArg) thisArg->accept(v);
        for (auto* a : args)
            if (a) a->accept(v);
    }
};

struct FlirIntrinsic : FlirExpr
{
    FLIR_NODE(FlirIntrinsic, FlirExpr)

    IntrinsicOp op = IntrinsicOp::Add;
    std::vector<FlirExpr*> args;

    void visit_children(FlirVisitor* v) override
    {
        for (auto* a : args)
            if (a) a->accept(v);
    }
};

struct FlirCast : FlirExpr
{
    FLIR_NODE(FlirCast, FlirExpr)

    FlirExpr* operand = nullptr;
    TypeSymbol* targetType = nullptr;

    void visit_children(FlirVisitor* v) override
    {
        if (operand) operand->accept(v);
    }
};

struct FlirAlloc : FlirExpr
{
    FLIR_NODE(FlirAlloc, FlirExpr)

    TypeSymbol* allocType = nullptr;
};

// Runs sideEffects in order and returns value
// Used by lowering when one statement in FHIR becomes multiple FLIR statements and an expression.
struct FlirSequence : FlirExpr
{
    FLIR_NODE(FlirSequence, FlirExpr)

    std::vector<FlirStmt*> sideEffects;
    FlirExpr* value = nullptr;

    void visit_children(FlirVisitor* v) override
    {
        for (auto* s : sideEffects)
            if (s) s->accept(v);
        if (value) value->accept(v);
    }
};

#pragma region Statements

struct FlirBlock : FlirStmt
{
    FLIR_NODE(FlirBlock, FlirStmt)

    std::vector<FlirStmt*> statements;

    void visit_children(FlirVisitor* v) override
    {
        for (auto* s : statements)
            if (s) s->accept(v);
    }
};

struct FlirAssign : FlirStmt
{
    FLIR_NODE(FlirAssign, FlirStmt)

    // Valid target kinds: FlirLoadLocal, FlirLoadField.
    FlirExpr* target = nullptr;
    FlirExpr* value = nullptr;

    void visit_children(FlirVisitor* v) override
    {
        if (target) target->accept(v);
        if (value) value->accept(v);
    }
};

struct FlirExprStmt : FlirStmt
{
    FLIR_NODE(FlirExprStmt, FlirStmt)

    FlirExpr* expression = nullptr;

    void visit_children(FlirVisitor* v) override
    {
        if (expression) expression->accept(v);
    }
};

struct FlirIf : FlirStmt
{
    FLIR_NODE(FlirIf, FlirStmt)

    FlirExpr* condition = nullptr;
    FlirBlock* thenBlock = nullptr;
    FlirBlock* elseBlock = nullptr;

    void visit_children(FlirVisitor* v) override
    {
        if (condition) condition->accept(v);
        if (thenBlock) thenBlock->accept(v);
        if (elseBlock) elseBlock->accept(v);
    }
};

struct FlirLoop : FlirStmt
{
    FLIR_NODE(FlirLoop, FlirStmt)

    FlirBlock* body = nullptr;

    void visit_children(FlirVisitor* v) override
    {
        if (body) body->accept(v);
    }
};

struct FlirBreak : FlirStmt
{
    FLIR_NODE(FlirBreak, FlirStmt)
};

struct FlirReturn : FlirStmt
{
    FLIR_NODE(FlirReturn, FlirStmt)

    FlirExpr* value = nullptr;

    void visit_children(FlirVisitor* v) override
    {
        if (value) value->accept(v);
    }
};

#pragma region Method

struct FlirMethod
{
    MethodSymbol* symbol = nullptr;
    std::vector<FlirLocal*> parameters;
    std::vector<FlirLocal*> locals;
    FlirBlock* body = nullptr;
};

#pragma region DefaultFlirVisitor

class DefaultFlirVisitor : public FlirVisitor
{
protected:
    virtual void on_visit(FlirNode*) {}

public:
    void visit(FlirConst* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirLoadLocal* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirLoadField* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirCall* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirIntrinsic* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirCast* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirAlloc* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirSequence* node) override { on_visit(node); node->visit_children(this); }

    void visit(FlirBlock* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirAssign* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirExprStmt* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirIf* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirLoop* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirBreak* node) override { on_visit(node); node->visit_children(this); }
    void visit(FlirReturn* node) override { on_visit(node); node->visit_children(this); }
};

}
