#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>
#include <source/span.hpp>

namespace Fern
{

struct BaseSyntax;
struct Symbol;
struct TypeSymbol;
struct MethodSymbol;
struct FieldSymbol;
struct ParameterSymbol;
struct LocalSymbol;

#pragma region Forward Declarations

class FhirVisitor;

struct FhirNode;

struct FhirExpr;
struct FhirLiteralExpr;
struct FhirLocalRefExpr;
struct FhirParamRefExpr;
struct FhirFieldAccessExpr;
struct FhirThisExpr;
struct FhirIntrinsicExpr;
struct FhirCallExpr;
struct FhirMethodCallExpr;
struct FhirObjectCreateExpr;
struct FhirAssignExpr;

struct FhirBlock;

struct FhirStmt;
struct FhirVarDeclStmt;
struct FhirExprStmt;
struct FhirReturnStmt;
struct FhirIfStmt;
struct FhirWhileStmt;

struct FhirMethod;

#pragma region Enums

enum class IntrinsicOp
{
    Add,
    Sub,
    Mul,
    Div,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
    And,
    Or,
    Negative,
    Positive,
    Not,
};

constexpr std::string_view format(IntrinsicOp op)
{
    switch (op)
    {
        case IntrinsicOp::Add:          return "Add";
        case IntrinsicOp::Sub:          return "Sub";
        case IntrinsicOp::Mul:          return "Mul";
        case IntrinsicOp::Div:          return "Div";
        case IntrinsicOp::Greater:      return "Greater";
        case IntrinsicOp::Less:         return "Less";
        case IntrinsicOp::GreaterEqual: return "GreaterEqual";
        case IntrinsicOp::LessEqual:    return "LessEqual";
        case IntrinsicOp::Equal:        return "Equal";
        case IntrinsicOp::NotEqual:     return "NotEqual";
        case IntrinsicOp::And:          return "And";
        case IntrinsicOp::Or:           return "Or";
        case IntrinsicOp::Negative:     return "Negative";
        case IntrinsicOp::Positive:     return "Positive";
        case IntrinsicOp::Not:          return "Not";
    }
}

struct LiteralValue
{
    enum class Kind { Int32, Float32, Bool };

    Kind kind = Kind::Int32;
    union
    {
        int32_t intValue = 0;
        float floatValue;
        bool boolValue;
    };

    static LiteralValue make_int(int32_t v)
    {
        LiteralValue lv;
        lv.kind = Kind::Int32;
        lv.intValue = v;
        return lv;
    }

    static LiteralValue make_float(float v)
    {
        LiteralValue lv;
        lv.kind = Kind::Float32;
        lv.floatValue = v;
        return lv;
    }

    static LiteralValue make_bool(bool v)
    {
        LiteralValue lv;
        lv.kind = Kind::Bool;
        lv.boolValue = v;
        return lv;
    }

    std::string format() const
    {
        switch (kind)
        {
            case Kind::Int32:   return std::to_string(intValue);
            case Kind::Float32: return std::to_string(floatValue);
            case Kind::Bool:    return boolValue ? "true" : "false";
        }
    }
};

#pragma region Visitor

class FhirVisitor
{
public:
    virtual ~FhirVisitor() = default;

    virtual void visit(FhirLiteralExpr* node) = 0;
    virtual void visit(FhirLocalRefExpr* node) = 0;
    virtual void visit(FhirParamRefExpr* node) = 0;
    virtual void visit(FhirFieldAccessExpr* node) = 0;
    virtual void visit(FhirThisExpr* node) = 0;
    virtual void visit(FhirIntrinsicExpr* node) = 0;
    virtual void visit(FhirCallExpr* node) = 0;
    virtual void visit(FhirMethodCallExpr* node) = 0;
    virtual void visit(FhirObjectCreateExpr* node) = 0;
    virtual void visit(FhirAssignExpr* node) = 0;

    virtual void visit(FhirBlock* node) = 0;
    virtual void visit(FhirVarDeclStmt* node) = 0;
    virtual void visit(FhirExprStmt* node) = 0;
    virtual void visit(FhirReturnStmt* node) = 0;
    virtual void visit(FhirIfStmt* node) = 0;
    virtual void visit(FhirWhileStmt* node) = 0;
};

#define FHIR_NODE(K, Base) \
    static constexpr int Kind = __LINE__; \
    std::string_view node_name() const override { return #K; } \
    void accept(FhirVisitor* visitor) override { visitor->visit(this); } \
    K() : Base(Kind) {}

#pragma region Base Nodes

struct FhirNode
{
private:
    int kind;

public:
    Span span;
    BaseSyntax* syntax = nullptr;

    FhirNode(int k) : kind(k) {}
    virtual ~FhirNode() = default;
    virtual std::string_view node_name() const = 0;
    virtual void accept(FhirVisitor* visitor) = 0;
    virtual void visit_children(FhirVisitor*) {}

    template<typename T>
    bool is() const { return kind == T::Kind; }

    template<typename T>
    T* as() { return is<T>() ? static_cast<T*>(this) : nullptr; }

    template<typename T>
    const T* as() const { return is<T>() ? static_cast<const T*>(this) : nullptr; }
};

struct FhirExpr : FhirNode
{
    TypeSymbol* type = nullptr;
    FhirExpr(int k) : FhirNode(k) {}
};

struct FhirStmt : FhirNode
{
    FhirStmt(int k) : FhirNode(k) {}
};

#pragma region Expressions

struct FhirLiteralExpr : FhirExpr
{
    FHIR_NODE(FhirLiteralExpr, FhirExpr)

    LiteralValue value;
};

struct FhirLocalRefExpr : FhirExpr
{
    FHIR_NODE(FhirLocalRefExpr, FhirExpr)

    LocalSymbol* local = nullptr;
};

struct FhirParamRefExpr : FhirExpr
{
    FHIR_NODE(FhirParamRefExpr, FhirExpr)

    ParameterSymbol* parameter = nullptr;
};

struct FhirFieldAccessExpr : FhirExpr
{
    FHIR_NODE(FhirFieldAccessExpr, FhirExpr)

    FhirExpr* object = nullptr;
    FieldSymbol* field = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (object) object->accept(v);
    }
};

struct FhirThisExpr : FhirExpr
{
    FHIR_NODE(FhirThisExpr, FhirExpr)
};

struct FhirIntrinsicExpr : FhirExpr
{
    FHIR_NODE(FhirIntrinsicExpr, FhirExpr)

    IntrinsicOp op = IntrinsicOp::Add;
    std::vector<FhirExpr*> args;

    void visit_children(FhirVisitor* v) override
    {
        for (auto* arg : args)
            if (arg) arg->accept(v);
    }
};

struct FhirCallExpr : FhirExpr
{
    FHIR_NODE(FhirCallExpr, FhirExpr)

    MethodSymbol* target = nullptr;
    std::vector<FhirExpr*> arguments;

    void visit_children(FhirVisitor* v) override
    {
        for (auto* arg : arguments)
            if (arg) arg->accept(v);
    }
};

struct FhirMethodCallExpr : FhirExpr
{
    FHIR_NODE(FhirMethodCallExpr, FhirExpr)

    FhirExpr* receiver = nullptr;
    MethodSymbol* method = nullptr;
    std::vector<FhirExpr*> arguments;

    void visit_children(FhirVisitor* v) override
    {
        if (receiver) receiver->accept(v);
        for (auto* arg : arguments)
            if (arg) arg->accept(v);
    }
};

struct FhirObjectCreateExpr : FhirExpr
{
    FHIR_NODE(FhirObjectCreateExpr, FhirExpr)

    MethodSymbol* constructor = nullptr;
    std::vector<FhirExpr*> arguments;

    void visit_children(FhirVisitor* v) override
    {
        for (auto* arg : arguments)
            if (arg) arg->accept(v);
    }
};

struct FhirAssignExpr : FhirExpr
{
    FHIR_NODE(FhirAssignExpr, FhirExpr)

    FhirExpr* target = nullptr;
    FhirExpr* value = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (target) target->accept(v);
        if (value) value->accept(v);
    }
};

#pragma region Block

struct FhirBlock : FhirNode
{
    FHIR_NODE(FhirBlock, FhirNode)

    std::vector<FhirStmt*> statements;

    void visit_children(FhirVisitor* v) override
    {
        for (auto* stmt : statements)
            if (stmt) stmt->accept(v);
    }
};

#pragma region Statements

struct FhirVarDeclStmt : FhirStmt
{
    FHIR_NODE(FhirVarDeclStmt, FhirStmt)

    LocalSymbol* local = nullptr;
    FhirExpr* initializer = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (initializer) initializer->accept(v);
    }
};

struct FhirExprStmt : FhirStmt
{
    FHIR_NODE(FhirExprStmt, FhirStmt)

    FhirExpr* expression = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (expression) expression->accept(v);
    }
};

struct FhirReturnStmt : FhirStmt
{
    FHIR_NODE(FhirReturnStmt, FhirStmt)

    FhirExpr* value = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (value) value->accept(v);
    }
};

struct FhirIfStmt : FhirStmt
{
    FHIR_NODE(FhirIfStmt, FhirStmt)

    FhirExpr* condition = nullptr;
    FhirBlock* thenBlock = nullptr;
    FhirIfStmt* elseIf = nullptr;
    FhirBlock* elseBlock = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (condition) condition->accept(v);
        if (thenBlock) thenBlock->accept(v);
        if (elseIf) elseIf->accept(v);
        if (elseBlock) elseBlock->accept(v);
    }
};

struct FhirWhileStmt : FhirStmt
{
    FHIR_NODE(FhirWhileStmt, FhirStmt)

    FhirExpr* condition = nullptr;
    FhirBlock* body = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (condition) condition->accept(v);
        if (body) body->accept(v);
    }
};

#pragma region Method

struct FhirMethod
{
    MethodSymbol* symbol = nullptr;
    FhirBlock* body = nullptr;
};

#pragma region DefaultFhirVisitor

class DefaultFhirVisitor : public FhirVisitor
{
public:
    void visit(FhirLiteralExpr* node) override { node->visit_children(this); }
    void visit(FhirLocalRefExpr* node) override { node->visit_children(this); }
    void visit(FhirParamRefExpr* node) override { node->visit_children(this); }
    void visit(FhirFieldAccessExpr* node) override { node->visit_children(this); }
    void visit(FhirThisExpr* node) override { node->visit_children(this); }
    void visit(FhirIntrinsicExpr* node) override { node->visit_children(this); }
    void visit(FhirCallExpr* node) override { node->visit_children(this); }
    void visit(FhirMethodCallExpr* node) override { node->visit_children(this); }
    void visit(FhirObjectCreateExpr* node) override { node->visit_children(this); }
    void visit(FhirAssignExpr* node) override { node->visit_children(this); }

    void visit(FhirBlock* node) override { node->visit_children(this); }
    void visit(FhirVarDeclStmt* node) override { node->visit_children(this); }
    void visit(FhirExprStmt* node) override { node->visit_children(this); }
    void visit(FhirReturnStmt* node) override { node->visit_children(this); }
    void visit(FhirIfStmt* node) override { node->visit_children(this); }
    void visit(FhirWhileStmt* node) override { node->visit_children(this); }
};

}

