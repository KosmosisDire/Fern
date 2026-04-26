#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>
#include <source/span.hpp>

namespace Fern
{

struct BaseSyntax;
struct Symbol;
struct TypeSymbol;
struct NamespaceSymbol;
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
struct FhirFieldRefExpr;
struct FhirThisExpr;
struct FhirIntrinsicExpr;
struct FhirCallExpr;
struct FhirConstructionExpr;
struct FhirAssignExpr;
struct FhirCastExpr;
struct FhirErrorExpr;
struct FhirNamespaceRefExpr;
struct FhirMethodGroupRefExpr;
struct FhirMethodRefExpr;

struct FhirBlock;

struct FhirStmt;
struct FhirVarDeclStmt;
struct FhirExprStmt;
struct FhirReturnStmt;
struct FhirIfStmt;
struct FhirWhileStmt;

struct FhirTypeRef;

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

struct ConstantValue
{
    enum class Kind { Int, Float, Bool, String };

    Kind kind = Kind::Int;
    union
    {
        int64_t intValue = 0;
        double floatValue;
        bool boolValue;
        std::string_view stringValue;
    };

    static ConstantValue make_int(int64_t v)
    {
        ConstantValue cv;
        cv.kind = Kind::Int;
        cv.intValue = v;
        return cv;
    }

    static ConstantValue make_float(double v)
    {
        ConstantValue cv;
        cv.kind = Kind::Float;
        cv.floatValue = v;
        return cv;
    }

    static ConstantValue make_bool(bool v)
    {
        ConstantValue cv;
        cv.kind = Kind::Bool;
        cv.boolValue = v;
        return cv;
    }

    static ConstantValue make_string(std::string_view v)
    {
        ConstantValue cv;
        cv.kind = Kind::String;
        cv.stringValue = v;
        return cv;
    }

    std::string format() const
    {
        switch (kind)
        {
            case Kind::Int:     return std::to_string(intValue);
            case Kind::Float:   return std::to_string(floatValue);
            case Kind::Bool:    return boolValue ? "true" : "false";
            case Kind::String:  return "\"" + std::string(stringValue) + "\"";
        }
    }

    bool range_fits(TypeSymbol* target) const;
    std::string format_range_message(TypeSymbol* target) const;
};

#pragma region Visitor

class FhirVisitor
{
public:
    virtual ~FhirVisitor() = default;

    virtual void visit(FhirLiteralExpr* node) = 0;
    virtual void visit(FhirLocalRefExpr* node) = 0;
    virtual void visit(FhirParamRefExpr* node) = 0;
    virtual void visit(FhirFieldRefExpr* node) = 0;
    virtual void visit(FhirThisExpr* node) = 0;
    virtual void visit(FhirIntrinsicExpr* node) = 0;
    virtual void visit(FhirCallExpr* node) = 0;
    virtual void visit(FhirConstructionExpr* node) = 0;
    virtual void visit(FhirAssignExpr* node) = 0;
    virtual void visit(FhirCastExpr* node) = 0;
    virtual void visit(FhirErrorExpr* node) = 0;
    virtual void visit(FhirNamespaceRefExpr* node) = 0;
    virtual void visit(FhirMethodGroupRefExpr* node) = 0;
    virtual void visit(FhirMethodRefExpr* node) = 0;

    virtual void visit(FhirBlock* node) = 0;
    virtual void visit(FhirVarDeclStmt* node) = 0;
    virtual void visit(FhirExprStmt* node) = 0;
    virtual void visit(FhirReturnStmt* node) = 0;
    virtual void visit(FhirIfStmt* node) = 0;
    virtual void visit(FhirWhileStmt* node) = 0;

    virtual void visit(FhirTypeRef* node) = 0;
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

    mutable std::optional<ConstantValue> constantCache;
    mutable bool constantComputed = false;

    FhirExpr(int k) : FhirNode(k) {}
    bool is_error() const { return is<FhirErrorExpr>(); }

    const std::optional<ConstantValue>& get_constant() const;
};

struct FhirStmt : FhirNode
{
    FhirStmt(int k) : FhirNode(k) {}
};

#pragma region Type Ref

// A user written reference to a type. Used in two ways. In type slots like
// var or param or cast types it is a type annotation. As a callee in bind_call
// it stands in for an unresolved construction and bind_call picks a constructor
// overload from referenced.
struct FhirTypeRef : FhirExpr
{
    FHIR_NODE(FhirTypeRef, FhirExpr)

    TypeSymbol* referenced = nullptr;
    std::vector<FhirTypeRef*> args;

    void visit_children(FhirVisitor* v) override
    {
        for (auto* arg : args)
            if (arg) arg->accept(v);
    }
};

#pragma region Expressions

struct FhirLiteralExpr : FhirExpr
{
    FHIR_NODE(FhirLiteralExpr, FhirExpr)

    ConstantValue value;

    std::optional<ConstantValue> compute_constant() const;
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

struct FhirFieldRefExpr : FhirExpr
{
    FHIR_NODE(FhirFieldRefExpr, FhirExpr)

    FhirExpr* thisRef = nullptr;
    FieldSymbol* field = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (thisRef) thisRef->accept(v);
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

    std::optional<ConstantValue> compute_constant() const;

    void visit_children(FhirVisitor* v) override
    {
        for (auto* arg : args)
            if (arg) arg->accept(v);
    }
};

// A name that bound to a namespace. Only valid as the left side of a member
// access while resolving qualified names. In any other slot it is an error.
struct FhirNamespaceRefExpr : FhirExpr
{
    FHIR_NODE(FhirNamespaceRefExpr, FhirExpr)

    NamespaceSymbol* namespaceSymbol = nullptr;
};

// A name that bound to a method, before overload resolution. Carries the
// owning scope and name so bind_call can resolve against arg types. Method
// names are the only lookup that can produce multiple candidates per name,
// so this is the one ref kind with an unresolved or resolved split.
struct FhirMethodGroupRefExpr : FhirExpr
{
    FHIR_NODE(FhirMethodGroupRefExpr, FhirExpr)

    Symbol* enclosingScope = nullptr;
    std::string_view name;
    FhirExpr* thisRef = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (thisRef) thisRef->accept(v);
    }
};

// A specific method overload, picked. Lives as the callee of FhirCallExpr
// after bind_call runs overload resolution. Rarely escapes elsewhere.
struct FhirMethodRefExpr : FhirExpr
{
    FHIR_NODE(FhirMethodRefExpr, FhirExpr)

    MethodSymbol* method = nullptr;
    FhirExpr* thisRef = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (thisRef) thisRef->accept(v);
    }
};

struct FhirCallExpr : FhirExpr
{
    FHIR_NODE(FhirCallExpr, FhirExpr)

    FhirMethodRefExpr* callee = nullptr;
    std::vector<FhirExpr*> arguments;

    void visit_children(FhirVisitor* v) override
    {
        if (callee) callee->accept(v);
        for (auto* arg : arguments)
            if (arg) arg->accept(v);
    }
};

// Wraps a constructor call in instance allocation. The inner `call` carries
// the resolved constructor and arguments. Its `callee.thisRef` is null because
// the new instance does not exist as a value until codegen.
// Constructors only ever appear inside this nested call.
struct FhirConstructionExpr : FhirExpr
{
    FHIR_NODE(FhirConstructionExpr, FhirExpr)

    FhirTypeRef* typeRef = nullptr;
    FhirCallExpr* call = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (typeRef) typeRef->accept(v);
        if (call) call->accept(v);
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

struct FhirCastExpr : FhirExpr
{
    FHIR_NODE(FhirCastExpr, FhirExpr)

    FhirExpr* operand = nullptr;
    FhirTypeRef* typeRef = nullptr;
    MethodSymbol* method = nullptr;
    bool isImplicit = false;

    std::optional<ConstantValue> compute_constant() const;

    void visit_children(FhirVisitor* v) override
    {
        if (operand) operand->accept(v);
        if (typeRef) typeRef->accept(v);
    }
};

struct FhirErrorExpr : FhirExpr
{
    FHIR_NODE(FhirErrorExpr, FhirExpr)

    FhirExpr* inner = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (inner) inner->accept(v);
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
    FhirTypeRef* typeRef = nullptr;
    FhirExpr* initializer = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (typeRef) typeRef->accept(v);
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
    void visit(FhirFieldRefExpr* node) override { node->visit_children(this); }
    void visit(FhirThisExpr* node) override { node->visit_children(this); }
    void visit(FhirIntrinsicExpr* node) override { node->visit_children(this); }
    void visit(FhirCallExpr* node) override { node->visit_children(this); }
    void visit(FhirConstructionExpr* node) override { node->visit_children(this); }
    void visit(FhirAssignExpr* node) override { node->visit_children(this); }
    void visit(FhirCastExpr* node) override { node->visit_children(this); }
    void visit(FhirErrorExpr* node) override {}
    void visit(FhirNamespaceRefExpr* node) override {}
    void visit(FhirMethodGroupRefExpr* node) override { node->visit_children(this); }
    void visit(FhirMethodRefExpr* node) override { node->visit_children(this); }

    void visit(FhirBlock* node) override { node->visit_children(this); }
    void visit(FhirVarDeclStmt* node) override { node->visit_children(this); }
    void visit(FhirExprStmt* node) override { node->visit_children(this); }
    void visit(FhirReturnStmt* node) override { node->visit_children(this); }
    void visit(FhirIfStmt* node) override { node->visit_children(this); }
    void visit(FhirWhileStmt* node) override { node->visit_children(this); }

    void visit(FhirTypeRef* node) override { node->visit_children(this); }
};

}

