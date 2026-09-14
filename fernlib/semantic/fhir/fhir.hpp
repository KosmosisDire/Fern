#pragma once

#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <vector>
#include <source/span.hpp>

#include <semantic/constant.hpp>
#include <semantic/intrinsics.hpp>
#include <semantic/symbol/overload.hpp>
#include <semantic/symbol/symbol.hpp>

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
struct FhirOpExpr;
struct FhirCallExpr;
struct FhirConstructionExpr;
struct FhirAssignExpr;
struct FhirCompoundAssignExpr;
struct FhirCastExpr;
struct FhirIndexExpr;
struct FhirObjectBuilderExpr;
struct FhirArrayLiteralExpr;
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
    virtual void visit(FhirOpExpr* node) = 0;
    virtual void visit(FhirCallExpr* node) = 0;
    virtual void visit(FhirConstructionExpr* node) = 0;
    virtual void visit(FhirAssignExpr* node) = 0;
    virtual void visit(FhirCompoundAssignExpr* node) = 0;
    virtual void visit(FhirCastExpr* node) = 0;
    virtual void visit(FhirIndexExpr* node) = 0;
    virtual void visit(FhirObjectBuilderExpr* node) = 0;
    virtual void visit(FhirArrayLiteralExpr* node) = 0;
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

// A place is any memory location that is assignable and outlives the expression naming it, so
// another way of saying lvalue. This says where a place's storage lives, which decides how long it
// lasts. Temporary means the expression is a value and names no storage at all.
enum class PlaceStorage
{
    Temporary,
    // Dies at return: a local, a param copy, or the handle slot of this in a ref type
    Frame,
    // The caller's own memory, reached through this in a value type method
    Receiver,
    // An object's own memory, reached through a ref type handle or a static
    Heap,
    // Memory reached through a Ptr<T>, lifetime unknown
    Pointer,
};

struct FhirExpr : FhirNode
{
    TypeSymbol* type = nullptr;

    mutable std::optional<ConstantValue> constantCache;
    mutable bool constantComputed = false;

    FhirExpr(int k) : FhirNode(k) {}
    bool is_error() const { return is<FhirErrorExpr>(); }

    const std::optional<ConstantValue>& get_constant() const;

    // Assignment needs any place, a ref return needs one that outlives the frame
    PlaceStorage place_storage() const;
    bool is_place() const { return place_storage() != PlaceStorage::Temporary; }
};

// The storage a method called on this receiver may hand back through a ref return or write into
// through this. Null means a static call.
PlaceStorage receiver_storage(const FhirExpr* receiver);

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

    LocalSymbol* symbol = nullptr;
};

struct FhirParamRefExpr : FhirExpr
{
    FHIR_NODE(FhirParamRefExpr, FhirExpr)

    ParameterSymbol* symbol = nullptr;
};

struct FhirFieldRefExpr : FhirExpr
{
    FHIR_NODE(FhirFieldRefExpr, FhirExpr)

    FhirExpr* thisRef = nullptr;
    FieldSymbol* symbol = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        if (thisRef) thisRef->accept(v);
    }
};

struct FhirThisExpr : FhirExpr
{
    FHIR_NODE(FhirThisExpr, FhirExpr)
};

struct FhirOpExpr : FhirExpr
{
    FHIR_NODE(FhirOpExpr, FhirExpr)

    IntrinsicKind op = IntrinsicKind::None;
    MethodSymbol* method = nullptr;
    std::vector<FhirExpr*> args;

    mutable bool constantOverflowed = false;

    std::optional<ConstantValue> compute_constant() const;

    // True when every operand is constant but the result does not fit this op's type
    bool constant_overflows() const;

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
    // true when the receiver was written in source, false for an implicit this
    bool explicitReceiver = false;

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

    // True when the callee returns ref, so the call yields the returned place rather than a copy
    bool returns_ref() const { return callee && callee->method && callee->method->returnsRef; }

    void visit_children(FhirVisitor* v) override
    {
        if (callee) callee->accept(v);
        for (auto* arg : arguments)
            if (arg) arg->accept(v);
    }
};

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

struct FhirCompoundAssignExpr : FhirExpr
{
    FHIR_NODE(FhirCompoundAssignExpr, FhirExpr)

    FhirOpExpr* binaryOp = nullptr;

    FhirExpr* target() const { return binaryOp && !binaryOp->args.empty() ? binaryOp->args[0] : nullptr; }
    FhirExpr* value()  const { return binaryOp && binaryOp->args.size() > 1 ? binaryOp->args[1] : nullptr; }

    void visit_children(FhirVisitor* v) override
    {
        // Bypass the op node since it is data only and doesn't represent an actual operation here.
        if (!binaryOp) return;
        for (auto* arg : binaryOp->args) if (arg) arg->accept(v);
    }
};

struct FhirCastExpr : FhirExpr
{
    FHIR_NODE(FhirCastExpr, FhirExpr)

    FhirExpr* operand = nullptr;
    FhirTypeRef* typeRef = nullptr;
    MethodSymbol* method = nullptr;

    std::optional<ConstantValue> compute_constant() const;

    void visit_children(FhirVisitor* v) override
    {
        if (operand) operand->accept(v);
        if (typeRef) typeRef->accept(v);
    }
};

struct FhirIndexExpr : FhirExpr
{
    FHIR_NODE(FhirIndexExpr, FhirExpr)

    FhirExpr* object = nullptr;
    FhirExpr* index = nullptr;
    MethodSymbol* getter = nullptr;
    MethodSymbol* setter = nullptr;

    // True when the getter returns ref, so indexing yields the element place and writes need no setter
    bool returns_ref() const { return getter && getter->returnsRef; }

    void visit_children(FhirVisitor* v) override
    {
        if (object) object->accept(v);
        if (index) index->accept(v);
    }
};

struct FhirObjectBuilderEntry
{
    std::vector<FieldSymbol*> path;
    FhirExpr* value = nullptr;
};

struct FhirObjectBuilderExpr : FhirExpr
{
    FHIR_NODE(FhirObjectBuilderExpr, FhirExpr)

    FhirExpr* construction = nullptr;
    std::vector<FhirObjectBuilderEntry> entries;

    void visit_children(FhirVisitor* v) override
    {
        if (construction) construction->accept(v);
        for (auto& entry : entries)
            if (entry.value) entry.value->accept(v);
    }
};

struct FhirArrayLiteralExpr : FhirExpr
{
    FHIR_NODE(FhirArrayLiteralExpr, FhirExpr)

    TypeSymbol* elementType = nullptr;
    std::vector<FhirExpr*> elements;
    MethodSymbol* ctor = nullptr;
    // Elements are stored through the getter when it returns ref, otherwise through the setter
    MethodSymbol* getter = nullptr;
    MethodSymbol* setter = nullptr;

    void visit_children(FhirVisitor* v) override
    {
        for (auto* elem : elements)
            if (elem) elem->accept(v);
    }
};

struct FhirErrorExpr : FhirExpr
{
    FHIR_NODE(FhirErrorExpr, FhirExpr)

    // TODO make this a list so multi operand errors keep every operand for IDE use
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
protected:
    virtual void on_visit(FhirNode*) {}

public:
    void visit(FhirLiteralExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirLocalRefExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirParamRefExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirFieldRefExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirThisExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirOpExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirCallExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirConstructionExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirAssignExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirCompoundAssignExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirCastExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirIndexExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirObjectBuilderExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirArrayLiteralExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirErrorExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirNamespaceRefExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirMethodGroupRefExpr* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirMethodRefExpr* node) override { on_visit(node); node->visit_children(this); }

    void visit(FhirBlock* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirVarDeclStmt* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirExprStmt* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirReturnStmt* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirIfStmt* node) override { on_visit(node); node->visit_children(this); }
    void visit(FhirWhileStmt* node) override { on_visit(node); node->visit_children(this); }

    void visit(FhirTypeRef* node) override { on_visit(node); node->visit_children(this); }
};

#pragma region Overload Helpers

inline OverloadArg::OverloadArg(FhirExpr* expr)
{
    if (!expr) return;
    type = expr->type;
    const auto& c = expr->get_constant();
    constant = c ? &*c : nullptr;
}

}

