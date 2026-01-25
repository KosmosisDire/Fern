#pragma once

#include <string>
#include <string_view>
#include <vector>
#include <source/span.hpp>
#include <token/token.hpp>

namespace Fern
{

#pragma region Forward Declarations

class DefaultAstVisitor;

// Base
struct BaseSyntax;

// Expressions
struct BaseExprSyntax;
struct IdentifierExprSyntax;
struct LiteralExprSyntax;
struct ParenExprSyntax;
struct BlockExprSyntax;
struct CallExprSyntax;
struct BinaryExprSyntax;
struct AssignmentExprSyntax;
struct TypeExprSyntax;

// Statements
struct BaseStmtSyntax;
struct ReturnStmtSyntax;
struct ExpressionStmtSyntax;

// Declarations
struct BaseDeclSyntax;
struct ParameterDeclSyntax;
struct VariableDeclSyntax;
struct FunctionDeclSyntax;

// Root
struct ProgramSyntax;

// Pointer aliases (arena-managed, no ownership semantics)
using ExprPtr = BaseExprSyntax*;
using StmtPtr = BaseStmtSyntax*;
using DeclPtr = BaseDeclSyntax*;



#pragma region DefaultAstVisitor

class DefaultAstVisitor
{
public:
    virtual ~DefaultAstVisitor() = default;

    // Expressions
    virtual void visit(IdentifierExprSyntax* node) = 0;
    virtual void visit(LiteralExprSyntax* node) = 0;
    virtual void visit(ParenExprSyntax* node) = 0;
    virtual void visit(BlockExprSyntax* node) = 0;
    virtual void visit(CallExprSyntax* node) = 0;
    virtual void visit(BinaryExprSyntax* node) = 0;
    virtual void visit(AssignmentExprSyntax* node) = 0;
    virtual void visit(TypeExprSyntax* node) = 0;

    // Statements
    virtual void visit(ReturnStmtSyntax* node) = 0;
    virtual void visit(ExpressionStmtSyntax* node) = 0;

    // Declarations
    virtual void visit(ParameterDeclSyntax* node) = 0;
    virtual void visit(VariableDeclSyntax* node) = 0;
    virtual void visit(FunctionDeclSyntax* node) = 0;

    // Root
    virtual void visit(ProgramSyntax* node) = 0;
};



// Macro to define Kind constant, syntax_node_name, accept function, and default constructor
#define SYNTAX_NODE(K, Base) \
    static constexpr int Kind = __LINE__; \
    std::string_view syntax_node_name() const override { return #K; } \
    void accept(DefaultAstVisitor* visitor) override { visitor->visit(this); } \
    K##Syntax() : Base(Kind) {}

#pragma region Base Nodes

struct BaseSyntax
{
    private: 
    int kind;

    public:
    Span span;

    BaseSyntax(int k) : kind(k) {}
    virtual ~BaseSyntax() = default;
    virtual std::string_view syntax_node_name() const = 0;
    virtual void accept(DefaultAstVisitor* visitor) = 0;

    template<typename T>
    bool is() const { return kind == T::Kind; }

    template<typename T>
    T* as() { return is<T>() ? static_cast<T*>(this) : nullptr; }

    template<typename T>
    const T* as() const { return is<T>() ? static_cast<const T*>(this) : nullptr; }
};

struct BaseExprSyntax : BaseSyntax
{
    BaseExprSyntax(int k) : BaseSyntax(k) {}
};

struct BaseStmtSyntax : BaseSyntax
{
    BaseStmtSyntax(int k) : BaseSyntax(k) {}
};

struct BaseDeclSyntax : BaseStmtSyntax
{
    BaseDeclSyntax(int k) : BaseStmtSyntax(k) {}
};



#pragma region Expressions

// Identifier
struct IdentifierExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(IdentifierExpr, BaseExprSyntax)

    std::string name;
};

// LiteralF32
struct LiteralExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(LiteralExpr, BaseExprSyntax)

    float value = 0.0f;
};

// (expr)
struct ParenExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(ParenExpr, BaseExprSyntax)

    ExprPtr expression = nullptr;
};

// { statements... tailExpr }
struct BlockExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(BlockExpr, BaseExprSyntax)

    std::vector<StmtPtr> statements;
    ExprPtr tailExpression = nullptr;
};

// identifier(args...)
struct CallExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(CallExpr, BaseExprSyntax)

    ExprPtr callee = nullptr;
    std::vector<ExprPtr> arguments;
};

// left + right
struct BinaryExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(BinaryExpr, BaseExprSyntax)

    ExprPtr left = nullptr;
    BinaryOp op = BinaryOp::Add;
    ExprPtr right = nullptr;
};

// target = value
struct AssignmentExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(AssignmentExpr, BaseExprSyntax)

    ExprPtr target = nullptr;
    AssignOp op = AssignOp::Simple;
    ExprPtr value = nullptr;
};

// f32 (type reference)
struct TypeExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(TypeExpr, BaseExprSyntax)

    std::string name;
};



#pragma region Statements

// return expr;
struct ReturnStmtSyntax : BaseStmtSyntax
{
    SYNTAX_NODE(ReturnStmt, BaseStmtSyntax)

    ExprPtr value = nullptr;
};

// expr;
struct ExpressionStmtSyntax : BaseStmtSyntax
{
    SYNTAX_NODE(ExpressionStmt, BaseStmtSyntax)

    ExprPtr expression = nullptr;
};



#pragma region Declarations

// name: type (in parameter list)
struct ParameterDeclSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(ParameterDecl, BaseDeclSyntax)

    std::string name;
    ExprPtr type = nullptr;
};

// var name: type = initializer;
struct VariableDeclSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(VariableDecl, BaseDeclSyntax)

    std::string name;
    ExprPtr type = nullptr;
    ExprPtr initializer = nullptr;
};

// fn name(params...): returnType { body }
struct FunctionDeclSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(FunctionDecl, BaseDeclSyntax)

    std::string name;
    std::vector<ParameterDeclSyntax*> parameters;
    ExprPtr returnType = nullptr;
    BlockExprSyntax* body = nullptr;
};



#pragma region Root

struct ProgramSyntax : BaseSyntax
{
    SYNTAX_NODE(Program, BaseSyntax)

    std::vector<DeclPtr> declarations;
};



#pragma region Default DefaultAstVisitor

class DefaultVisitor : public DefaultAstVisitor
{
public:
    void visit(IdentifierExprSyntax* node) override {}
    void visit(LiteralExprSyntax* node) override {}

    void visit(ParenExprSyntax* node) override
    {
        if (node->expression) node->expression->accept(this);
    }

    void visit(BlockExprSyntax* node) override
    {
        for (auto& stmt : node->statements)
            if (stmt) stmt->accept(this);
        if (node->tailExpression) node->tailExpression->accept(this);
    }

    void visit(CallExprSyntax* node) override
    {
        if (node->callee) node->callee->accept(this);
        for (auto& arg : node->arguments)
            if (arg) arg->accept(this);
    }

    void visit(BinaryExprSyntax* node) override
    {
        if (node->left) node->left->accept(this);
        if (node->right) node->right->accept(this);
    }

    void visit(AssignmentExprSyntax* node) override
    {
        if (node->target) node->target->accept(this);
        if (node->value) node->value->accept(this);
    }

    void visit(TypeExprSyntax* node) override {}

    void visit(ReturnStmtSyntax* node) override
    {
        if (node->value) node->value->accept(this);
    }

    void visit(ExpressionStmtSyntax* node) override
    {
        if (node->expression) node->expression->accept(this);
    }

    void visit(ParameterDeclSyntax* node) override
    {
        if (node->type) node->type->accept(this);
    }

    void visit(VariableDeclSyntax* node) override
    {
        if (node->type) node->type->accept(this);
        if (node->initializer) node->initializer->accept(this);
    }

    void visit(FunctionDeclSyntax* node) override
    {
        for (auto& param : node->parameters)
            if (param) param->accept(this);
        if (node->returnType) node->returnType->accept(this);
        if (node->body) node->body->accept(this);
    }

    void visit(ProgramSyntax* node) override
    {
        for (auto& decl : node->declarations)
            if (decl) decl->accept(this);
    }
};



} 
