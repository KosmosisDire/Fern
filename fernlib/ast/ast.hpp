#pragma once

#include <string>
#include <string_view>
#include <vector>
#include <source/span.hpp>
#include <token/token.hpp>

namespace Fern
{

#pragma region Forward Declarations

class AstVisitor;

// Base
struct BaseSyntax;

// Expressions
struct BaseExprSyntax;
struct IdentifierExprSyntax;
struct LiteralExprSyntax;
struct ParenExprSyntax;
struct BlockExprSyntax;
struct CallExprSyntax;
struct InitializerExprSyntax;
struct UnaryExprSyntax;
struct BinaryExprSyntax;
struct AssignmentExprSyntax;
struct MemberAccessExprSyntax;
struct ThisExprSyntax;
struct TypeExprSyntax;

// Statements
struct BaseStmtSyntax;
struct ReturnStmtSyntax;
struct ExpressionStmtSyntax;
struct IfStmtSyntax;
struct WhileStmtSyntax;

// Declarations
struct BaseDeclSyntax;
struct ParameterDeclSyntax;
struct VariableDeclSyntax;
struct FunctionDeclSyntax;
struct InitDeclSyntax;
struct OperatorDeclSyntax;
struct TypeDeclSyntax;
struct FieldDeclSyntax;
struct FieldInitSyntax;
struct NamespaceDeclSyntax;

// Root
struct RootSyntax;

// Pointer aliases (arena-managed, no ownership semantics)
using ExprPtr = BaseExprSyntax*;
using StmtPtr = BaseStmtSyntax*;
using DeclPtr = BaseDeclSyntax*;



#pragma region DefaultAstVisitor

class AstVisitor
{
public:
    virtual ~AstVisitor() = default;

    // Expressions
    virtual void visit(IdentifierExprSyntax* node) = 0;
    virtual void visit(LiteralExprSyntax* node) = 0;
    virtual void visit(ParenExprSyntax* node) = 0;
    virtual void visit(BlockExprSyntax* node) = 0;
    virtual void visit(CallExprSyntax* node) = 0;
    virtual void visit(InitializerExprSyntax* node) = 0;
    virtual void visit(UnaryExprSyntax* node) = 0;
    virtual void visit(BinaryExprSyntax* node) = 0;
    virtual void visit(AssignmentExprSyntax* node) = 0;
    virtual void visit(MemberAccessExprSyntax* node) = 0;
    virtual void visit(ThisExprSyntax* node) = 0;
    virtual void visit(TypeExprSyntax* node) = 0;

    // Statements
    virtual void visit(ReturnStmtSyntax* node) = 0;
    virtual void visit(ExpressionStmtSyntax* node) = 0;
    virtual void visit(IfStmtSyntax* node) = 0;
    virtual void visit(WhileStmtSyntax* node) = 0;

    // Declarations
    virtual void visit(ParameterDeclSyntax* node) = 0;
    virtual void visit(VariableDeclSyntax* node) = 0;
    virtual void visit(FunctionDeclSyntax* node) = 0;
    virtual void visit(InitDeclSyntax* node) = 0;
    virtual void visit(OperatorDeclSyntax* node) = 0;
    virtual void visit(TypeDeclSyntax* node) = 0;
    virtual void visit(FieldDeclSyntax* node) = 0;
    virtual void visit(FieldInitSyntax* node) = 0;
    virtual void visit(NamespaceDeclSyntax* node) = 0;

    // Root
    virtual void visit(RootSyntax* node) = 0;
};



// Macro to define Kind constant, syntax_node_name, accept function, and default constructor
#define SYNTAX_NODE(K, Base) \
    static constexpr int Kind = __LINE__; \
    std::string_view syntax_node_name() const override { return #K; } \
    void accept(AstVisitor* visitor) override { visitor->visit(this); } \
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
    virtual void accept(AstVisitor* visitor) = 0;

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
    Modifier modifiers = Modifier::None;
    BaseDeclSyntax(int k) : BaseStmtSyntax(k) {}
};



#pragma region Expressions

// Identifier
struct IdentifierExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(IdentifierExpr, BaseExprSyntax)

    Token name = Token::Invalid();
};

// 2, 2.5, true, false
struct LiteralExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(LiteralExpr, BaseExprSyntax)

    Token token = Token::Invalid();
};

// (expr)
struct ParenExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(ParenExpr, BaseExprSyntax)

    ExprPtr expression = nullptr;
};

// { statements... }
struct BlockExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(BlockExpr, BaseExprSyntax)

    std::vector<StmtPtr> statements;
};

// identifier(args...)
struct CallExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(CallExpr, BaseExprSyntax)

    ExprPtr callee = nullptr;
    std::vector<ExprPtr> arguments;
};

// Vector2(1.0, 2.0) { y: 5.0 }
struct InitializerExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(InitializerExpr, BaseExprSyntax)

    ExprPtr target = nullptr;
    std::vector<FieldInitSyntax*> initializers;
};

// -operand, +operand
struct UnaryExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(UnaryExpr, BaseExprSyntax)

    UnaryOp op = UnaryOp::Negative;
    ExprPtr operand = nullptr;
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

// object.member
struct MemberAccessExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(MemberAccessExpr, BaseExprSyntax)

    ExprPtr left = nullptr;
    Token right = Token::Invalid();
};

// this
struct ThisExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(ThisExpr, BaseExprSyntax)

    Token token = Token::Invalid();
};

// f32 (type reference)
struct TypeExprSyntax : BaseExprSyntax
{
    SYNTAX_NODE(TypeExpr, BaseExprSyntax)

    Token name = Token::Invalid();
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

// if condition { ... } else { ... }
struct IfStmtSyntax : BaseStmtSyntax
{
    SYNTAX_NODE(IfStmt, BaseStmtSyntax)

    ExprPtr condition = nullptr;
    BlockExprSyntax* thenBody = nullptr;
    IfStmtSyntax* elseIf = nullptr;
    BlockExprSyntax* elseBlock = nullptr;
};

// while condition { ... }
struct WhileStmtSyntax : BaseStmtSyntax
{
    SYNTAX_NODE(WhileStmt, BaseStmtSyntax)

    ExprPtr condition = nullptr;
    BlockExprSyntax* body = nullptr;
};

#pragma region Declarations

// name: type (in parameter list)
struct ParameterDeclSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(ParameterDecl, BaseDeclSyntax)

    Token name = Token::Invalid();
    ExprPtr type = nullptr;
};

// var name: type = initializer;
struct VariableDeclSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(VariableDecl, BaseDeclSyntax)

    Token name = Token::Invalid();
    ExprPtr type = nullptr;
    ExprPtr initializer = nullptr;
};

// fn name(params...)-> returnType { body }
struct FunctionDeclSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(FunctionDecl, BaseDeclSyntax)

    Token name = Token::Invalid();
    std::vector<ParameterDeclSyntax*> parameters;
    ExprPtr returnType = nullptr;
    BlockExprSyntax* body = nullptr;
};

// init(params...) { body }
struct InitDeclSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(InitDecl, BaseDeclSyntax)

    std::vector<ParameterDeclSyntax*> parameters;
    BlockExprSyntax* body = nullptr;
};

// op +(params) -> Type { body }
struct OperatorDeclSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(OperatorDecl, BaseDeclSyntax)

    Token op = Token::Invalid();
    std::vector<ParameterDeclSyntax*> parameters;
    ExprPtr returnType = nullptr;
    BlockExprSyntax* body = nullptr;
};

// type name { ... }
struct TypeDeclSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(TypeDecl, BaseDeclSyntax)

    Token name = Token::Invalid();
    std::vector<DeclPtr> declarations;
};

struct FieldDeclSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(FieldDecl, BaseDeclSyntax)

    Token name = Token::Invalid();
    ExprPtr type = nullptr;
    ExprPtr initializer = nullptr;
};

// x: 3.0 (field initializer in initializer list)
struct FieldInitSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(FieldInit, BaseDeclSyntax)

    Token name = Token::Invalid();
    ExprPtr value = nullptr;
};

struct NamespaceDeclSyntax : BaseDeclSyntax
{
    SYNTAX_NODE(NamespaceDecl, BaseDeclSyntax)

    bool is_file_level = false;
    Token name = Token::Invalid();
    std::vector<DeclPtr> declarations;
};

#pragma region Root

struct RootSyntax : BaseSyntax
{
    SYNTAX_NODE(Root, BaseSyntax)

    std::vector<DeclPtr> declarations;
};



#pragma region DefaultAstVisitor

class DefaultAstVisitor : public AstVisitor
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
    }

    void visit(CallExprSyntax* node) override
    {
        if (node->callee) node->callee->accept(this);
        for (auto& arg : node->arguments)
            if (arg) arg->accept(this);
    }

    void visit(InitializerExprSyntax* node) override
    {
        if (node->target) node->target->accept(this);
        for (auto& init : node->initializers)
            if (init) init->accept(this);
    }

    void visit(UnaryExprSyntax* node) override
    {
        if (node->operand) node->operand->accept(this);
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

    void visit(MemberAccessExprSyntax* node) override
    {
        if (node->left) node->left->accept(this);
    }

    void visit(ThisExprSyntax* node) override {}

    void visit(TypeExprSyntax* node) override {}

    void visit(ReturnStmtSyntax* node) override
    {
        if (node->value) node->value->accept(this);
    }

    void visit(ExpressionStmtSyntax* node) override
    {
        if (node->expression) node->expression->accept(this);
    }

    void visit(IfStmtSyntax* node) override
    {
        if (node->condition) node->condition->accept(this);
        if (node->thenBody) node->thenBody->accept(this);
        if (node->elseIf) node->elseIf->accept(this);
        if (node->elseBlock) node->elseBlock->accept(this);
    }

    void visit(WhileStmtSyntax* node) override
    {
        if (node->condition) node->condition->accept(this);
        if (node->body) node->body->accept(this);
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

    void visit(InitDeclSyntax* node) override
    {
        for (auto& param : node->parameters)
            if (param) param->accept(this);
        if (node->body) node->body->accept(this);
    }

    void visit(OperatorDeclSyntax* node) override
    {
        for (auto& param : node->parameters)
            if (param) param->accept(this);
        if (node->returnType) node->returnType->accept(this);
        if (node->body) node->body->accept(this);
    }

    void visit(TypeDeclSyntax* node) override {}

    void visit(FieldDeclSyntax* node) override
    {
        if (node->type) node->type->accept(this);
    }

    void visit(FieldInitSyntax* node) override
    {
        if (node->value) node->value->accept(this);
    }

    void visit(NamespaceDeclSyntax* node) override
    {
        for (auto& decl : node->declarations)
            if (decl) decl->accept(this);
    }

    void visit(RootSyntax* node) override
    {
        for (auto& decl : node->declarations)
            if (decl) decl->accept(this);
    }
};



} 
