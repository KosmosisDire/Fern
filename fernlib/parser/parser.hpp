#pragma once

#include "ast/ast.hpp"
#include <arena.hpp>
#include <common/diagnostic.hpp>
#include <token/walker.hpp>

namespace Fern
{

class Parser : public DiagnosticSystem
{
public:
    Parser(TokenWalker& walker, AllocArena& arena);

    RootSyntax* parse();

private:
    const Token* expect(TokenKind kind, std::string_view message);

    // Declarations
    BaseDeclSyntax* parse_declaration();
    FunctionDeclSyntax* parse_function_decl();
    VariableDeclSyntax* parse_variable_decl();
    ParameterDeclSyntax* parse_parameter_decl();
    TypeDeclSyntax* parse_type_decl();
    FieldDeclSyntax* parse_field_decl();
    InitDeclSyntax* parse_init_decl();
    OperatorDeclSyntax* parse_operator_decl();
    NamespaceDeclSyntax* parse_namespace_decl();

    // Shared parse helpers
    void parse_parameter_list(std::vector<ParameterDeclSyntax*>& out, Span& span);
    BaseExprSyntax* parse_return_type(Span& span);
    BlockExprSyntax* parse_body(Span& span);

    // Statements
    BaseStmtSyntax* parse_statement();
    ReturnStmtSyntax* parse_return_stmt();

    // Expressions
    BaseExprSyntax* parse_expression();
    BaseExprSyntax* parse_assignment();
    BaseExprSyntax* parse_binary(Precedence minPrec = Precedence::None);
    BaseExprSyntax* parse_unary();
    BaseExprSyntax* parse_primary();
    CallExprSyntax* parse_call(BaseExprSyntax* callee);
    InitializerExprSyntax* parse_initializer(BaseExprSyntax* target);
    BaseExprSyntax* parse_member_access(BaseExprSyntax* left);
    BaseExprSyntax* parse_postfix();
    BlockExprSyntax* parse_block();

    // Types
    BaseExprSyntax* parse_type();

    TokenWalker& walker;
    AllocArena& arena;
};

}
