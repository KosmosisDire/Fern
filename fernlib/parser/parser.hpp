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
    NamespaceDeclSyntax* parse_namespace_decl();

    // Statements
    BaseStmtSyntax* parse_statement();
    ReturnStmtSyntax* parse_return_stmt();

    // Expressions
    BaseExprSyntax* parse_expression();
    BaseExprSyntax* parse_assignment();
    BaseExprSyntax* parse_binary();
    BaseExprSyntax* parse_primary();
    CallExprSyntax* parse_call(BaseExprSyntax* callee);
    BaseExprSyntax* parse_member_access(BaseExprSyntax* left);
    BaseExprSyntax* parse_postfix();
    BlockExprSyntax* parse_block();

    // Types
    BaseExprSyntax* parse_type();

    TokenWalker& walker;
    AllocArena& arena;
};

}
