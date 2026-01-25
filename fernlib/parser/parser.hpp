#pragma once

#include "ast/ast.hpp"
#include "ast/arena.hpp"
#include <token/walker.hpp>

namespace Fern
{

class Parser
{
public:
    Parser(TokenWalker& walker, AstArena& arena);

    ProgramSyntax* parse();

private:
    // Declarations
    BaseDeclSyntax* parse_declaration();
    FunctionDeclSyntax* parse_function_decl();
    VariableDeclSyntax* parse_variable_decl();
    ParameterDeclSyntax* parse_parameter_decl();

    // Statements
    BaseStmtSyntax* parse_statement();
    ReturnStmtSyntax* parse_return_stmt();

    // Expressions
    BaseExprSyntax* parse_expression();
    BaseExprSyntax* parse_assignment();
    BaseExprSyntax* parse_binary();
    BaseExprSyntax* parse_primary();
    BaseExprSyntax* parse_call();
    BlockExprSyntax* parse_block();

    // Types
    BaseExprSyntax* parse_type();

    TokenWalker& walker;
    AstArena& arena;
};

}
