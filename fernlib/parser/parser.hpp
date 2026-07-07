#pragma once

#include <ast/ast.hpp>
#include <ast/builder.hpp>
#include <arena.hpp>
#include <common/diagnostic.hpp>
#include <token/walker.hpp>

namespace Fern
{

class Parser
{
public:
    Parser(TokenWalker& walker, AllocArena& arena, Diagnostics& diag);

    RootSyntax* parse();

private:
    const Token* expect(TokenKind kind, std::string_view message);
    void expect_progress(TokenWalker::Checkpoint cp);
    Modifier parse_modifiers(std::vector<Token>& outTokens);
    void parse_attributes(std::vector<AttributeSyntax*>& out);

    // Declarations
    BaseDeclSyntax* parse_declaration();
    CallableDeclSyntax* parse_function_decl();
    VariableDeclSyntax* parse_variable_decl();
    ParameterDeclSyntax* parse_parameter_decl();
    TypeDeclSyntax* parse_type_decl();
    FieldDeclSyntax* parse_field_decl();
    CallableDeclSyntax* parse_init_decl();
    CallableDeclSyntax* parse_operator_decl();
    CallableDeclSyntax* parse_literal_decl();
    CallableDeclSyntax* parse_cast_decl();
    NamespaceDeclSyntax* parse_namespace_decl();

    // Shared parse helpers
    void parse_parameter_list(ParameterListSyntax& out, Span& span);
    TypeExprSyntax* parse_return_type(Span& span);
    BlockSyntax* parse_body(Span& span);

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
    void parse_object_builder_members(std::vector<StmtPtr>& out);
    ObjectBuilderExprSyntax* parse_object_builder(BaseExprSyntax* target = nullptr);
    MemberAccessExprSyntax* parse_member_access(BaseExprSyntax* left);
    SimpleNameExprSyntax* parse_simple_name();
    BaseExprSyntax* parse_postfix();
    BlockSyntax* parse_block();
    IfStmtSyntax* parse_if();
    WhileStmtSyntax* parse_while();

    // Types
    TypeExprSyntax* parse_type();

    // Trace brace scope for parsing conditions (and maybe other stuff in the future)
    // Modifies the parser passed into it, uses RAII to track a scope
    struct BraceInitScope
    {
        Parser& parser;
        bool previous;

        explicit BraceInitScope(Parser& owner)
            : parser(owner), previous(owner.inCondition)
        {
            parser.inCondition = false;
        }

        ~BraceInitScope()
        {
            parser.inCondition = previous;
        }
    };

    TokenWalker& walker;
    AllocArena& arena;
    Diagnostics& diag;
    AstBuilder builder;
    // Set while parsing a condition so a top-level '{' ends it as the body.
    bool inCondition = false;
};

}
