#include "parser.hpp"
#include <charconv>

namespace Fern
{

Parser::Parser(TokenWalker& walker, AstArena& arena)
    : walker(walker)
    , arena(arena)
{
}

#pragma region Helpers

static void skip_newlines(TokenWalker& walker)
{
    while (walker.check(TokenKind::Newline))
    {
        walker.advance();
    }
}

static bool is_at_statement_boundary(const TokenWalker& walker)
{
    return walker.check(TokenKind::Newline) ||
           walker.check(TokenKind::Semicolon) ||
           walker.check(TokenKind::RightBrace) ||
           walker.check(TokenKind::EndOfFile);
}

static float parse_float_lexeme(std::string_view lexeme)
{
    float value = 0.0f;
    std::from_chars(lexeme.data(), lexeme.data() + lexeme.size(), value);
    return value;
}

#pragma region Main Entry

ProgramSyntax* Parser::parse()
{
    auto* program = arena.alloc<ProgramSyntax>();
    Span startSpan = walker.current().span;

    skip_newlines(walker);

    while (!walker.is_at_end())
    {
        auto* decl = parse_declaration();
        if (decl)
        {
            program->declarations.push_back(decl);
        }
        skip_newlines(walker);
    }

    if (!program->declarations.empty())
    {
        program->span = startSpan.merge(program->declarations.back()->span);
    }
    else
    {
        program->span = startSpan;
    }

    return program;
}

#pragma region Declarations

BaseDeclSyntax* Parser::parse_declaration()
{
    skip_newlines(walker);

    if (walker.check(TokenKind::Fn))
    {
        return parse_function_decl();
    }
    if (walker.check(TokenKind::Var))
    {
        return parse_variable_decl();
    }

    walker.advance();
    return nullptr;
}

FunctionDeclSyntax* Parser::parse_function_decl()
{
    auto* func = arena.alloc<FunctionDeclSyntax>();
    Span startSpan = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    if (walker.check(TokenKind::Identifier))
    {
        func->name = std::string(walker.current().lexeme);
        walker.advance();
    }
    skip_newlines(walker);

    if (walker.check(TokenKind::LeftParen))
    {
        walker.advance();
        skip_newlines(walker);

        while (!walker.check(TokenKind::RightParen) && !walker.is_at_end())
        {
            auto* param = parse_parameter_decl();
            if (param)
            {
                func->parameters.push_back(param);
            }
            skip_newlines(walker);

            if (walker.check(TokenKind::Comma))
            {
                walker.advance();
                skip_newlines(walker);
            }
            else
            {
                break;
            }
        }

        if (walker.check(TokenKind::RightParen))
        {
            walker.advance();
        }
    }
    skip_newlines(walker);

    if (walker.check(TokenKind::ThinArrow))
    {
        walker.advance();
        skip_newlines(walker);
        func->returnType = parse_type();
    }
    skip_newlines(walker);

    if (walker.check(TokenKind::LeftBrace))
    {
        func->body = parse_block();
    }

    Span endSpan = func->body ? func->body->span : walker.current().span;
    func->span = startSpan.merge(endSpan);

    return func;
}

VariableDeclSyntax* Parser::parse_variable_decl()
{
    auto* var = arena.alloc<VariableDeclSyntax>();
    Span startSpan = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    if (walker.check(TokenKind::Identifier))
    {
        var->name = std::string(walker.current().lexeme);
        walker.advance();
    }

    if (walker.check(TokenKind::Colon))
    {
        walker.advance();
        skip_newlines(walker);
        var->type = parse_type();
    }

    if (walker.check(TokenKind::Assign))
    {
        walker.advance();
        skip_newlines(walker);
        var->initializer = parse_expression();
    }

    Span endSpan = walker.current().span;
    if (var->initializer)
    {
        endSpan = var->initializer->span;
    }
    else if (var->type)
    {
        endSpan = var->type->span;
    }
    var->span = startSpan.merge(endSpan);

    return var;
}

ParameterDeclSyntax* Parser::parse_parameter_decl()
{
    auto* param = arena.alloc<ParameterDeclSyntax>();
    Span startSpan = walker.current().span;

    if (walker.check(TokenKind::Identifier))
    {
        param->name = std::string(walker.current().lexeme);
        walker.advance();
    }

    if (walker.check(TokenKind::Colon))
    {
        walker.advance();
        skip_newlines(walker);
        param->type = parse_type();
    }

    Span endSpan = param->type ? param->type->span : startSpan;
    param->span = startSpan.merge(endSpan);

    return param;
}

#pragma region Statements

BaseStmtSyntax* Parser::parse_statement()
{
    skip_newlines(walker);

    if (walker.check(TokenKind::Return))
    {
        return parse_return_stmt();
    }
    if (walker.check(TokenKind::Var))
    {
        return parse_variable_decl();
    }

    auto* expr = parse_expression();
    if (expr)
    {
        auto* stmt = arena.alloc<ExpressionStmtSyntax>();
        stmt->expression = expr;
        stmt->span = expr->span;
        return stmt;
    }

    return nullptr;
}

ReturnStmtSyntax* Parser::parse_return_stmt()
{
    auto* stmt = arena.alloc<ReturnStmtSyntax>();
    Span startSpan = walker.current().span;

    walker.advance();

    if (!is_at_statement_boundary(walker))
    {
        stmt->value = parse_expression();
    }

    Span endSpan = stmt->value ? stmt->value->span : startSpan;
    stmt->span = startSpan.merge(endSpan);

    return stmt;
}

#pragma region Expressions

BaseExprSyntax* Parser::parse_expression()
{
    return parse_assignment();
}

BaseExprSyntax* Parser::parse_assignment()
{
    auto* left = parse_binary();
    if (!left)
    {
        return nullptr;
    }

    if (walker.check(TokenKind::Assign))
    {
        auto* assign = arena.alloc<AssignmentExprSyntax>();
        assign->target = left;
        assign->op = AssignOp::Simple;

        walker.advance();
        skip_newlines(walker);

        assign->value = parse_assignment();

        Span endSpan = assign->value ? assign->value->span : walker.current().span;
        assign->span = left->span.merge(endSpan);

        return assign;
    }

    return left;
}

BaseExprSyntax* Parser::parse_binary()
{
    auto* left = parse_call();
    if (!left)
    {
        return nullptr;
    }

    while (walker.check(TokenKind::Plus))
    {
        auto binaryOp = to_binary_op(walker.current().kind);
        walker.advance();
        skip_newlines(walker);

        auto* right = parse_call();

        auto* binary = arena.alloc<BinaryExprSyntax>();
        binary->left = left;
        binary->op = binaryOp.value_or(BinaryOp::Add);
        binary->right = right;

        Span endSpan = right ? right->span : walker.current().span;
        binary->span = left->span.merge(endSpan);

        left = binary;
    }

    return left;
}

BaseExprSyntax* Parser::parse_call()
{
    auto* callee = parse_primary();
    if (!callee)
    {
        return nullptr;
    }

    while (walker.check(TokenKind::LeftParen))
    {
        auto* call = arena.alloc<CallExprSyntax>();
        call->callee = callee;

        walker.advance();
        skip_newlines(walker);

        while (!walker.check(TokenKind::RightParen) && !walker.is_at_end())
        {
            auto* arg = parse_expression();
            if (arg)
            {
                call->arguments.push_back(arg);
            }
            skip_newlines(walker);

            if (walker.check(TokenKind::Comma))
            {
                walker.advance();
                skip_newlines(walker);
            }
            else
            {
                break;
            }
        }

        Span endSpan = walker.current().span;
        if (walker.check(TokenKind::RightParen))
        {
            walker.advance();
        }
        call->span = callee->span.merge(endSpan);

        callee = call;
    }

    return callee;
}

BaseExprSyntax* Parser::parse_primary()
{
    if (walker.check(TokenKind::Identifier))
    {
        auto* ident = arena.alloc<IdentifierExprSyntax>();
        ident->name = std::string(walker.current().lexeme);
        ident->span = walker.current().span;
        walker.advance();
        return ident;
    }

    if (walker.check(TokenKind::LiteralF32))
    {
        auto* lit = arena.alloc<LiteralExprSyntax>();
        lit->value = parse_float_lexeme(walker.current().lexeme);
        lit->span = walker.current().span;
        walker.advance();
        return lit;
    }

    if (walker.check(TokenKind::LeftParen))
    {
        auto* paren = arena.alloc<ParenExprSyntax>();
        Span startSpan = walker.current().span;

        walker.advance();
        skip_newlines(walker);

        paren->expression = parse_expression();
        skip_newlines(walker);

        Span endSpan = walker.current().span;
        if (walker.check(TokenKind::RightParen))
        {
            walker.advance();
        }
        paren->span = startSpan.merge(endSpan);

        return paren;
    }

    if (walker.check(TokenKind::LeftBrace))
    {
        return parse_block();
    }

    return nullptr;
}

BlockExprSyntax* Parser::parse_block()
{
    auto* block = arena.alloc<BlockExprSyntax>();
    Span startSpan = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    while (!walker.check(TokenKind::RightBrace) && !walker.is_at_end())
    {
        auto checkpoint = walker.checkpoint();

        auto* stmt = parse_statement();
        if (stmt)
        {
            block->statements.push_back(stmt);
        }
        skip_newlines(walker);

        if (walker.position() == checkpoint.position)
        {
            walker.advance();
        }
    }

    Span endSpan = walker.current().span;
    if (walker.check(TokenKind::RightBrace))
    {
        walker.advance();
    }
    block->span = startSpan.merge(endSpan);

    return block;
}

#pragma region Types

BaseExprSyntax* Parser::parse_type()
{
    if (walker.check(TokenKind::F32Keyword))
    {
        auto* type = arena.alloc<TypeExprSyntax>();
        type->name = std::string(walker.current().lexeme);
        type->span = walker.current().span;
        walker.advance();
        return type;
    }

    if (walker.check(TokenKind::Identifier))
    {
        auto* type = arena.alloc<TypeExprSyntax>();
        type->name = std::string(walker.current().lexeme);
        type->span = walker.current().span;
        walker.advance();
        return type;
    }

    return nullptr;
}

}
