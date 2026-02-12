#include "parser.hpp"
#include <charconv>

namespace Fern
{

Parser::Parser(TokenWalker& walker, AllocArena& arena)
    : DiagnosticSystem("Parser")
    , walker(walker)
    , arena(arena)
{
}

const Token* Parser::expect(TokenKind kind, std::string_view message)
{
    if (walker.check(kind))
    {
        return &walker.advance();
    }
    error(message, walker.current().span);
    return nullptr;
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

RootSyntax* Parser::parse()
{
    auto* program = arena.alloc<RootSyntax>();
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
    if (walker.check(TokenKind::Type))
    {
        return parse_type_decl();
    }
    if (walker.check(TokenKind::Namespace))
    {
        return parse_namespace_decl();
    }

    walker.advance();
    return nullptr;
}

FunctionDeclSyntax* Parser::parse_function_decl()
{
    auto* func = arena.alloc<FunctionDeclSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    if (auto* name = expect(TokenKind::Identifier, "expected name after 'fn'"))
    {
        func->name = *name;
        span = span.merge(name->span);
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

        if (auto* token = expect(TokenKind::RightParen, "expected ')' after parameter list"))
        {
            span = span.merge(token->span);
        }
    }
    skip_newlines(walker);

    if (walker.check(TokenKind::ThinArrow))
    {
        walker.advance();
        skip_newlines(walker);
        func->returnType = parse_type();
        if (func->returnType)
        {
            span = span.merge(func->returnType->span);
        }
    }
    skip_newlines(walker);

    if (walker.check(TokenKind::LeftBrace))
    {
        func->body = parse_block();
        span = span.merge(func->body->span);
    }

    func->span = span;

    return func;
}

VariableDeclSyntax* Parser::parse_variable_decl()
{
    auto* var = arena.alloc<VariableDeclSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    if (auto* name = expect(TokenKind::Identifier, "expected name after 'var'"))
    {
        var->name = *name;
        span = span.merge(name->span);
    }

    if (walker.check(TokenKind::Colon))
    {
        walker.advance();
        skip_newlines(walker);
        var->type = parse_type();
        if (var->type)
        {
            span = span.merge(var->type->span);
        }
    }

    if (walker.check(TokenKind::Assign))
    {
        walker.advance();
        skip_newlines(walker);
        var->initializer = parse_expression();
        if (var->initializer)
        {
            span = span.merge(var->initializer->span);
        }
    }

    var->span = span;

    return var;
}

ParameterDeclSyntax* Parser::parse_parameter_decl()
{
    auto* param = arena.alloc<ParameterDeclSyntax>();
    Span span = walker.current().span;

    if (auto* name = expect(TokenKind::Identifier, "expected parameter name"))
    {
        param->name = *name;
    }

    if (walker.check(TokenKind::Colon))
    {
        walker.advance();
        skip_newlines(walker);
        param->type = parse_type();
        if (param->type)
        {
            span = span.merge(param->type->span);
        }
    }

    param->span = span;

    return param;
}

TypeDeclSyntax* Parser::parse_type_decl()
{
    auto* typeDecl = arena.alloc<TypeDeclSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    if (auto* name = expect(TokenKind::Identifier, "expected name after 'type'"))
    {
        typeDecl->name = *name;
        span = span.merge(name->span);
    }

    skip_newlines(walker);

    if (walker.check(TokenKind::LeftBrace))
    {
        walker.advance();
        skip_newlines(walker);

        while (!walker.check(TokenKind::RightBrace) && !walker.is_at_end())
        {
            auto* field = parse_field_decl();
            if (field)
            {
                typeDecl->declarations.push_back(field);
            }
            else
            {
                auto* declaration = parse_declaration();
                if (declaration)
                {
                    typeDecl->declarations.push_back(declaration);
                }
            }
            skip_newlines(walker);
        }

        if (auto* token = expect(TokenKind::RightBrace, "expected '}' after type body"))
        {
            span = span.merge(token->span);
        }
    }

    typeDecl->span = span;

    return typeDecl;
}

FieldDeclSyntax* Parser::parse_field_decl()
{
    if (!walker.check(TokenKind::Identifier) || 
        !(walker.peek(1).kind == TokenKind::Colon || walker.peek(1).kind == TokenKind::Assign))
    {
        return nullptr;
    }

    auto* field = arena.alloc<FieldDeclSyntax>();
    Span span = walker.current().span;

    if (walker.check(TokenKind::Identifier))
    {
        field->name = walker.current();
        walker.advance();
    }

    if (walker.check(TokenKind::Colon))
    {
        walker.advance();
        skip_newlines(walker);
        field->type = parse_type();
        if (field->type)
        {
            span = span.merge(field->type->span);
        }
    }

    if (walker.check(TokenKind::Assign))
    {
        walker.advance();
        skip_newlines(walker);
        field->initializer = parse_expression();
        if (field->initializer)
        {
            span = span.merge(field->initializer->span);
        }
    }

    field->span = span;

    return field;
}

NamespaceDeclSyntax* Parser::parse_namespace_decl()
{
    auto* nsDecl = arena.alloc<NamespaceDeclSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    if (auto* name = expect(TokenKind::Identifier, "expected name after 'namespace'"))
    {
        nsDecl->name = *name;
        span = span.merge(name->span);
    }

    skip_newlines(walker);

    if (walker.check(TokenKind::LeftBrace))
    {
        walker.advance();
        skip_newlines(walker);

        while (!walker.check(TokenKind::RightBrace) && !walker.is_at_end())
        {
            auto* decl = parse_declaration();
            if (decl)
            {
                nsDecl->declarations.push_back(decl);
            }
            skip_newlines(walker);
        }

        if (auto* token = expect(TokenKind::RightBrace, "expected '}' after namespace body"))
        {
            span = span.merge(token->span);
        }
    }
    else
    {
        while (!walker.is_at_end())
        {
            auto* decl = parse_declaration();
            if (decl)
            {
                nsDecl->declarations.push_back(decl);
                span = span.merge(decl->span);
            }
            skip_newlines(walker);
        }
    }

    nsDecl->span = span;

    return nsDecl;
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
    Span span = walker.current().span;

    walker.advance();

    if (!is_at_statement_boundary(walker))
    {
        stmt->value = parse_expression();
        if (stmt->value)
        {
            span = span.merge(stmt->value->span);
        }
    }

    stmt->span = span;

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

    auto assignOp = to_assign_op(walker.current().kind);
    if (assignOp)
    {
        auto* assign = arena.alloc<AssignmentExprSyntax>();
        assign->target = left;
        assign->op = *assignOp;

        walker.advance();
        skip_newlines(walker);

        assign->value = parse_assignment();

        Span span = left->span;
        if (assign->value)
        {
            span = span.merge(assign->value->span);
        }
        assign->span = span;

        return assign;
    }

    return left;
}

BaseExprSyntax* Parser::parse_binary()
{
    auto* left = parse_postfix();
    if (!left)
    {
        return nullptr;
    }

    while (walker.check(TokenKind::Plus))
    {
        auto binaryOp = to_binary_op(walker.current().kind);
        walker.advance();
        skip_newlines(walker);

        auto* right = parse_postfix();

        auto* binary = arena.alloc<BinaryExprSyntax>();
        binary->left = left;
        binary->op = binaryOp.value_or(BinaryOp::Add);
        binary->right = right;

        Span span = left->span;
        if (right)
        {
            span = span.merge(right->span);
        }
        binary->span = span;

        left = binary;
    }

    return left;
}

CallExprSyntax* Parser::parse_call(BaseExprSyntax* callee)
{
    auto* call = arena.alloc<CallExprSyntax>();
    call->callee = callee;

    walker.advance(); // consume '('
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

    Span span = callee->span;
    if (auto* token = expect(TokenKind::RightParen, "expected ')' after arguments"))
    {
        span = span.merge(token->span);
    }
    call->span = span;

    return call;
}

BaseExprSyntax* Parser::parse_member_access(BaseExprSyntax* left)
{
    auto* memberAccess = arena.alloc<MemberAccessExprSyntax>();
    memberAccess->left = left;

    walker.advance();

    if (auto* name = expect(TokenKind::Identifier, "expected member name after '.'"))
    {
        memberAccess->right = *name;
        memberAccess->span = left->span.merge(name->span);
        return memberAccess;
    }

    memberAccess->span = left->span;
    return memberAccess;
}

BaseExprSyntax* Parser::parse_postfix()
{
    auto* left = parse_primary();
    if (!left)
    {
        return nullptr;
    }

    while (true)
    {
        if (walker.check(TokenKind::Dot))
        {
            left = parse_member_access(left);
        }
        else if (walker.check(TokenKind::LeftParen))
        {
            left = parse_call(left);
        }
        else
        {
            break;
        }
    }

    return left;
}

BaseExprSyntax* Parser::parse_primary()
{
    if (walker.check(TokenKind::Identifier))
    {
        auto* ident = arena.alloc<IdentifierExprSyntax>();
        ident->name = walker.current();
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
        Span span = walker.current().span;

        walker.advance();
        skip_newlines(walker);

        paren->expression = parse_expression();
        skip_newlines(walker);

        if (auto* token = expect(TokenKind::RightParen, "expected ')' after expression"))
        {
            span = span.merge(token->span);
        }
        paren->span = span;

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
    Span span = walker.current().span;

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

    if (auto* token = expect(TokenKind::RightBrace, "expected '}' after block"))
    {
        span = span.merge(token->span);
    }
    block->span = span;

    return block;
}

#pragma region Types

BaseExprSyntax* Parser::parse_type()
{
    BaseExprSyntax* type = nullptr;

    if (walker.check(TokenKind::F32Keyword))
    {
        auto* t = arena.alloc<TypeExprSyntax>();
        t->name = walker.current();
        t->span = walker.current().span;
        walker.advance();
        type = t;
    }
    else if (walker.check(TokenKind::Identifier))
    {
        auto* t = arena.alloc<TypeExprSyntax>();
        t->name = walker.current();
        t->span = walker.current().span;
        walker.advance();
        type = t;
    }

    while (type && walker.check(TokenKind::Dot))
    {
        type = parse_member_access(type);
    }

    return type;
}

}
