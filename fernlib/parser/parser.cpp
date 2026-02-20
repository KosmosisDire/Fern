#include "parser.hpp"

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

static bool is_terminator(TokenKind kind)
{
    return kind == TokenKind::Newline || kind == TokenKind::Semicolon;
}

static void skip_terminators(TokenWalker& walker)
{
    while (is_terminator(walker.current().kind))
    {
        walker.advance();
    }
}

Modifier Parser::parse_modifiers()
{
    Modifier mods = Modifier::None;
    while (auto mod = to_modifier(walker.current().kind))
    {
        if (has_modifier(mods, *mod))
        {
            error("duplicate modifier '" + std::string(walker.current().lexeme) + "'", walker.current().span);
        }
        mods = mods | *mod;
        walker.advance();
        skip_terminators(walker);
    }
    return mods;
}

static bool is_initializer_list_ahead(const TokenWalker& walker)
{
    if (walker.peek(0).kind != TokenKind::LeftBrace)
    {
        return false;
    }

    size_t offset = 1;
    while (is_terminator(walker.peek(offset).kind))
    {
        ++offset;
    }

    return walker.peek(offset).kind == TokenKind::Identifier &&
           walker.peek(offset + 1).kind == TokenKind::Colon;
}

static bool is_at_statement_boundary(const TokenWalker& walker)
{
    return walker.check(TokenKind::Newline) ||
           walker.check(TokenKind::Semicolon) ||
           walker.check(TokenKind::RightBrace) ||
           walker.check(TokenKind::EndOfFile);
}

#pragma region Main Entry

RootSyntax* Parser::parse()
{
    auto* program = arena.alloc<RootSyntax>();
    Span startSpan = walker.current().span;

    skip_terminators(walker);

    while (!walker.is_at_end())
    {
        auto* decl = parse_declaration();
        if (decl)
        {
            program->declarations.push_back(decl);
        }
        skip_terminators(walker);
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
    skip_terminators(walker);

    Span modSpan = walker.current().span;
    Modifier mods = parse_modifiers();

    BaseDeclSyntax* decl = nullptr;

    if (walker.check(TokenKind::Fn))
    {
        decl = parse_function_decl();
    }
    else if (walker.check(TokenKind::Var))
    {
        decl = parse_variable_decl();
    }
    else if (walker.check(TokenKind::Type))
    {
        decl = parse_type_decl();
    }
    else if (walker.check(TokenKind::Namespace))
    {
        decl = parse_namespace_decl();
    }
    else
    {
        if (mods != Modifier::None)
        {
            error("modifiers must be followed by a declaration", walker.current().span);
        }
        walker.advance();
        return nullptr;
    }

    if (decl)
    {
        if (has_modifier(mods, Modifier::Ref) && !decl->is<TypeDeclSyntax>())
        {
            error("'ref' modifier can only be applied to type declarations", modSpan);
        }
        decl->modifiers = mods;
        if (mods != Modifier::None)
        {
            decl->span = modSpan.merge(decl->span);
        }
    }
    return decl;
}

FunctionDeclSyntax* Parser::parse_function_decl()
{
    auto* func = arena.alloc<FunctionDeclSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_terminators(walker);

    if (auto* name = expect(TokenKind::Identifier, "expected name after 'fn'"))
    {
        func->name = *name;
        span = span.merge(name->span);
    }
    skip_terminators(walker);

    parse_parameter_list(func->parameters, span);
    skip_terminators(walker);

    func->returnType = parse_return_type(span);
    skip_terminators(walker);

    func->body = parse_body(span);
    func->span = span;

    return func;
}

VariableDeclSyntax* Parser::parse_variable_decl()
{
    auto* var = arena.alloc<VariableDeclSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_terminators(walker);

    if (auto* name = expect(TokenKind::Identifier, "expected name after 'var'"))
    {
        var->name = *name;
        span = span.merge(name->span);
    }

    skip_terminators(walker);

    if (walker.check(TokenKind::Colon))
    {
        walker.advance();
        skip_terminators(walker);
        var->type = parse_type();
        if (var->type)
        {
            span = span.merge(var->type->span);
        }
    }

    skip_terminators(walker);

    if (walker.check(TokenKind::Assign))
    {
        walker.advance();
        skip_terminators(walker);
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
        skip_terminators(walker);
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
    skip_terminators(walker);

    if (auto* name = expect(TokenKind::Identifier, "expected name after 'type'"))
    {
        typeDecl->name = *name;
        span = span.merge(name->span);
    }

    skip_terminators(walker);

    if (walker.check(TokenKind::LeftBrace))
    {
        walker.advance();
        skip_terminators(walker);

        while (!walker.check(TokenKind::RightBrace) && !walker.is_at_end())
        {
            Span modSpan = walker.current().span;
            Modifier mods = parse_modifiers();

            BaseDeclSyntax* member = nullptr;

            if (walker.check(TokenKind::Init))
            {
                member = parse_init_decl();
            }
            else if (walker.check(TokenKind::Op))
            {
                member = parse_operator_decl();
            }
            else if (auto* field = parse_field_decl())
            {
                member = field;
            }
            else
            {
                member = parse_declaration();
            }

            if (member)
            {
                if (has_modifier(mods, Modifier::Ref) && !member->is<TypeDeclSyntax>())
                {
                    error("'ref' modifier can only be applied to type declarations", modSpan);
                }
                member->modifiers = member->modifiers | mods;
                if (mods != Modifier::None)
                {
                    member->span = modSpan.merge(member->span);
                }
                typeDecl->declarations.push_back(member);
            }
            skip_terminators(walker);
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
        skip_terminators(walker);
        field->type = parse_type();
        if (field->type)
        {
            span = span.merge(field->type->span);
        }
    }

    if (walker.check(TokenKind::Assign))
    {
        walker.advance();
        skip_terminators(walker);
        field->initializer = parse_expression();
        if (field->initializer)
        {
            span = span.merge(field->initializer->span);
        }
    }

    field->span = span;

    return field;
}

void Parser::parse_parameter_list(std::vector<ParameterDeclSyntax*>& out, Span& span)
{
    if (!walker.check(TokenKind::LeftParen))
    {
        return;
    }

    walker.advance();
    skip_terminators(walker);

    while (!walker.check(TokenKind::RightParen) && !walker.is_at_end())
    {
        auto* param = parse_parameter_decl();
        if (param)
        {
            out.push_back(param);
        }
        skip_terminators(walker);

        if (walker.check(TokenKind::Comma))
        {
            walker.advance();
            skip_terminators(walker);
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

BaseExprSyntax* Parser::parse_return_type(Span& span)
{
    if (!walker.check(TokenKind::ThinArrow))
    {
        return nullptr;
    }

    walker.advance();
    skip_terminators(walker);

    auto* type = parse_type();
    if (type)
    {
        span = span.merge(type->span);
    }
    return type;
}

BlockExprSyntax* Parser::parse_body(Span& span)
{
    if (!walker.check(TokenKind::LeftBrace))
    {
        return nullptr;
    }

    auto* body = parse_block();
    span = span.merge(body->span);
    return body;
}

InitDeclSyntax* Parser::parse_init_decl()
{
    auto* initDecl = arena.alloc<InitDeclSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_terminators(walker);

    parse_parameter_list(initDecl->parameters, span);
    skip_terminators(walker);

    initDecl->body = parse_body(span);
    initDecl->span = span;

    return initDecl;
}

OperatorDeclSyntax* Parser::parse_operator_decl()
{
    auto* opDecl = arena.alloc<OperatorDeclSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_terminators(walker);

    if (is_operator_token(walker.current().kind))
    {
        opDecl->op = walker.current();
        span = span.merge(walker.current().span);
        walker.advance();
    }
    else
    {
        error("expected operator symbol after 'op'", walker.current().span);
    }
    skip_terminators(walker);

    parse_parameter_list(opDecl->parameters, span);
    skip_terminators(walker);

    opDecl->returnType = parse_return_type(span);
    skip_terminators(walker);

    opDecl->body = parse_body(span);
    opDecl->span = span;

    return opDecl;
}

NamespaceDeclSyntax* Parser::parse_namespace_decl()
{
    auto* nsDecl = arena.alloc<NamespaceDeclSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_terminators(walker);

    if (auto* name = expect(TokenKind::Identifier, "expected name after 'namespace'"))
    {
        nsDecl->name = *name;
        span = span.merge(name->span);
    }

    skip_terminators(walker);

    if (walker.check(TokenKind::LeftBrace))
    {
        walker.advance();
        skip_terminators(walker);

        while (!walker.check(TokenKind::RightBrace) && !walker.is_at_end())
        {
            auto* decl = parse_declaration();
            if (decl)
            {
                nsDecl->declarations.push_back(decl);
            }
            skip_terminators(walker);
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
            skip_terminators(walker);
        }
    }

    nsDecl->span = span;

    return nsDecl;
}

#pragma region Statements

BaseStmtSyntax* Parser::parse_statement()
{
    skip_terminators(walker);

    if (walker.check(TokenKind::Return))
    {
        return parse_return_stmt();
    }

    if (is_modifier(walker.current().kind))
    {
        Span modSpan = walker.current().span;
        auto* decl = parse_declaration();
        if (decl && decl->is<VariableDeclSyntax>())
        {
            error("modifiers are not allowed on local variable declarations", modSpan);
            decl->modifiers = Modifier::None;
        }
        return decl;
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
        skip_terminators(walker);

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

BaseExprSyntax* Parser::parse_binary(Precedence minPrec)
{
    auto* left = parse_unary();
    if (!left)
    {
        return nullptr;
    }

    while (true)
    {
        auto binaryOp = to_binary_op(walker.current().kind);
        if (!binaryOp)
        {
            break;
        }

        Precedence prec = precedence_of(*binaryOp);
        if (prec < minPrec)
        {
            break;
        }

        walker.advance();
        skip_terminators(walker);

        auto* right = parse_binary(static_cast<Precedence>(static_cast<int>(prec) + 1));

        auto* binary = arena.alloc<BinaryExprSyntax>();
        binary->left = left;
        binary->op = *binaryOp;
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

BaseExprSyntax* Parser::parse_unary()
{
    if (walker.check(TokenKind::Minus) || walker.check(TokenKind::Plus))
    {
        UnaryOp op = walker.check(TokenKind::Minus) ? UnaryOp::Negative : UnaryOp::Positive;
        Span opSpan = walker.current().span;
        walker.advance();
        skip_terminators(walker);

        auto* operand = parse_unary();

        auto* unary = arena.alloc<UnaryExprSyntax>();
        unary->op = op;
        unary->operand = operand;

        Span span = opSpan;
        if (operand)
        {
            span = span.merge(operand->span);
        }
        unary->span = span;

        return unary;
    }

    return parse_postfix();
}

CallExprSyntax* Parser::parse_call(BaseExprSyntax* callee)
{
    auto* call = arena.alloc<CallExprSyntax>();
    call->callee = callee;

    walker.advance(); // consume '('
    skip_terminators(walker);

    while (!walker.check(TokenKind::RightParen) && !walker.is_at_end())
    {
        auto* arg = parse_expression();
        if (arg)
        {
            call->arguments.push_back(arg);
        }
        skip_terminators(walker);

        if (walker.check(TokenKind::Comma))
        {
            walker.advance();
            skip_terminators(walker);
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
    else
    {
        walker.synchronize_to(TokenKind::RightParen);
    }
    call->span = span;

    return call;
}

InitializerExprSyntax* Parser::parse_initializer(BaseExprSyntax* target)
{
    auto* init = arena.alloc<InitializerExprSyntax>();
    init->target = target;
    Span span = target->span;

    walker.advance();
    skip_terminators(walker);

    while (!walker.check(TokenKind::RightBrace) && !walker.is_at_end())
    {
        auto cp = walker.checkpoint();

        auto* fieldInit = arena.alloc<FieldInitSyntax>();
        Span fieldSpan = walker.current().span;

        if (auto* name = expect(TokenKind::Identifier, "expected field name in initializer"))
        {
            fieldInit->name = *name;
        }
        else
        {
            walker.synchronize_to(TokenKind::Comma);
            skip_terminators(walker);
            walker.check_progress(cp);
            continue;
        }

        if (!expect(TokenKind::Colon, "expected ':' after field name"))
        {
            walker.synchronize_to(TokenKind::Comma);
            skip_terminators(walker);
            walker.check_progress(cp);
            continue;
        }
        skip_terminators(walker);

        fieldInit->value = parse_expression();
        if (fieldInit->value)
        {
            fieldSpan = fieldSpan.merge(fieldInit->value->span);
        }

        fieldInit->span = fieldSpan;
        init->initializers.push_back(fieldInit);
        skip_terminators(walker);

        if (walker.check(TokenKind::Comma))
        {
            walker.advance();
            skip_terminators(walker);
        }

        walker.check_progress(cp);
    }

    if (auto* token = expect(TokenKind::RightBrace, "expected '}' after initializer list"))
    {
        span = span.merge(token->span);
    }

    init->span = span;

    return init;
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

    if (is_literal(walker.current().kind))
    {
        walker.advance();
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
        else if (is_initializer_list_ahead(walker))
        {
            left = parse_initializer(left);
        }
        else if (is_terminator(walker.current().kind))
        {
            auto cp = walker.checkpoint();
            skip_terminators(walker);
            if (is_initializer_list_ahead(walker))
            {
                left = parse_initializer(left);
            }
            else if (walker.check(TokenKind::Dot))
            {
                left = parse_member_access(left);
            }
            else if (walker.check(TokenKind::LeftParen))
            {
                left = parse_call(left);
            }
            else
            {
                walker.restore(cp);
                break;
            }
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

    if (is_literal(walker.current().kind))
    {
        auto* lit = arena.alloc<LiteralExprSyntax>();
        lit->token = walker.current();
        lit->span = lit->token.span;
        walker.advance();
        return lit;
    }

    if (walker.check(TokenKind::LeftParen))
    {
        auto* paren = arena.alloc<ParenExprSyntax>();
        Span span = walker.current().span;

        walker.advance();
        skip_terminators(walker);

        paren->expression = parse_expression();
        skip_terminators(walker);

        if (auto* token = expect(TokenKind::RightParen, "expected ')' after expression"))
        {
            span = span.merge(token->span);
        }
        paren->span = span;

        return paren;
    }

    if (walker.check(TokenKind::This))
    {
        auto* thisExpr = arena.alloc<ThisExprSyntax>();
        thisExpr->token = walker.current();
        thisExpr->span = walker.current().span;
        walker.advance();
        return thisExpr;
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
    skip_terminators(walker);

    while (!walker.check(TokenKind::RightBrace) && !walker.is_at_end())
    {
        auto cp = walker.checkpoint();

        auto* stmt = parse_statement();
        if (stmt)
        {
            block->statements.push_back(stmt);
        }
        skip_terminators(walker);

        walker.check_progress(cp);
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

    if (is_type_keyword(walker.current().kind) || walker.check(TokenKind::Identifier))
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
