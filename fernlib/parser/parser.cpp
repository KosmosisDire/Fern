#include "parser.hpp"

#include <format>

#include <parser/errors.hpp>

namespace Fern
{

Parser::Parser(TokenWalker& walker, AllocArena& arena, Diagnostics& diag)
    : walker(walker)
    , arena(arena)
    , diag(diag)
    , builder(arena)
{
}

const Token* Parser::expect(TokenKind kind, std::string_view message)
{
    if (walker.check(kind))
    {
        return &walker.advance();
    }
    diag.report(DiagnosticCode::Err_SyntaxError, walker.current().span, message);
    return nullptr;
}

void Parser::expect_progress(TokenWalker::Checkpoint cp)
{
    if (!walker.check_progress(cp))
    {
        diag.report(DiagnosticCode::Err_UnexpectedToken, walker.current().span, walker.current().lexeme);
        walker.advance();
    }
}

#pragma region Helpers

static void skip_newlines(TokenWalker& walker)
{
    while (walker.current().kind == TokenKind::Newline)
    {
        walker.advance();
    }
}

static void skip_statement_terminators(TokenWalker& walker)
{
    while (walker.current().kind == TokenKind::Newline ||
           walker.current().kind == TokenKind::Semicolon)
    {
        walker.advance();
    }
}

static void advance_past_field(TokenWalker& walker)
{
    skip_newlines(walker);
    if (walker.check(TokenKind::Comma))
    {
        walker.advance();
        skip_newlines(walker);
    }
}

static bool has_space_before(const Span& left, const Span& op)
{
    return left.endLine != op.startLine || left.endColumn != op.startColumn;
}

static bool has_space_after(const Span& op, const TokenWalker& walker)
{
    size_t offset = 1;
    while (is_terminator(walker.peek(offset).kind))
    {
        ++offset;
    }
    const Span& next = walker.peek(offset).span;
    return op.endLine != next.startLine || op.endColumn != next.startColumn;
}

// These tokens after a potential cast signify that we ARE doing a cast and not a parenthesized expression or call
static bool is_cast_trigger(TokenKind k)
{
    return k == TokenKind::Identifier ||
           k == TokenKind::This ||
           k == TokenKind::LeftParen ||
           k == TokenKind::Not ||
           is_literal(k);
}

void Parser::parse_attributes(std::vector<AttributeSyntax*>& out)
{
    while (walker.check(TokenKind::At))
    {
        Span span = walker.current().span;
        walker.advance();

        auto* value = parse_postfix();
        if (value)
        {
            out.push_back(builder.attribute(value, span.merge(value->span)));
        }
        else
        {
            diag.report(DiagnosticCode::Err_ExpectedAttributeName, span);
        }
        skip_newlines(walker);
    }
}

Modifier Parser::parse_modifiers(std::vector<Token>& outTokens)
{
    Modifier mods = Modifier::None;
    while (auto mod = to_modifier(walker.current().kind))
    {
        if (has_modifier(mods, *mod))
        {
            diag.report(DiagnosticCode::Err_DuplicateModifier, walker.current().span, walker.current().lexeme);
        }
        mods = mods | *mod;
        outTokens.push_back(walker.current());
        walker.advance();
        skip_newlines(walker);
    }
    return mods;
}

static bool scan_type_arg_list(const TokenWalker& walker, size_t offset)
{
    int depth = 1;
    while (depth > 0)
    {
        TokenKind kind = walker.peek(offset).kind;
        ++offset;
        if (kind == TokenKind::Less)
        {
            ++depth;
        }
        else if (kind == TokenKind::Greater)
        {
            --depth;
        }
        else if (kind == TokenKind::Identifier || kind == TokenKind::Comma || kind == TokenKind::Dot)
        {
            // valid inside type args
        }
        else
        {
            return false;
        }
    }
    TokenKind following = walker.peek(offset).kind;
    return following == TokenKind::LeftParen  ||
           following == TokenKind::LeftBrace  ||
           following == TokenKind::Greater    ||
           following == TokenKind::Comma      ||
           following == TokenKind::RightParen ||
           following == TokenKind::Dot        ||
           is_terminator(following)           ||
           following == TokenKind::EndOfFile;
}

static bool is_at_statement_boundary(const TokenWalker& walker)
{
    return is_terminator(walker.current().kind) ||
           walker.check(TokenKind::RightBrace) ||
           walker.check(TokenKind::EndOfFile);
}

#pragma region Main Entry

RootSyntax* Parser::parse()
{
    auto* program = arena.alloc<RootSyntax>();
    Span startSpan = walker.current().span;

    skip_statement_terminators(walker);

    while (!walker.is_at_end())
    {
        auto* decl = parse_declaration();
        if (decl)
        {
            program->declarations.push_back(expect_namespace_member(decl, diag, arena));
        }
        skip_statement_terminators(walker);
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
    skip_statement_terminators(walker);

    std::vector<AttributeSyntax*> attrs;
    parse_attributes(attrs);

    Span modSpan = walker.current().span;
    std::vector<Token> modTokens;
    Modifier mods = parse_modifiers(modTokens);

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
    else if (walker.check(TokenKind::Init))
    {
        decl = parse_init_decl();
    }
    else if (walker.check(TokenKind::Op))
    {
        decl = parse_operator_decl();
    }
    else if (walker.check(TokenKind::Literal))
    {
        decl = parse_literal_decl();
    }
    else if (walker.check(TokenKind::Cast))
    {
        decl = parse_cast_decl();
    }
    else if (auto* field = parse_field_decl())
    {
        decl = field;
    }
    else
    {
        if (!attrs.empty())
        {
            diag.report(DiagnosticCode::Err_ExpectedDeclaration, walker.current().span);
        }
        if (mods != Modifier::None)
        {
            diag.report(DiagnosticCode::Err_ModifiersWithoutDecl, walker.current().span);
        }
        walker.advance();
        return nullptr;
    }

    if (decl)
    {
        builder.attach_metadata(decl, mods, modSpan, modTokens, attrs);
        validate_modifier(decl, diag);
    }
    return decl;
}

CallableDeclSyntax* Parser::parse_function_decl()
{
    auto* func = arena.alloc<CallableDeclSyntax>();
    func->callableKind = CallableKind::Function;
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    if (auto* name = expect(TokenKind::Identifier, "expected name after 'fn'"))
    {
        func->name = *name;
        span = span.merge(name->span);
    }
    skip_newlines(walker);

    parse_parameter_list(func->parameters, span);
    skip_newlines(walker);

    func->returnType = parse_return_type(span);
    skip_newlines(walker);

    func->body = parse_body(span);
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

    skip_newlines(walker);

    if (walker.check(TokenKind::Colon))
    {
        walker.advance();
        skip_newlines(walker);
        var->type = parse_type();
        builder.merge_if(span, var->type);
    }

    skip_newlines(walker);

    bool hasAssign = false;
    if (walker.check(TokenKind::Assign))
    {
        hasAssign = true;
        Span assignSpan = walker.current().span;
        walker.advance();
        skip_newlines(walker);
        var->initializer = parse_expression();
        if (var->initializer)
        {
            span = span.merge(var->initializer->span);
        }
        else
        {
            diag.report(DiagnosticCode::Err_ExpectedExprAfterEq, assignSpan);
        }
    }

    if (!var->type && !hasAssign)
    {
        diag.report(DiagnosticCode::Err_VarRequiresTypeOrInit, var->name.span);
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
        builder.merge_if(span, param->type);
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

    if (walker.check(TokenKind::Less))
    {
        walker.advance();
        skip_newlines(walker);

        while (!walker.check(TokenKind::Greater) && !walker.is_at_end())
        {
            if (auto* param = expect(TokenKind::Identifier, "expected type parameter name"))
            {
                typeDecl->typeParams.push_back(*param);
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

        builder.merge_if(span, expect(TokenKind::Greater, "expected '>' after type parameters"));
        skip_newlines(walker);
    }

    if (walker.check(TokenKind::LeftBrace))
    {
        walker.advance();
        skip_statement_terminators(walker);

        while (!walker.check(TokenKind::RightBrace) && !walker.is_at_end())
        {
            auto* member = parse_declaration();
            if (member)
            {
                typeDecl->declarations.push_back(expect_type_member(member, diag, arena));
            }
            skip_statement_terminators(walker);
        }

        builder.merge_if(span, expect(TokenKind::RightBrace, "expected '}' after type body"));
    }
    else
    {
        diag.report(DiagnosticCode::Err_ExpectedOpenBrace, walker.current().span);
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

    field->name = walker.current();
    walker.advance();

    if (walker.check(TokenKind::Colon))
    {
        walker.advance();
        skip_newlines(walker);
        field->type = parse_type();
        builder.merge_if(span, field->type);
    }

    if (walker.check(TokenKind::Assign))
    {
        walker.advance();
        skip_newlines(walker);
        field->initializer = parse_expression();
        builder.merge_if(span, field->initializer);
    }

    field->span = span;

    return field;
}

void Parser::parse_parameter_list(ParameterListSyntax& out, Span& span)
{
    if (!walker.check(TokenKind::LeftParen))
    {
        out.span = span.at_end();
        return;
    }

    Span parametersSpan = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    while (!walker.check(TokenKind::RightParen) && !walker.is_at_end())
    {
        if (!walker.check(TokenKind::Identifier))
        {
            diag.report(DiagnosticCode::Err_SyntaxError, walker.current().span, "expected parameter name");
            while (!walker.check(TokenKind::Comma) &&
                   !walker.check(TokenKind::RightParen) &&
                   !walker.is_at_end())
            {
                walker.advance();
            }
            if (walker.check(TokenKind::Comma))
            {
                walker.advance();
                skip_newlines(walker);
                continue;
            }
            break;
        }

        if (auto* param = parse_parameter_decl())
        {
            out.list.push_back(param);
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
        parametersSpan = parametersSpan.merge(token->span);
        out.span = parametersSpan;
    }
}

TypeExprSyntax* Parser::parse_return_type(Span& span)
{
    if (!walker.check(TokenKind::ThinArrow))
    {
        return nullptr;
    }

    walker.advance();
    skip_newlines(walker);

    auto* type = parse_type();
    builder.merge_if(span, type);
    return type;
}

BlockSyntax* Parser::parse_body(Span& span)
{
    if (!walker.check(TokenKind::LeftBrace))
    {
        diag.report(DiagnosticCode::Err_ExpectedOpenBrace, walker.current().span);
        return nullptr;
    }

    auto* body = parse_block();
    span = span.merge(body->span);
    return body;
}

CallableDeclSyntax* Parser::parse_init_decl()
{
    auto* initDecl = arena.alloc<CallableDeclSyntax>();
    initDecl->callableKind = CallableKind::Constructor;
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    parse_parameter_list(initDecl->parameters, span);
    skip_newlines(walker);

    if (walker.check(TokenKind::ThinArrow))
    {
        diag.report(DiagnosticCode::Err_CtorReturnType, walker.current().span);
        walker.advance();
        skip_newlines(walker);
        parse_type();
        skip_newlines(walker);
    }

    if (walker.check(TokenKind::LeftBrace))
    {
        initDecl->body = parse_block();
        span = span.merge(initDecl->body->span);
    }
    else
    {
        diag.report(DiagnosticCode::Err_ExpectedOpenBrace, walker.current().span);
    }
    initDecl->span = span;

    return initDecl;
}

CallableDeclSyntax* Parser::parse_literal_decl()
{
    auto* decl = arena.alloc<CallableDeclSyntax>();
    decl->callableKind = CallableKind::Literal;
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    if (walker.check(TokenKind::Identifier) ||
        walker.check(TokenKind::Percent) ||
        walker.check(TokenKind::Dollar))
    {
        decl->name = walker.current();
        span = span.merge(walker.current().span);
        walker.advance();
    }
    else
    {
        diag.report(DiagnosticCode::Err_ExpectedLiteralSuffix, walker.current().span);
    }
    skip_newlines(walker);

    parse_parameter_list(decl->parameters, span);
    skip_newlines(walker);

    decl->returnType = parse_return_type(span);
    skip_newlines(walker);

    if (walker.check(TokenKind::LeftBrace))
    {
        decl->body = parse_block();
        span = span.merge(decl->body->span);
    }
    else
    {
        diag.report(DiagnosticCode::Err_ExpectedOpenBrace, walker.current().span);
    }
    decl->span = span;

    return decl;
}

CallableDeclSyntax* Parser::parse_cast_decl()
{
    auto* decl = arena.alloc<CallableDeclSyntax>();
    decl->callableKind = CallableKind::Cast;
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    parse_parameter_list(decl->parameters, span);
    skip_newlines(walker);

    decl->returnType = parse_return_type(span);
    skip_newlines(walker);

    if (walker.check(TokenKind::LeftBrace))
    {
        decl->body = parse_block();
        span = span.merge(decl->body->span);
    }
    else
    {
        diag.report(DiagnosticCode::Err_ExpectedOpenBrace, walker.current().span);
    }
    decl->span = span;

    return decl;
}

CallableDeclSyntax* Parser::parse_operator_decl()
{
    auto* opDecl = arena.alloc<CallableDeclSyntax>();
    opDecl->callableKind = CallableKind::Operator;
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    if (walker.check(TokenKind::LeftBracket))
    {
        Span bracketSpan = walker.current().span;
        walker.advance();

        if (auto* rb = expect(TokenKind::RightBracket, "expected ']' after '['"))
        {
            bracketSpan = bracketSpan.merge(rb->span);
        }

        if (walker.check(TokenKind::Assign))
        {
            bracketSpan = bracketSpan.merge(walker.current().span);
            walker.advance();
            opDecl->name = Token{TokenKind::IndexSetOp, bracketSpan, "[]="};
        }
        else
        {
            opDecl->name = Token{TokenKind::IndexOp, bracketSpan, "[]"};
        }
        span = span.merge(bracketSpan);
    }
    else if (is_operator_token(walker.current().kind))
    {
        opDecl->name = walker.current();
        span = span.merge(walker.current().span);
        walker.advance();
    }
    else
    {
        diag.report(DiagnosticCode::Err_ExpectedOpSymbol, span);
    }
    skip_newlines(walker);

    parse_parameter_list(opDecl->parameters, span);
    skip_newlines(walker);

    opDecl->returnType = parse_return_type(span);
    skip_newlines(walker);

    opDecl->body = parse_body(span);
    opDecl->span = span;

    return opDecl;
}

NamespaceDeclSyntax* Parser::parse_namespace_decl()
{
    auto* nsDecl = arena.alloc<NamespaceDeclSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    if (auto* name = expect(TokenKind::Identifier, "expected name after 'namespace'"))
    {
        BaseExprSyntax* nameExpr = builder.identifier(*name);

        while (walker.check(TokenKind::Dot))
        {
            nameExpr = parse_member_access(nameExpr);
        }

        nsDecl->name = nameExpr;
        span = span.merge(nameExpr->span);
    }

    skip_newlines(walker);

    if (walker.check(TokenKind::LeftBrace))
    {
        walker.advance();
        skip_statement_terminators(walker);

        while (!walker.check(TokenKind::RightBrace) && !walker.is_at_end())
        {
            auto* decl = parse_declaration();
            if (decl)
            {
                nsDecl->declarations.push_back(expect_namespace_member(decl, diag, arena));
            }
            skip_statement_terminators(walker);
        }

        builder.merge_if(span, expect(TokenKind::RightBrace, "expected '}' after namespace body"));
    }
    else
    {
        nsDecl->isFileLevel = true;
        while (!walker.is_at_end())
        {
            auto* decl = parse_declaration();
            if (decl)
            {
                nsDecl->declarations.push_back(expect_namespace_member(decl, diag, arena));
                span = span.merge(decl->span);
            }
            skip_statement_terminators(walker);
        }
    }

    nsDecl->span = span;

    return nsDecl;
}

#pragma region Statements

BaseStmtSyntax* Parser::parse_statement()
{
    skip_statement_terminators(walker);

    if (walker.check(TokenKind::Return))
    {
        return parse_return_stmt();
    }

    if (walker.check(TokenKind::If))
    {
        return parse_if();
    }

    if (walker.check(TokenKind::While))
    {
        return parse_while();
    }

    if (walker.check(TokenKind::Var))
    {
        return parse_variable_decl();
    }

    if (walker.check(TokenKind::LeftBrace))
    {
        return parse_block();
    }

    if (walker.check(TokenKind::At) || is_modifier(walker.current().kind) ||
        is_declaration_keyword(walker.current().kind))
    {
        auto* result = expect_function_body_stmt(parse_declaration(), diag, arena);
        if (auto* var = result ? result->as<VariableDeclSyntax>() : nullptr)
        {
            validate_local(var, diag);
        }
        return result;
    }

    if (walker.check(TokenKind::Identifier) && walker.peek(1).kind == TokenKind::Colon)
    {
        if (auto* field = parse_field_decl())
        {
            return make_error_stmt(field, DiagnosticCode::Err_BadFunctionBodyContent, diag, arena);
        }
    }

    if (is_expression_start(walker.current().kind))
    {
        if (auto* expr = parse_expression())
        {
            return builder.expr_stmt(expr);
        }
    }

    if (!is_terminator(walker.current().kind) &&
        !walker.check(TokenKind::RightBrace) &&
        !walker.is_at_end())
    {
        Token bad = walker.current();
        diag.report(DiagnosticCode::Err_UnexpectedToken, bad.span, bad.lexeme);
        walker.advance();
        return builder.error_stmt(bad.span);
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
        builder.merge_if(span, stmt->value);
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
        walker.advance();
        skip_newlines(walker);

        auto* value = parse_assignment();
        return builder.assignment(left, *assignOp, value);
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

    if (walker.check(TokenKind::LiteralSuffix))
    {
        Token suffixTok = walker.current();
        walker.advance();
        left = builder.literal_suffix(left, suffixTok);
    }

    while (true)
    {
        auto cp = walker.checkpoint();
        bool crossedTerminator = is_terminator(walker.current().kind);
        skip_newlines(walker);

        auto binaryOp = to_binary_op(walker.current().kind);
        if (!binaryOp)
        {
            walker.restore(cp);
            break;
        }

        TokenKind opKind = walker.current().kind;
        if (opKind == TokenKind::Plus || opKind == TokenKind::Minus)
        {
            bool spaceLeft = has_space_before(left->span, walker.current().span);
            bool spaceRight = has_space_after(walker.current().span, walker);
            if (spaceLeft && !spaceRight)
            {
                if (crossedTerminator)
                {
                    walker.restore(cp);
                    break;
                }
                diag.report(DiagnosticCode::Err_AmbiguousOpSpacing,
                      walker.current().span,
                      walker.current().lexeme,
                      walker.current().lexeme,
                      walker.peek(1).lexeme);
                walker.restore(cp);
                break;
            }
        }

        Precedence prec = precedence_of(*binaryOp);
        if (prec < minPrec)
        {
            walker.restore(cp);
            break;
        }

        Span opSpan = walker.current().span;
        walker.advance();
        skip_newlines(walker);

        auto* right = parse_binary(static_cast<Precedence>(static_cast<int>(prec) + 1));
        if (!right)
        {
            diag.report(DiagnosticCode::Err_ExpectedExprAfterOp,
                  opSpan.at_end(),
                  Fern::format(opKind));
        }

        left = builder.binary(left, *binaryOp, right);
    }

    return left;
}

BaseExprSyntax* Parser::parse_unary()
{
    auto unaryOp = to_unary_op(walker.current().kind);
    if (unaryOp)
    {
        bool spaceRight = has_space_after(walker.current().span, walker);
        TokenKind opKind = walker.current().kind;
        if (spaceRight)
        {
            if (opKind == TokenKind::Plus || opKind == TokenKind::Minus)
            {
                return parse_postfix();
            }
            diag.report(DiagnosticCode::Err_UnaryOpDetached, walker.current().span);
        }

        Span span = walker.current().span;
        walker.advance();
        skip_newlines(walker);

        auto* operand = parse_unary();
        builder.merge_if(span, operand);
        return builder.unary(*unaryOp, operand, span);
    }

    return parse_postfix();
}

CallExprSyntax* Parser::parse_call(BaseExprSyntax* callee)
{
    auto* call = arena.alloc<CallExprSyntax>();
    call->callee = callee;

    walker.advance();
    skip_newlines(walker);

    while (!walker.check(TokenKind::RightParen) && !walker.is_at_end())
    {
        if (is_statement_keyword(walker.current().kind))
        {
            break;
        }

        auto* arg = parse_expression();
        if (arg)
        {
            call->arguments.push_back(arg);
        }
        else
        {
            break;
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
    builder.merge_if(span, expect(TokenKind::RightParen, "expected ')' after arguments"));
    call->span = span;

    return call;
}

void Parser::parse_initializer_members(std::vector<StmtPtr>& out)
{
    while (!walker.check(TokenKind::RightBrace) && !walker.is_at_end())
    {
        if (is_statement_keyword(walker.current().kind))
        {
            break;
        }

        auto cp = walker.checkpoint();

        if (walker.check(TokenKind::Comma))
        {
            diag.report(DiagnosticCode::Err_UnexpectedCommaInInit, walker.current().span);
            walker.advance();
            advance_past_field(walker);
            expect_progress(cp);
            continue;
        }

        auto* expr = parse_postfix();
        if (!expr)
        {
            advance_past_field(walker);
            expect_progress(cp);
            continue;
        }

        if ((expr->is<IdentifierExprSyntax>() || expr->is<MemberAccessExprSyntax>()) &&
            walker.check(TokenKind::Colon))
        {
            Span fieldSpan = expr->span;

            walker.advance();
            skip_newlines(walker);

            if (walker.check(TokenKind::Comma) ||
                walker.check(TokenKind::RightBrace) ||
                is_terminator(walker.current().kind) ||
                is_statement_keyword(walker.current().kind))
            {
                diag.report(DiagnosticCode::Err_ExpectedValueAfterColon, walker.current().span);
                out.push_back(builder.field_init(expr, nullptr, fieldSpan));
                advance_past_field(walker);
                expect_progress(cp);
                continue;
            }

            auto* value = parse_expression();
            builder.merge_if(fieldSpan, value);
            out.push_back(builder.field_init(expr, value, fieldSpan));
        }
        else
        {
            out.push_back(builder.expr_stmt(expr));
        }

        skip_newlines(walker);

        if (walker.check(TokenKind::Comma))
        {
            walker.advance();
            skip_newlines(walker);
        }

        expect_progress(cp);
    }
}

InitializerExprSyntax* Parser::parse_initializer(BaseExprSyntax* target)
{
    auto* init = arena.alloc<InitializerExprSyntax>();
    init->target = target;
    Span span = target ? target->span : walker.current().span;

    walker.advance();
    skip_newlines(walker);

    parse_initializer_members(init->members);

    builder.merge_if(span, expect(TokenKind::RightBrace, "expected '}' after initializer list"));

    init->span = span;

    return init;
}

MemberAccessExprSyntax* Parser::parse_member_access(BaseExprSyntax* left)
{
    walker.advance();

    if (walker.check(TokenKind::Identifier))
    {
        return builder.member_access(left, parse_simple_name());
    }

    diag.report(DiagnosticCode::Err_ExpectedMemberAfterDot, walker.current().span);

    if (is_literal(walker.current().kind))
    {
        walker.advance();
    }

    return builder.member_access(left, nullptr);
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
        auto cp = walker.checkpoint();
        bool skippedNewline = walker.current().kind == TokenKind::Newline;
        if (skippedNewline)
        {
            skip_newlines(walker);
        }

        if (walker.check(TokenKind::Dot))
        {
            left = parse_member_access(left);
        }
        else if (walker.check(TokenKind::LeftParen))
        {
            left = parse_call(left);
        }
        else if (walker.check(TokenKind::LeftBracket))
        {
            Span bracketSpan = left->span;
            walker.advance();
            skip_newlines(walker);

            auto* indexValue = parse_expression();
            skip_newlines(walker);

            builder.merge_if(bracketSpan, expect(TokenKind::RightBracket, "expected ']' after index expression"));
            left = builder.index(left, indexValue, bracketSpan);
        }
        else if (walker.check(TokenKind::LeftBrace) && !inCondition)
        {
            left = parse_initializer(left);
        }
        else
        {
            walker.restore(cp);
            break;
        }
    }

    return left;
}

BaseExprSyntax* Parser::parse_primary()
{
    if (walker.check(TokenKind::Identifier))
    {
        return parse_simple_name();
    }

    if (is_literal(walker.current().kind))
    {
        Token tok = walker.current();
        walker.advance();
        return builder.literal(tok);
    }

    if (walker.check(TokenKind::LeftParen))
    {
        Span span = walker.current().span;

        auto tokenCP = walker.checkpoint();
        auto diagCP = diag.checkpoint();

        walker.advance();
        skip_newlines(walker);

        TypeExprSyntax* maybeType = parse_type();
        skip_newlines(walker);
        bool endsAtCloseParen = maybeType && walker.check(TokenKind::RightParen);

        bool isCast = false;
        if (endsAtCloseParen)
        {
            bool unambiguous = maybeType->is<GenericNameExprSyntax>() ||
                               maybeType->is<ArrayTypeExprSyntax>();
            TokenKind afterParen = walker.peek(1).kind;
            isCast = unambiguous || is_cast_trigger(afterParen);
        }

        if (isCast)
        {
            Span closingSpan = walker.current().span;
            walker.advance();
            span = span.merge(closingSpan);

            auto* operand = parse_unary();
            builder.merge_if(span, operand);
            return builder.cast(maybeType, operand, span);
        }

        walker.restore(tokenCP);
        diag.restore(diagCP);

        walker.advance();
        skip_newlines(walker);

        bool wasInCondition = inCondition;
        inCondition = false;
        auto* inner = parse_expression();
        inCondition = wasInCondition;
        skip_newlines(walker);

        builder.merge_if(span, expect(TokenKind::RightParen, "expected ')' after expression"));
        return builder.paren(inner, span);
    }

    if (walker.check(TokenKind::This))
    {
        Token tok = walker.current();
        walker.advance();
        return builder.this_expr(tok);
    }

    if (walker.check(TokenKind::LeftBracket))
    {
        auto* arrayLit = arena.alloc<ArrayLiteralExprSyntax>();
        Span span = walker.current().span;

        walker.advance();
        skip_newlines(walker);

        while (!walker.check(TokenKind::RightBracket) && !walker.is_at_end())
        {
            auto* elem = parse_expression();
            if (elem)
            {
                arrayLit->elements.push_back(elem);
            }
            else
            {
                break;
            }
            skip_newlines(walker);

            if (walker.check(TokenKind::Comma))
            {
                walker.advance();
                skip_newlines(walker);
            }
        }

        builder.merge_if(span, expect(TokenKind::RightBracket, "expected ']' after array literal"));
        arrayLit->span = span;

        return arrayLit;
    }

    return nullptr;
}

IfStmtSyntax* Parser::parse_if()
{
    auto* ifStmt = arena.alloc<IfStmtSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    inCondition = true;
    ifStmt->condition = parse_expression();
    inCondition = false;
    skip_newlines(walker);

    if (walker.check(TokenKind::LeftBrace))
    {
        ifStmt->thenBody = parse_block();
        span = span.merge(ifStmt->thenBody->span);
    }
    else
    {
        diag.report(DiagnosticCode::Err_ExpectedOpenBrace, ifStmt->condition ? ifStmt->condition->span : span);
    }

    skip_newlines(walker);

    if (walker.check(TokenKind::Else))
    {
        Span elseSpan = walker.current().span;
        walker.advance();
        skip_newlines(walker);

        if (walker.check(TokenKind::If))
        {
            ifStmt->elseIf = parse_if();
            span = span.merge(ifStmt->elseIf->span);
        }
        else if (walker.check(TokenKind::LeftBrace))
        {
            ifStmt->elseBlock = parse_block();
            span = span.merge(ifStmt->elseBlock->span);
        }
        else
        {
            diag.report(DiagnosticCode::Err_ExpectedElseBody, elseSpan);
        }
    }

    ifStmt->span = span;
    return ifStmt;
}

WhileStmtSyntax* Parser::parse_while()
{
    auto* whileStmt = arena.alloc<WhileStmtSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_newlines(walker);

    inCondition = true;
    whileStmt->condition = parse_expression();
    inCondition = false;
    skip_newlines(walker);

    if (walker.check(TokenKind::LeftBrace))
    {
        whileStmt->body = parse_block();
        span = span.merge(whileStmt->body->span);
    }
    else
    {
        diag.report(DiagnosticCode::Err_ExpectedOpenBrace, whileStmt->condition ? whileStmt->condition->span : span);
    }

    whileStmt->span = span;
    return whileStmt;
}

BlockSyntax* Parser::parse_block()
{
    auto* block = arena.alloc<BlockSyntax>();
    Span span = walker.current().span;

    walker.advance();
    skip_statement_terminators(walker);

    while (!walker.check(TokenKind::RightBrace) && !walker.is_at_end())
    {
        auto cp = walker.checkpoint();

        auto* stmt = parse_statement();
        if (stmt)
        {
            block->statements.push_back(stmt);
        }
        skip_statement_terminators(walker);

        expect_progress(cp);
    }

    builder.merge_if(span, expect(TokenKind::RightBrace, "expected '}' after block"));
    block->span = span;

    return block;
}

#pragma region Types

SimpleNameExprSyntax* Parser::parse_simple_name()
{
    if (!walker.check(TokenKind::Identifier))
    {
        return nullptr;
    }

    Token nameTok = walker.current();
    Span span = nameTok.span;
    walker.advance();

    if (walker.check(TokenKind::Less) &&
        (scan_type_arg_list(walker, 1) ||
         !has_space_before(nameTok.span, walker.current().span)))
    {
        auto* gen = arena.alloc<GenericNameExprSyntax>();
        gen->name = nameTok;

        walker.advance();
        skip_newlines(walker);

        while (!walker.check(TokenKind::Greater) && !walker.is_at_end())
        {
            auto* typeArg = parse_type();
            if (typeArg)
            {
                gen->typeArgs.push_back(typeArg);
                span = span.merge(typeArg->span);
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

        builder.merge_if(span, expect(TokenKind::Greater, "expected '>' after type arguments"));

        gen->span = span;
        return gen;
    }

    return builder.identifier(nameTok);
}

TypeExprSyntax* Parser::parse_type()
{
    if (!walker.check(TokenKind::Identifier))
    {
        return nullptr;
    }

    TypeExprSyntax* type = parse_simple_name();

    while (walker.check(TokenKind::Dot))
    {
        type = parse_member_access(type);
    }

    while (walker.check(TokenKind::LeftBracket) && walker.peek(1).kind == TokenKind::RightBracket)
    {
        walker.advance();
        Span closingSpan = walker.current().span;
        walker.advance();

        type = builder.array_type(type, type->span.merge(closingSpan));
    }

    return type;
}

}
