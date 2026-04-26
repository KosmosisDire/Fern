#include "parser.hpp"

namespace Fern
{

Parser::Parser(TokenWalker& walker, AllocArena& arena, Diagnostics& diag)
    : walker(walker)
    , arena(arena)
    , diag(diag)
{
}

const Token* Parser::expect(TokenKind kind, std::string_view message)
{
    if (walker.check(kind))
    {
        return &walker.advance();
    }
    diag.error(message, walker.current().span);
    return nullptr;
}

void Parser::expect_progress(TokenWalker::Checkpoint cp)
{
    if (!walker.check_progress(cp))
    {
        diag.error("unexpected token '" + std::string(walker.current().lexeme) + "'", walker.current().span);
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
            auto* attr = arena.alloc<AttributeSyntax>();
            attr->value = value;
            attr->span = span.merge(value->span);
            out.push_back(attr);
        }
        else
        {
            diag.error("expected attribute name after '@'", span);
        }
        skip_newlines(walker);
    }
}

Modifier Parser::parse_modifiers()
{
    Modifier mods = Modifier::None;
    while (auto mod = to_modifier(walker.current().kind))
    {
        if (has_modifier(mods, *mod))
        {
            diag.error("duplicate modifier '" + std::string(walker.current().lexeme) + "'", walker.current().span);
        }
        mods = mods | *mod;
        walker.advance();
        skip_newlines(walker);
    }
    return mods;
}

void Parser::attach_declaration_metadata(BaseDeclSyntax* decl, Modifier mods, Span modSpan, std::vector<AttributeSyntax*>& attrs)
{
    decl->modifiers = decl->modifiers | mods;
    decl->attributes = std::move(attrs);
    if (mods != Modifier::None)
    {
        decl->span = modSpan.merge(decl->span);
    }
    if (!decl->attributes.empty())
    {
        decl->span = decl->attributes.front()->span.merge(decl->span);
    }
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
            program->declarations.push_back(decl);
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
            diag.error("expected a declaration", walker.current().span);
        }
        if (mods != Modifier::None)
        {
            diag.error("modifiers must be followed by a declaration", walker.current().span);
        }
        walker.advance();
        return nullptr;
    }

    if (decl)
    {
        attach_declaration_metadata(decl, mods, modSpan, attrs);
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
        if (var->type)
        {
            span = span.merge(var->type->span);
        }
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
            diag.error("expected expression after '='", assignSpan);
        }
    }

    if (!var->type && !hasAssign)
    {
        diag.error("variable declaration requires a type annotation or initializer", var->name.span);
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

        if (auto* token = expect(TokenKind::Greater, "expected '>' after type parameters"))
        {
            span = span.merge(token->span);
        }
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
                typeDecl->declarations.push_back(member);
            }
            skip_statement_terminators(walker);
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

    field->name = walker.current();
    walker.advance();

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

void Parser::parse_parameter_list(std::vector<ParameterDeclSyntax*>& out, Span& span)
{
    if (!walker.check(TokenKind::LeftParen))
    {
        return;
    }

    walker.advance();
    skip_newlines(walker);

    while (!walker.check(TokenKind::RightParen) && !walker.is_at_end())
    {
        if (is_statement_keyword(walker.current().kind))
        {
            break;
        }

        auto* param = parse_parameter_decl();
        if (param)
        {
            out.push_back(param);
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

    if (auto* token = expect(TokenKind::RightParen, "expected ')' after parameter list"))
    {
        span = span.merge(token->span);
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
    if (type)
    {
        span = span.merge(type->span);
    }
    return type;
}

BlockSyntax* Parser::parse_body(Span& span)
{
    if (!walker.check(TokenKind::LeftBrace))
    {
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
        diag.error("constructors cannot have a return type annotation", walker.current().span);
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
        diag.error("expected '{' after constructor declaration", walker.current().span);
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
        diag.error("expected literal suffix name", walker.current().span);
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
        diag.error("expected '{' after literal declaration", walker.current().span);
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
        diag.error("expected '{' after cast declaration", walker.current().span);
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
        diag.error("expected operator symbol after 'op'", span);
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
        auto* ident = arena.alloc<IdentifierExprSyntax>();
        ident->name = *name;
        ident->span = name->span;
        BaseExprSyntax* nameExpr = ident;

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
                nsDecl->declarations.push_back(decl);
            }
            skip_statement_terminators(walker);
        }

        if (auto* token = expect(TokenKind::RightBrace, "expected '}' after namespace body"))
        {
            span = span.merge(token->span);
        }
    }
    else
    {
        nsDecl->isFileLevel = true;
        while (!walker.is_at_end())
        {
            auto* decl = parse_declaration();
            if (decl)
            {
                nsDecl->declarations.push_back(decl);
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

    if (walker.check(TokenKind::At) || is_modifier(walker.current().kind) ||
        is_declaration_keyword(walker.current().kind))
    {
        return parse_declaration();
    }

    if (walker.check(TokenKind::Var))
    {
        return parse_variable_decl();
    }

    if (walker.check(TokenKind::LeftBrace))
    {
        return parse_block();
    }

    auto* expr = parse_expression();
    if (expr)
    {
        auto* stmt = arena.alloc<ExpressionStmtSyntax>();
        stmt->expression = expr;
        stmt->span = expr->span;
        return stmt;
    }

    if (!is_terminator(walker.current().kind) &&
        !walker.check(TokenKind::RightBrace) &&
        !walker.is_at_end())
    {
        diag.error("unexpected token '" + std::string(walker.current().lexeme) + "'", walker.current().span);
        walker.advance();
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

BaseExprSyntax* Parser::parse_binary(Precedence minPrec)
{
    auto* left = parse_unary();
    if (!left)
    {
        return nullptr;
    }

    if (walker.check(TokenKind::LiteralSuffix))
    {
        auto* suffixExpr = arena.alloc<LiteralSuffixExprSyntax>();
        suffixExpr->operand = left;
        suffixExpr->suffix = walker.current();
        suffixExpr->span = left->span.merge(walker.current().span);
        walker.advance();
        left = suffixExpr;
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
                diag.error("ambiguous operator spacing: '" +
                      std::string(walker.current().lexeme) +
                      "' has space on the left but not the right. Did you mean '... " +
                      std::string(walker.current().lexeme) + " " +
                      std::string(walker.peek(1).lexeme) + "'?",
                      walker.current().span);
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

        walker.advance();
        skip_newlines(walker);

        auto* right = parse_binary(static_cast<Precedence>(static_cast<int>(prec) + 1));
        if (!right)
        {
            diag.error("expected expression after '" + std::string(Fern::format(opKind)) + "'",
                  walker.current().span);
        }

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
            diag.error("unary operator cannot be separated from its operand", walker.current().span);
        }

        Span opSpan = walker.current().span;
        walker.advance();
        skip_newlines(walker);

        auto* operand = parse_unary();

        auto* unary = arena.alloc<UnaryExprSyntax>();
        unary->op = *unaryOp;
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
    if (auto* token = expect(TokenKind::RightParen, "expected ')' after arguments"))
    {
        span = span.merge(token->span);
    }
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
            diag.error("unexpected ',' before initializer member", walker.current().span);
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
            auto* fieldInit = arena.alloc<FieldInitSyntax>();
            fieldInit->target = expr;
            Span fieldSpan = expr->span;

            walker.advance();
            skip_newlines(walker);

            if (walker.check(TokenKind::Comma) ||
                walker.check(TokenKind::RightBrace) ||
                is_terminator(walker.current().kind) ||
                is_statement_keyword(walker.current().kind))
            {
                diag.error("expected value after ':'", walker.current().span);
                fieldInit->span = fieldSpan;
                out.push_back(fieldInit);
                advance_past_field(walker);
                expect_progress(cp);
                continue;
            }

            fieldInit->value = parse_expression();
            if (fieldInit->value)
            {
                fieldSpan = fieldSpan.merge(fieldInit->value->span);
            }

            fieldInit->span = fieldSpan;
            out.push_back(fieldInit);
        }
        else
        {
            auto* childStmt = arena.alloc<ExpressionStmtSyntax>();
            childStmt->expression = expr;
            childStmt->span = expr->span;
            out.push_back(childStmt);
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

    if (auto* token = expect(TokenKind::RightBrace, "expected '}' after initializer list"))
    {
        span = span.merge(token->span);
    }

    init->span = span;

    return init;
}

MemberAccessExprSyntax* Parser::parse_member_access(BaseExprSyntax* left)
{
    walker.advance();

    auto* memberAccess = arena.alloc<MemberAccessExprSyntax>();
    memberAccess->left = left;

    if (walker.check(TokenKind::Identifier))
    {
        auto* right = parse_simple_name();
        memberAccess->right = right;
        memberAccess->span = left->span.merge(right->span);
        return memberAccess;
    }

    diag.error("expected member name after '.'", walker.current().span);

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

            auto* indexExpr = arena.alloc<IndexExprSyntax>();
            indexExpr->object = left;
            indexExpr->index = parse_expression();
            skip_newlines(walker);

            if (auto* rb = expect(TokenKind::RightBracket, "expected ']' after index expression"))
            {
                bracketSpan = bracketSpan.merge(rb->span);
            }
            indexExpr->span = bracketSpan;
            left = indexExpr;
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
        auto* lit = arena.alloc<LiteralExprSyntax>();
        lit->token = walker.current();
        lit->span = lit->token.span;
        walker.advance();
        return lit;
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

            auto* cast = arena.alloc<CastExprSyntax>();
            cast->type = maybeType;
            cast->operand = parse_unary();
            cast->span = span;
            if (cast->operand)
            {
                cast->span = cast->span.merge(cast->operand->span);
            }
            return cast;
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

        if (auto* token = expect(TokenKind::RightParen, "expected ')' after expression"))
        {
            span = span.merge(token->span);
        }

        auto* paren = arena.alloc<ParenExprSyntax>();
        paren->expression = inner;
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

        if (auto* token = expect(TokenKind::RightBracket, "expected ']' after array literal"))
        {
            span = span.merge(token->span);
        }
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
        diag.error("expected '{' after if condition", ifStmt->condition ? ifStmt->condition->span : span);
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
            diag.error("expected '{' or 'if' after 'else'", elseSpan);
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
        diag.error("expected '{' after while condition", whileStmt->condition ? whileStmt->condition->span : span);
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

    if (auto* token = expect(TokenKind::RightBrace, "expected '}' after block"))
    {
        span = span.merge(token->span);
    }
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

        if (auto* token = expect(TokenKind::Greater, "expected '>' after type arguments"))
        {
            span = span.merge(token->span);
        }

        gen->span = span;
        return gen;
    }

    auto* ident = arena.alloc<IdentifierExprSyntax>();
    ident->name = nameTok;
    ident->span = span;
    return ident;
}

QualifiedNameExprSyntax* Parser::parse_qualified_name(TypeExprSyntax* left)
{
    auto* node = arena.alloc<QualifiedNameExprSyntax>();
    node->left = left;

    walker.advance();

    if (walker.check(TokenKind::Identifier))
    {
        auto* right = parse_simple_name();
        node->right = right;
        node->span = left->span.merge(right->span);
        return node;
    }

    diag.error("expected member name after '.'", walker.current().span);
    node->span = left->span;
    return node;
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
        type = parse_qualified_name(type);
    }

    while (walker.check(TokenKind::LeftBracket) && walker.peek(1).kind == TokenKind::RightBracket)
    {
        walker.advance();
        Span closingSpan = walker.current().span;
        walker.advance();

        auto* arrType = arena.alloc<ArrayTypeExprSyntax>();
        arrType->elementType = type;
        arrType->span = type->span.merge(closingSpan);
        type = arrType;
    }

    return type;
}

}
