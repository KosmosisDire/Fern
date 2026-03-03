#include "lower.hpp"
#include <string>
#include <cassert>
#include <arena.hpp>
#include <semantic/context.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

#pragma region Conversion Helpers

constexpr IntrinsicOp to_intrinsic_op(BinaryOp op)
{
    switch (op)
    {
        case BinaryOp::Add:          return IntrinsicOp::Add;
        case BinaryOp::Sub:          return IntrinsicOp::Sub;
        case BinaryOp::Mul:          return IntrinsicOp::Mul;
        case BinaryOp::Div:          return IntrinsicOp::Div;
        case BinaryOp::Greater:      return IntrinsicOp::Greater;
        case BinaryOp::Less:         return IntrinsicOp::Less;
        case BinaryOp::GreaterEqual: return IntrinsicOp::GreaterEqual;
        case BinaryOp::LessEqual:    return IntrinsicOp::LessEqual;
        case BinaryOp::Equal:        return IntrinsicOp::Equal;
        case BinaryOp::NotEqual:     return IntrinsicOp::NotEqual;
        case BinaryOp::And:          return IntrinsicOp::And;
        case BinaryOp::Or:           return IntrinsicOp::Or;
    }
}

constexpr IntrinsicOp to_intrinsic_op(UnaryOp op)
{
    switch (op)
    {
        case UnaryOp::Negative: return IntrinsicOp::Negative;
        case UnaryOp::Positive: return IntrinsicOp::Positive;
        case UnaryOp::Not:      return IntrinsicOp::Not;
    }
}

#pragma region Constructor

FhirLowerer::FhirLowerer(SemanticContext& ctx, AllocArena& arena)
    : context(ctx), arena(arena) {}

template<typename T>
T* FhirLowerer::make()
{
    return arena.alloc<T>();
}

#pragma region Public

std::vector<FhirMethod*> FhirLowerer::lower_all()
{
    collect_methods(context.symbols.globalNamespace);
    return std::move(result);
}

#pragma region Top-level

bool FhirLowerer::is_builtin_type(NamedTypeSymbol* type)
{
    for (const auto& attr : type->resolvedAttributes)
    {
        if (attr.type && attr.type->qualified_name() == "Core.BuiltinType")
            return true;
    }
    return false;
}

void FhirLowerer::collect_methods(NamespaceSymbol* ns)
{
    if (!ns) return;
    for (auto& [name, member] : ns->members)
    {
        if (auto* childNs = member->as<NamespaceSymbol>())
        {
            collect_methods(childNs);
        }
        else if (auto* type = member->as<NamedTypeSymbol>())
        {
            if (is_builtin_type(type)) continue;
            for (auto* method : type->methods)
            {
                if (method->syntax)
                    result.push_back(lower_method(method));
                else if (method->isConstructor)
                    result.push_back(lower_synthetic_constructor(method, type));
            }
        }
    }
}

FhirMethod* FhirLowerer::lower_method(MethodSymbol* method)
{
    BlockSyntax* astBody = nullptr;
    if (auto* func = method->syntax->as<FunctionDeclSyntax>())
        astBody = func->body;
    else if (auto* op = method->syntax->as<OperatorDeclSyntax>())
        astBody = op->body;
    else if (auto* init = method->syntax->as<InitDeclSyntax>())
        astBody = init->body;

    auto* m = arena.alloc<FhirMethod>();
    m->symbol = method;
    m->body = astBody ? lower_block(astBody) : nullptr;
    return m;
}

FhirMethod* FhirLowerer::lower_synthetic_constructor(MethodSymbol* method, NamedTypeSymbol* parentType)
{
    auto* block = make<FhirBlock>();

    for (auto* param : method->parameters)
    {
        auto* field = parentType->find_field(param->name);
        if (!field) continue;

        auto* thisExpr = make<FhirThisExpr>();
        thisExpr->type = parentType;

        auto* fieldAccess = make<FhirFieldAccessExpr>();
        fieldAccess->type = field->type;
        fieldAccess->object = thisExpr;
        fieldAccess->field = field;

        auto* paramRef = make<FhirParamRefExpr>();
        paramRef->type = param->type;
        paramRef->parameter = param;

        auto* assign = make<FhirAssignExpr>();
        assign->type = field->type;
        assign->target = fieldAccess;
        assign->value = paramRef;

        auto* stmt = make<FhirExprStmt>();
        stmt->expression = assign;
        block->statements.push_back(stmt);
    }

    auto* m = arena.alloc<FhirMethod>();
    m->symbol = method;
    m->body = block;
    return m;
}

#pragma region Expressions

FhirExpr* FhirLowerer::lower_expr(BaseExprSyntax* ast)
{
    if (!ast) return nullptr;
    if (auto* n = ast->as<LiteralExprSyntax>())      return lower_literal(n);
    if (auto* n = ast->as<IdentifierExprSyntax>())    return lower_identifier(n);
    if (auto* n = ast->as<ThisExprSyntax>())          return lower_this(n);
    if (auto* n = ast->as<BinaryExprSyntax>())        return lower_binary(n);
    if (auto* n = ast->as<UnaryExprSyntax>())         return lower_unary(n);
    if (auto* n = ast->as<AssignmentExprSyntax>())    return lower_assignment(n);
    if (auto* n = ast->as<CallExprSyntax>())          return lower_call(n);
    if (auto* n = ast->as<MemberAccessExprSyntax>())  return lower_member_access(n);
    if (auto* n = ast->as<InitializerExprSyntax>())   return lower_initializer_expr(n);
    if (auto* n = ast->as<ParenExprSyntax>())         return lower_expr(n->expression);
    return nullptr;
}

FhirExpr* FhirLowerer::lower_literal(LiteralExprSyntax* ast)
{
    auto* node = make<FhirLiteralExpr>();
    node->span = ast->span;
    node->type = context.bindings.get_type(ast);
    if (ast->token.kind == TokenKind::LiteralI32)
        node->value = LiteralValue::make_int(std::stoi(std::string(ast->token.lexeme)));
    else if (ast->token.kind == TokenKind::LiteralF32)
        node->value = LiteralValue::make_float(std::stof(std::string(ast->token.lexeme)));
    else if (ast->token.kind == TokenKind::LiteralBool)
        node->value = LiteralValue::make_bool(ast->token.lexeme == "true");
    return node;
}

FhirExpr* FhirLowerer::lower_identifier(IdentifierExprSyntax* ast)
{
    Symbol* sym = context.bindings.get_symbol(ast);
    if (!sym) return nullptr;

    switch (sym->kind)
    {
        case SymbolKind::Local:
        {
            auto* node = make<FhirLocalRefExpr>();
            node->span = ast->span;
            node->type = context.bindings.get_type(ast);
            node->local = sym->as<LocalSymbol>();
            return node;
        }
        case SymbolKind::Parameter:
        {
            auto* node = make<FhirParamRefExpr>();
            node->span = ast->span;
            node->type = context.bindings.get_type(ast);
            node->parameter = sym->as<ParameterSymbol>();
            return node;
        }
        case SymbolKind::Field:
        {
            auto* thisNode = make<FhirThisExpr>();
            thisNode->span = ast->span;
            thisNode->type = sym->parent ? sym->parent->as<TypeSymbol>() : nullptr;

            auto* node = make<FhirFieldAccessExpr>();
            node->span = ast->span;
            node->type = context.bindings.get_type(ast);
            node->object = thisNode;
            node->field = sym->as<FieldSymbol>();
            return node;
        }
        default:
            return nullptr;
    }
}

FhirExpr* FhirLowerer::lower_this(ThisExprSyntax* ast)
{
    auto* node = make<FhirThisExpr>();
    node->span = ast->span;
    node->type = context.bindings.get_type(ast);
    return node;
}

FhirExpr* FhirLowerer::lower_binary(BinaryExprSyntax* ast)
{
    FhirExpr* lhs = lower_expr(ast->left);
    FhirExpr* rhs = lower_expr(ast->right);
    TypeSymbol* leftType = context.bindings.get_type(ast->left);

    auto* namedType = leftType ? leftType->as<NamedTypeSymbol>() : nullptr;
    if (namedType && !is_builtin_type(namedType))
    {
        TypeSymbol* rightType = context.bindings.get_type(ast->right);
        auto* method = namedType->find_binary_operator(binary_op_to_token(ast->op), leftType, rightType);
        if (method)
        {
            auto* node = make<FhirMethodCallExpr>();
            node->span = ast->span;
            node->type = context.bindings.get_type(ast);
            node->receiver = lhs;
            node->method = method;
            node->arguments.push_back(rhs);
            return node;
        }
    }

    auto* node = make<FhirIntrinsicExpr>();
    node->span = ast->span;
    node->type = context.bindings.get_type(ast);
    node->op = to_intrinsic_op(ast->op);
    node->args.push_back(lhs);
    node->args.push_back(rhs);
    return node;
}

FhirExpr* FhirLowerer::lower_unary(UnaryExprSyntax* ast)
{
    FhirExpr* operand = lower_expr(ast->operand);
    TypeSymbol* operandType = context.bindings.get_type(ast->operand);

    auto* namedType = operandType ? operandType->as<NamedTypeSymbol>() : nullptr;
    if (namedType && !is_builtin_type(namedType))
    {
        auto* method = namedType->find_unary_operator(unary_op_to_token(ast->op), operandType);
        if (method)
        {
            auto* node = make<FhirMethodCallExpr>();
            node->span = ast->span;
            node->type = context.bindings.get_type(ast);
            node->receiver = operand;
            node->method = method;
            return node;
        }
    }

    auto* node = make<FhirIntrinsicExpr>();
    node->span = ast->span;
    node->type = context.bindings.get_type(ast);
    node->op = to_intrinsic_op(ast->op);
    node->args.push_back(operand);
    return node;
}

FhirExpr* FhirLowerer::lower_assignment(AssignmentExprSyntax* ast)
{
    FhirExpr* value = lower_expr(ast->value);

    if (ast->op != AssignOp::Simple)
    {
        FhirExpr* readTarget = lower_expr(ast->target);

        auto* binOp = make<FhirIntrinsicExpr>();
        binOp->span = ast->span;
        binOp->type = context.bindings.get_type(ast);
        switch (ast->op)
        {
            case AssignOp::Add: binOp->op = IntrinsicOp::Add; break;
            case AssignOp::Sub: binOp->op = IntrinsicOp::Sub; break;
            case AssignOp::Mul: binOp->op = IntrinsicOp::Mul; break;
            case AssignOp::Div: binOp->op = IntrinsicOp::Div; break;
            default: break;
        }
        binOp->args.push_back(readTarget);
        binOp->args.push_back(value);
        value = binOp;
    }

    FhirExpr* writeTarget = lower_expr(ast->target);

    auto* node = make<FhirAssignExpr>();
    node->span = ast->span;
    node->type = context.bindings.get_type(ast);
    node->target = writeTarget;
    node->value = value;
    return node;
}

FhirExpr* FhirLowerer::lower_initializer_target(InitializerExprSyntax* ast)
{
    if (!ast->target) return nullptr;

    if (auto* call = ast->target->as<CallExprSyntax>())
        return lower_call(call);

    if (auto* id = ast->target->as<IdentifierExprSyntax>())
    {
        Symbol* sym = context.bindings.get_symbol(id);
        auto* ctor = sym ? sym->as<MethodSymbol>() : nullptr;
        if (ctor && ctor->isConstructor)
        {
            auto* node = make<FhirObjectCreateExpr>();
            node->span = id->span;
            node->type = context.bindings.get_type(id);
            node->constructor = ctor;
            return node;
        }
    }

    return lower_expr(ast->target);
}

FhirExpr* FhirLowerer::lower_initializer_expr(InitializerExprSyntax* ast)
{
    if (!ast->target) return nullptr;

    if (ast->initializers.empty())
        return lower_initializer_target(ast);

    TypeSymbol* type = context.bindings.get_type(ast);

    auto* tempLocal = arena.alloc<LocalSymbol>();
    tempLocal->name = "__init_" + std::to_string(tempCounter++);
    tempLocal->type = type;

    auto* varDecl = make<FhirVarDeclStmt>();
    varDecl->span = ast->span;
    varDecl->local = tempLocal;
    varDecl->initializer = lower_initializer_target(ast);

    assert(pendingStmts);
    pendingStmts->push_back(varDecl);

    auto* localRef = make<FhirLocalRefExpr>();
    localRef->span = ast->span;
    localRef->type = type;
    localRef->local = tempLocal;
    emit_field_inits(ast, localRef, *pendingStmts);

    auto* resultRef = make<FhirLocalRefExpr>();
    resultRef->span = ast->span;
    resultRef->type = type;
    resultRef->local = tempLocal;
    return resultRef;
}

FhirExpr* FhirLowerer::build_field_access_chain(FhirExpr* receiver, BaseExprSyntax* target)
{
    if (auto* id = target->as<IdentifierExprSyntax>())
    {
        Symbol* sym = context.bindings.get_symbol(id);
        auto* field = sym ? sym->as<FieldSymbol>() : nullptr;

        auto* node = make<FhirFieldAccessExpr>();
        node->span = id->span;
        node->type = field ? field->type : nullptr;
        node->object = receiver;
        node->field = field;
        return node;
    }

    if (auto* member = target->as<MemberAccessExprSyntax>())
    {
        FhirExpr* left = build_field_access_chain(receiver, member->left);

        Symbol* sym = context.bindings.get_symbol(member);
        auto* field = sym ? sym->as<FieldSymbol>() : nullptr;

        auto* node = make<FhirFieldAccessExpr>();
        node->span = member->span;
        node->type = field ? field->type : nullptr;
        node->object = left;
        node->field = field;
        return node;
    }

    return nullptr;
}

void FhirLowerer::emit_field_inits(InitializerExprSyntax* ast, FhirExpr* receiver, std::vector<FhirStmt*>& out)
{
    for (auto* fieldInit : ast->initializers)
    {
        if (!fieldInit->target) continue;

        FhirExpr* fieldTarget = build_field_access_chain(receiver, fieldInit->target);
        if (!fieldTarget) continue;

        auto* anonInit = fieldInit->value ? fieldInit->value->as<InitializerExprSyntax>() : nullptr;
        if (anonInit && !anonInit->target)
        {
            emit_field_inits(anonInit, fieldTarget, out);
        }
        else
        {
            FhirExpr* value = lower_expr(fieldInit->value);

            auto* assign = make<FhirAssignExpr>();
            assign->span = fieldInit->span;
            assign->type = value ? value->type : nullptr;
            assign->target = fieldTarget;
            assign->value = value;

            auto* stmt = make<FhirExprStmt>();
            stmt->span = fieldInit->span;
            stmt->expression = assign;
            out.push_back(stmt);
        }
    }
}

FhirExpr* FhirLowerer::lower_call(CallExprSyntax* ast)
{
    Symbol* calleeSym = context.bindings.get_symbol(ast->callee);
    auto* method = calleeSym ? calleeSym->as<MethodSymbol>() : nullptr;
    if (!method) return nullptr;

    TypeSymbol* resultType = context.bindings.get_type(ast);

    if (method->isConstructor)
    {
        auto* node = make<FhirObjectCreateExpr>();
        node->span = ast->span;
        node->type = resultType;
        node->constructor = method;
        for (auto* arg : ast->arguments)
            node->arguments.push_back(lower_expr(arg));

        return node;
    }

    if (auto* memberAccess = ast->callee->as<MemberAccessExprSyntax>())
    {
        TypeSymbol* leftType = context.bindings.get_type(memberAccess->left);
        if (leftType && !has_modifier(method->modifiers, Modifier::Static))
        {
            auto* node = make<FhirMethodCallExpr>();
            node->span = ast->span;
            node->type = resultType;
            node->receiver = lower_expr(memberAccess->left);
            node->method = method;
            for (auto* arg : ast->arguments)
                node->arguments.push_back(lower_expr(arg));
            return node;
        }
    }

    auto* node = make<FhirCallExpr>();
    node->span = ast->span;
    node->type = resultType;
    node->target = method;
    for (auto* arg : ast->arguments)
        node->arguments.push_back(lower_expr(arg));
    return node;
}

FhirExpr* FhirLowerer::lower_member_access(MemberAccessExprSyntax* ast)
{
    Symbol* sym = context.bindings.get_symbol(ast);
    if (!sym || !sym->is<FieldSymbol>()) return nullptr;

    auto* node = make<FhirFieldAccessExpr>();
    node->span = ast->span;
    node->type = context.bindings.get_type(ast);
    node->object = lower_expr(ast->left);
    node->field = sym->as<FieldSymbol>();
    return node;
}

FhirBlock* FhirLowerer::lower_block(BlockSyntax* ast)
{
    auto* node = make<FhirBlock>();
    node->span = ast->span;

    auto* prevPending = pendingStmts;
    pendingStmts = &node->statements;

    for (auto* stmt : ast->statements)
    {
        lower_stmt(stmt, node->statements);
    }

    pendingStmts = prevPending;
    return node;
}

#pragma region Statements

void FhirLowerer::lower_stmt(BaseStmtSyntax* ast, std::vector<FhirStmt*>& out)
{
    if (!ast) return;
    if (auto* n = ast->as<VariableDeclSyntax>())      { lower_var_decl(n, out); return; }
    if (auto* n = ast->as<ExpressionStmtSyntax>())    { out.push_back(lower_expr_stmt(n)); return; }
    if (auto* n = ast->as<ReturnStmtSyntax>())        { out.push_back(lower_return(n)); return; }
    if (auto* n = ast->as<IfStmtSyntax>())            { out.push_back(lower_if(n)); return; }
    if (auto* n = ast->as<WhileStmtSyntax>())         { out.push_back(lower_while(n)); return; }
}

void FhirLowerer::lower_var_decl(VariableDeclSyntax* ast, std::vector<FhirStmt*>& out)
{
    auto* node = make<FhirVarDeclStmt>();
    node->span = ast->span;
    Symbol* sym = context.bindings.get_decl(ast);
    node->local = sym ? sym->as<LocalSymbol>() : nullptr;

    auto* initExpr = ast->initializer ? ast->initializer->as<InitializerExprSyntax>() : nullptr;
    if (initExpr && !initExpr->initializers.empty())
    {
        node->initializer = lower_initializer_target(initExpr);
        out.push_back(node);

        if (node->local)
        {
            auto* localRef = make<FhirLocalRefExpr>();
            localRef->span = ast->span;
            localRef->type = node->local->type;
            localRef->local = node->local;
            emit_field_inits(initExpr, localRef, out);
        }
    }
    else
    {
        node->initializer = lower_expr(ast->initializer);
        out.push_back(node);
    }
}

FhirStmt* FhirLowerer::lower_expr_stmt(ExpressionStmtSyntax* ast)
{
    auto* node = make<FhirExprStmt>();
    node->span = ast->span;
    node->expression = lower_expr(ast->expression);
    return node;
}

FhirStmt* FhirLowerer::lower_return(ReturnStmtSyntax* ast)
{
    auto* node = make<FhirReturnStmt>();
    node->span = ast->span;
    node->value = lower_expr(ast->value);
    return node;
}

FhirStmt* FhirLowerer::lower_if(IfStmtSyntax* ast)
{
    auto* node = make<FhirIfStmt>();
    node->span = ast->span;
    node->condition = lower_expr(ast->condition);
    node->thenBlock = ast->thenBody ? lower_block(ast->thenBody) : nullptr;
    node->elseIf = ast->elseIf ? lower_if(ast->elseIf)->as<FhirIfStmt>() : nullptr;
    node->elseBlock = ast->elseBlock ? lower_block(ast->elseBlock) : nullptr;
    return node;
}

FhirStmt* FhirLowerer::lower_while(WhileStmtSyntax* ast)
{
    auto* node = make<FhirWhileStmt>();
    node->span = ast->span;
    node->condition = lower_expr(ast->condition);
    node->body = ast->body ? lower_block(ast->body) : nullptr;
    return node;
}

}
