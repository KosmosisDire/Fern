#include "binder.hpp"

#include <semantic/context.hpp>
#include <ast/ast.hpp>

namespace Fern
{

Binder::Binder(SemanticContext& context)
    : context(context)
{
}

static bool extract_type_path(BaseExprSyntax* expr, std::vector<std::string_view>& path)
{
    if (auto* typeExpr = expr->as<TypeExprSyntax>())
    {
        path.push_back(typeExpr->name.lexeme);
        return true;
    }
    if (auto* memberExpr = expr->as<MemberAccessExprSyntax>())
    {
        if (!extract_type_path(memberExpr->left, path)) return false;
        path.push_back(memberExpr->right.lexeme);
        return true;
    }
    return false;
}

static NamespaceSymbol* find_enclosing_namespace(Symbol* sym)
{
    for (auto* s = sym; s != nullptr; s = s->parent)
    {
        if (auto* ns = s->as<NamespaceSymbol>())
        {
            return ns;
        }
    }
    return nullptr;
}

#pragma region Symbol Creation

void Binder::bind_ast(RootSyntax* ast)
{
    if (!ast)
    {
        return;
    }

    auto* globalNs = context.symbols.globalNamespace;

    for (auto* decl : ast->declarations)
    {
        if (auto* nsDecl = decl->as<NamespaceDeclSyntax>())
        {
            process_namespace(nsDecl, globalNs);
        }
        else if (auto* typeDecl = decl->as<TypeDeclSyntax>())
        {
            create_type_symbol(typeDecl, globalNs);
        }
    }
}

void Binder::process_namespace(NamespaceDeclSyntax* nsDecl, NamespaceSymbol* parentNs)
{
    if (!nsDecl || !parentNs)
    {
        return;
    }

    auto* ns = context.symbols.get_or_create_namespace(parentNs, nsDecl->name.lexeme);

    for (auto* decl : nsDecl->declarations)
    {
        if (auto* nestedNs = decl->as<NamespaceDeclSyntax>())
        {
            process_namespace(nestedNs, ns);
        }
        else if (auto* typeDecl = decl->as<TypeDeclSyntax>())
        {
            create_type_symbol(typeDecl, ns);
        }
    }
}

NamedTypeSymbol* Binder::create_type_symbol(TypeDeclSyntax* typeDecl, Symbol* parent)
{
    if (!typeDecl || !parent)
    {
        return nullptr;
    }

    auto typePtr = std::make_unique<NamedTypeSymbol>();
    typePtr->name = std::string(typeDecl->name.lexeme);
    typePtr->syntax = typeDecl;
    typePtr->parent = parent;

    auto* type = context.symbols.own(std::move(typePtr));

    if (auto* ns = parent->as<NamespaceSymbol>())
    {
        ns->add_member(type);
    }
    else if (auto* parentType = parent->as<NamedTypeSymbol>())
    {
        parentType->nestedTypes.push_back(type);
    }

    allTypes.push_back(type);

    int fieldIndex = 0;
    for (auto* member : typeDecl->declarations)
    {
        if (auto* fieldAst = member->as<FieldDeclSyntax>())
        {
            auto fieldPtr = std::make_unique<FieldSymbol>();
            fieldPtr->name = std::string(fieldAst->name.lexeme);
            fieldPtr->syntax = fieldAst;
            fieldPtr->parent = type;
            fieldPtr->index = fieldIndex++;

            type->fields.push_back(context.symbols.own(std::move(fieldPtr)));
        }
        else if (auto* methodAst = member->as<FunctionDeclSyntax>())
        {
            auto methodPtr = std::make_unique<MethodSymbol>();
            methodPtr->name = std::string(methodAst->name.lexeme);
            methodPtr->syntax = methodAst;
            methodPtr->parent = type;
            auto* method = context.symbols.own(std::move(methodPtr));

            int paramIndex = 0;
            for (auto* paramAst : methodAst->parameters)
            {
                auto paramPtr = std::make_unique<ParameterSymbol>();
                paramPtr->name = std::string(paramAst->name.lexeme);
                paramPtr->syntax = paramAst;
                paramPtr->parent = method;
                paramPtr->index = paramIndex++;

                method->parameters.push_back(context.symbols.own(std::move(paramPtr)));
            }

            type->methods.push_back(method);
            allMethods.push_back(method);
        }
        else if (auto* nestedTypeDecl = member->as<TypeDeclSyntax>())
        {
            create_type_symbol(nestedTypeDecl, type);
        }
    }

    return type;
}

void Binder::resolve_all_types()
{
    for (auto* type : allTypes)
    {
        currentType = type;
        currentNamespace = find_enclosing_namespace(type);

        for (auto* field : type->fields)
        {
            auto* fieldAst = field->syntax ? field->syntax->as<FieldDeclSyntax>() : nullptr;
            if (fieldAst && fieldAst->type)
            {
                field->type = resolve_type_expr(fieldAst->type);
            }
        }

        for (auto* method : type->methods)
        {
            auto* funcAst = method->syntax ? method->syntax->as<FunctionDeclSyntax>() : nullptr;
            if (funcAst)
            {
                if (funcAst->returnType)
                {
                    method->returnType = resolve_type_expr(funcAst->returnType);
                }

                for (size_t i = 0; i < method->parameters.size() && i < funcAst->parameters.size(); ++i)
                {
                    if (funcAst->parameters[i]->type)
                    {
                        method->parameters[i]->type = resolve_type_expr(funcAst->parameters[i]->type);
                    }
                }
            }
        }
    }

    currentType = nullptr;
    currentNamespace = nullptr;
}

void Binder::bind_all_methods()
{
    for (auto* method : allMethods)
    {
        currentMethod = method;
        currentType = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
        currentNamespace = find_enclosing_namespace(currentType);

        auto* funcAst = method->syntax ? method->syntax->as<FunctionDeclSyntax>() : nullptr;
        if (!funcAst)
        {
            continue;
        }

        push_scope();

        for (auto* param : method->parameters)
        {
            current_scope().add(param->name, param);
        }

        if (funcAst->body)
        {
            bind_block(funcAst->body);
        }

        pop_scope();

        currentMethod = nullptr;
        currentType = nullptr;
        currentNamespace = nullptr;
    }
}

#pragma region Expr Binding

TypeSymbol* Binder::bind_expr(BaseExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* lit = expr->as<LiteralExprSyntax>())
    {
        return bind_literal(lit);
    }
    if (auto* id = expr->as<IdentifierExprSyntax>())
    {
        return bind_identifier(id);
    }
    if (auto* bin = expr->as<BinaryExprSyntax>())
    {
        return bind_binary(bin);
    }
    if (auto* assign = expr->as<AssignmentExprSyntax>())
    {
        return bind_assignment(assign);
    }
    if (auto* call = expr->as<CallExprSyntax>())
    {
        return bind_call(call);
    }
    if (auto* member = expr->as<MemberAccessExprSyntax>())
    {
        return bind_member_access(member);
    }
    if (auto* paren = expr->as<ParenExprSyntax>())
    {
        return bind_paren(paren);
    }
    if (auto* block = expr->as<BlockExprSyntax>())
    {
        return bind_block(block);
    }

    return nullptr;
}

TypeSymbol* Binder::bind_literal(LiteralExprSyntax* expr)
{
    // TODO: determine TokenKind from literal token (int vs float) instead of hardcoding F32
    TypeSymbol* type = context.resolve_type_name(TokenKind::F32Keyword);
    store_type(expr, type);
    return type;
}

TypeSymbol* Binder::bind_identifier(IdentifierExprSyntax* expr)
{
    Symbol* symbol = resolve_name(expr->name.lexeme);
    if (!symbol)
    {
        return nullptr;
    }

    store_symbol(expr, symbol);

    TypeSymbol* type = nullptr;
    switch (symbol->kind)
    {
        case SymbolKind::Local:
        {
            type = symbol->as<LocalSymbol>()->type;
            break;
        }
        case SymbolKind::Parameter:
        {
            type = symbol->as<ParameterSymbol>()->type;
            break;
        }
        case SymbolKind::Field:
        {
            type = symbol->as<FieldSymbol>()->type;
            break;
        }
        default:
            break;
    }

    store_type(expr, type);
    return type;
}

TypeSymbol* Binder::bind_binary(BinaryExprSyntax* expr)
{
    TypeSymbol* leftType = bind_expr(expr->left);
    TypeSymbol* rightType = bind_expr(expr->right);

    // TODO: type-check left and right, determine result type properly
    TypeSymbol* resultType = leftType;

    store_type(expr, resultType);
    return resultType;
}

TypeSymbol* Binder::bind_assignment(AssignmentExprSyntax* expr)
{
    TypeSymbol* targetType = bind_expr(expr->target);
    TypeSymbol* valueType = bind_expr(expr->value);

    store_type(expr, targetType);
    return targetType;
}

TypeSymbol* Binder::bind_call(CallExprSyntax* expr)
{
    bind_expr(expr->callee);

    for (auto* arg : expr->arguments)
    {
        bind_expr(arg);
    }

    MethodSymbol* method = nullptr;

    if (auto* idExpr = expr->callee->as<IdentifierExprSyntax>())
    {
        Symbol* sym = context.bindings.get_symbol(idExpr);
        if (sym && sym->kind == SymbolKind::Method)
        {
            method = sym->as<MethodSymbol>();
        }
    }
    else if (auto* memberExpr = expr->callee->as<MemberAccessExprSyntax>())
    {
        Symbol* sym = context.bindings.get_symbol(memberExpr);
        if (sym && sym->kind == SymbolKind::Method)
        {
            method = sym->as<MethodSymbol>();
        }
    }

    TypeSymbol* returnType = method ? method->returnType : nullptr;

    store_type(expr, returnType);
    return returnType;
}

TypeSymbol* Binder::bind_member_access(MemberAccessExprSyntax* expr)
{
    TypeSymbol* leftType = bind_expr(expr->left);

    auto* namedType = leftType ? leftType->as<NamedTypeSymbol>() : nullptr;
    if (!namedType)
    {
        store_type(expr, nullptr);
        return nullptr;
    }

    std::string_view memberName = expr->right.lexeme;

    for (auto* field : namedType->fields)
    {
        if (field->name == memberName)
        {
            store_symbol(expr, field);
            store_type(expr, field->type);
            return field->type;
        }
    }

    for (auto* method : namedType->methods)
    {
        if (method->name == memberName)
        {
            store_symbol(expr, method);
            store_type(expr, method->returnType);
            return method->returnType;
        }
    }

    store_type(expr, nullptr);
    return nullptr;
}

TypeSymbol* Binder::bind_paren(ParenExprSyntax* expr)
{
    TypeSymbol* type = bind_expr(expr->expression);
    store_type(expr, type);
    return type;
}

TypeSymbol* Binder::bind_block(BlockExprSyntax* expr)
{
    push_scope();

    // TODO: track type of last expression for block-as-expression
    TypeSymbol* lastType = nullptr;
    for (auto* stmt : expr->statements)
    {
        bind_stmt(stmt);
    }

    pop_scope();

    store_type(expr, lastType);
    return lastType;
}

#pragma region Statement Binding

void Binder::bind_stmt(BaseStmtSyntax* stmt)
{
    if (!stmt) return;

    if (auto* ret = stmt->as<ReturnStmtSyntax>())
    {
        bind_return(ret);
    }
    else if (auto* varDecl = stmt->as<VariableDeclSyntax>())
    {
        bind_var_decl(varDecl);
    }
    else if (auto* exprStmt = stmt->as<ExpressionStmtSyntax>())
    {
        bind_expr(exprStmt->expression);
    }
}

void Binder::bind_return(ReturnStmtSyntax* stmt)
{
    if (stmt->value)
    {
        bind_expr(stmt->value);
    }
}

void Binder::bind_var_decl(VariableDeclSyntax* decl)
{
    TypeSymbol* type = nullptr;

    if (decl->type)
    {
        type = resolve_type_expr(decl->type);
    }

    if (decl->initializer)
    {
        TypeSymbol* initType = bind_expr(decl->initializer);
        if (!type)
        {
            type = initType;
        }
    }

    auto localPtr = std::make_unique<LocalSymbol>();
    localPtr->name = std::string(decl->name.lexeme);
    localPtr->type = type;
    localPtr->syntax = decl;
    localPtr->parent = currentMethod;

    auto* local = context.symbols.own(std::move(localPtr));
    current_scope().add(decl->name.lexeme, local);
}

#pragma region Type Resolution

TypeSymbol* Binder::resolve_type_expr(BaseExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* typeExpr = expr->as<TypeExprSyntax>())
    {
        TypeSymbol* builtin = context.resolve_type_name(typeExpr->name.kind);
        if (builtin) return builtin;

        Symbol* sym = resolve_name(typeExpr->name.lexeme);
        if (sym) return sym->as<TypeSymbol>();
        return nullptr;
    }

    if (auto* memberExpr = expr->as<MemberAccessExprSyntax>())
    {
        std::vector<std::string_view> path;
        if (extract_type_path(memberExpr, path))
        {
            auto* startNs = currentNamespace ? currentNamespace : context.symbols.globalNamespace;
            Symbol* sym = context.symbols.lookup_from(startNs, path);
            if (sym) return sym->as<TypeSymbol>();
        }
        return nullptr;
    }

    return nullptr;
}

#pragma region Helpers

void Binder::push_scope()
{
    scopes.emplace_back();
}

void Binder::pop_scope()
{
    scopes.pop_back();
}

Scope& Binder::current_scope()
{
    return scopes.back();
}

Symbol* Binder::resolve_name(std::string_view name)
{
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it)
    {
        if (Symbol* sym = it->find(name))
        {
            return sym;
        }
    }

    if (currentType)
    {
        for (auto* field : currentType->fields)
        {
            if (field->name == name)
            {
                return field;
            }
        }
        for (auto* method : currentType->methods)
        {
            if (method->name == name)
            {
                return method;
            }
        }
    }

    Symbol* start = nullptr;
    if (currentType)
    {
        start = currentType;
    }
    else if (currentNamespace)
    {
        start = currentNamespace;
    }
    else
    {
        start = context.symbols.globalNamespace;
    }
    std::string_view path[] = {name};
    return context.symbols.lookup_from(start, path);
}

void Binder::store_type(BaseExprSyntax* expr, TypeSymbol* type)
{
    context.bindings.set_type(expr, type);
}

void Binder::store_symbol(BaseExprSyntax* expr, Symbol* symbol)
{
    context.bindings.set_symbol(expr, symbol);
}

}
