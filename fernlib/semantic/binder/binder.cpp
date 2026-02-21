#include "binder.hpp"

#include <semantic/context.hpp>
#include <ast/ast.hpp>

namespace Fern
{

Binder::Binder(SemanticContext& context)
    : DiagnosticSystem("Binder")
    , context(context)
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
    typePtr->modifiers = typeDecl->modifiers;

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
            fieldPtr->modifiers = fieldAst->modifiers;
            fieldPtr->index = fieldIndex++;

            type->fields.push_back(context.symbols.own(std::move(fieldPtr)));
        }
        else if (auto* methodAst = member->as<FunctionDeclSyntax>())
        {
            auto methodPtr = std::make_unique<MethodSymbol>();
            methodPtr->name = std::string(methodAst->name.lexeme);
            methodPtr->syntax = methodAst;
            methodPtr->parent = type;
            methodPtr->modifiers = methodAst->modifiers;

            auto* method = context.symbols.own(std::move(methodPtr));
            create_parameters(method, methodAst->parameters);

            type->methods.push_back(method);
            allMethods.push_back(method);
        }
        else if (auto* opAst = member->as<OperatorDeclSyntax>())
        {
            auto methodPtr = std::make_unique<MethodSymbol>();
            methodPtr->name = std::string(opAst->op.lexeme);
            methodPtr->syntax = opAst;
            methodPtr->parent = type;
            methodPtr->operatorKind = opAst->op.kind;
            methodPtr->modifiers = Modifier::Public | Modifier::Static;

            auto* method = context.symbols.own(std::move(methodPtr));
            create_parameters(method, opAst->parameters);

            if (method->parameters.size() < 1 || method->parameters.size() > 2)
            {
                error("operator '" + std::string(Fern::format(opAst->op.kind)) +
                      "' must have 1 parameter (unary) or 2 parameters (binary), but has " +
                      std::to_string(method->parameters.size()), opAst->span);
            }

            type->methods.push_back(method);
            allMethods.push_back(method);
        }
        else if (auto* initAst = member->as<InitDeclSyntax>())
        {
            auto methodPtr = std::make_unique<MethodSymbol>();
            methodPtr->name = "init";
            methodPtr->syntax = initAst;
            methodPtr->parent = type;
            methodPtr->isConstructor = true;
            methodPtr->modifiers = initAst->modifiers;

            auto* method = context.symbols.own(std::move(methodPtr));
            create_parameters(method, initAst->parameters);

            type->methods.push_back(method);
            allMethods.push_back(method);
        }
        else if (auto* nestedTypeDecl = member->as<TypeDeclSyntax>())
        {
            create_type_symbol(nestedTypeDecl, type);
        }
    }

    bool hasInit = false;
    for (auto* member : typeDecl->declarations)
    {
        if (member->is<InitDeclSyntax>())
        {
            hasInit = true;
            break;
        }
    }

    // auto generated constructor
    if (!hasInit)
    {
        auto methodPtr = std::make_unique<MethodSymbol>();
        methodPtr->name = "init";
        methodPtr->parent = type;
        methodPtr->isConstructor = true;
        methodPtr->modifiers = Modifier::Public;

        auto* method = context.symbols.own(std::move(methodPtr));

        int paramIndex = 0;
        for (auto* field : type->fields)
        {
            auto paramPtr = std::make_unique<ParameterSymbol>();
            paramPtr->name = field->name;
            paramPtr->parent = method;
            paramPtr->index = paramIndex++;

            method->parameters.push_back(context.symbols.own(std::move(paramPtr)));
        }

        type->methods.push_back(method);
        allMethods.push_back(method);
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
            std::vector<ParameterDeclSyntax*>* paramAsts = nullptr;

            if (auto* funcAst = method->syntax ? method->syntax->as<FunctionDeclSyntax>() : nullptr)
            {
                if (funcAst->returnType)
                {
                    method->returnType = resolve_type_expr(funcAst->returnType);
                }
                paramAsts = &funcAst->parameters;
            }
            else if (auto* opAst = method->syntax ? method->syntax->as<OperatorDeclSyntax>() : nullptr)
            {
                if (opAst->returnType)
                {
                    method->returnType = resolve_type_expr(opAst->returnType);
                }
                paramAsts = &opAst->parameters;
            }
            else if (auto* initAst = method->syntax ? method->syntax->as<InitDeclSyntax>() : nullptr)
            {
                method->returnType = type;
                paramAsts = &initAst->parameters;
            }
            else if (method->isConstructor)
            {
                method->returnType = type;
                for (size_t i = 0; i < method->parameters.size() && i < type->fields.size(); ++i)
                {
                    method->parameters[i]->type = type->fields[i]->type;
                }
            }

            if (paramAsts)
            {
                for (size_t i = 0; i < method->parameters.size() && i < paramAsts->size(); ++i)
                {
                    if ((*paramAsts)[i]->type)
                    {
                        method->parameters[i]->type = resolve_type_expr((*paramAsts)[i]->type);
                    }
                }
            }

            if (method->is_operator())
            {
                bool hasContainingType = false;
                for (auto* param : method->parameters)
                {
                    if (param->type == type)
                    {
                        hasContainingType = true;
                        break;
                    }
                }
                if (!hasContainingType)
                {
                    Span loc = method->syntax ? method->syntax->span : Span{};
                    error("operator '" + method->name +
                          "' must have at least one parameter of containing type '" +
                          type->name + "'", loc);
                }
            }
        }

        for (size_t i = 0; i < type->methods.size(); ++i)
        {
            auto* a = type->methods[i];
            for (size_t j = i + 1; j < type->methods.size(); ++j)
            {
                auto* b = type->methods[j];
                if (a->name != b->name || a->parameters.size() != b->parameters.size())
                {
                    continue;
                }

                bool sameSignature = true;
                for (size_t p = 0; p < a->parameters.size(); ++p)
                {
                    if (a->parameters[p]->type != b->parameters[p]->type)
                    {
                        sameSignature = false;
                        break;
                    }
                }

                if (sameSignature)
                {
                    Span loc = b->syntax ? b->syntax->span : Span{};
                    if (b->isConstructor)
                    {
                        error("duplicate constructor on type '" + type->name + "'", loc);
                    }
                    else if (b->is_operator())
                    {
                        error("duplicate operator '" + b->name + "' on type '" + type->name + "'", loc);
                    }
                    else
                    {
                        error("duplicate method '" + b->name + "' on type '" + type->name + "'", loc);
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
        bind_method(method);
    }
}

void Binder::bind_method(MethodSymbol* method)
{
    if (!method || boundMethods.contains(method) || bindingMethods.contains(method))
    {
        return;
    }

    auto* prevMethod = currentMethod;
    auto* prevType = currentType;
    auto* prevNamespace = currentNamespace;
    auto savedScopes = std::move(scopes);

    currentMethod = method;
    currentType = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
    currentNamespace = find_enclosing_namespace(currentType);
    scopes.clear();

    bindingMethods.insert(method);

    BlockExprSyntax* body = nullptr;

    if (auto* funcAst = method->syntax ? method->syntax->as<FunctionDeclSyntax>() : nullptr)
    {
        body = funcAst->body;
    }
    else if (auto* opAst = method->syntax ? method->syntax->as<OperatorDeclSyntax>() : nullptr)
    {
        body = opAst->body;
    }
    else if (auto* initAst = method->syntax ? method->syntax->as<InitDeclSyntax>() : nullptr)
    {
        body = initAst->body;
    }

    if (body)
    {
        push_scope();

        for (auto* param : method->parameters)
        {
            current_scope().add(param->name, param);
        }

        bind_block(body);

        pop_scope();
    }

    bindingMethods.erase(method);
    boundMethods.insert(method);

    scopes = std::move(savedScopes);
    currentMethod = prevMethod;
    currentType = prevType;
    currentNamespace = prevNamespace;
}

TypeSymbol* Binder::get_return_type(MethodSymbol* method)
{
    if (!method)
    {
        return nullptr;
    }

    if (method->returnType)
    {
        return method->returnType;
    }

    if (boundMethods.contains(method))
    {
        return nullptr;
    }

    if (bindingMethods.contains(method))
    {
        return nullptr;
    }

    bind_method(method);
    return method->returnType;
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
    if (auto* thisExpr = expr->as<ThisExprSyntax>())
    {
        return bind_this(thisExpr);
    }
    if (auto* unary = expr->as<UnaryExprSyntax>())
    {
        return bind_unary(unary);
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
    if (auto* initializer = expr->as<InitializerExprSyntax>())
    {
        return bind_initializer(initializer);
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
    auto typeKeyword = literal_to_type_keyword(expr->token.kind);
    TypeSymbol* type = typeKeyword ? context.resolve_type_name(*typeKeyword) : nullptr;
    store_type(expr, type);
    return type;
}

TypeSymbol* Binder::bind_identifier(IdentifierExprSyntax* expr)
{
    Symbol* symbol = resolve_name(expr->name.lexeme);
    if (!symbol)
    {
        error("undefined name '" + std::string(expr->name.lexeme) + "'", expr->span);
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

TypeSymbol* Binder::bind_this(ThisExprSyntax* expr)
{
    if (!currentType)
    {
        error("'this' can only be used inside a type", expr->span);
        store_type(expr, nullptr);
        return nullptr;
    }

    store_symbol(expr, currentType);
    store_type(expr, currentType);
    return currentType;
}

TypeSymbol* Binder::bind_unary(UnaryExprSyntax* expr)
{
    TypeSymbol* operandType = bind_expr(expr->operand);

    auto* namedType = operandType ? operandType->as<NamedTypeSymbol>() : nullptr;
    if (namedType)
    {
        TokenKind opToken = unary_op_to_token(expr->op);
        for (auto* method : namedType->methods)
        {
            if (method->operatorKind == opToken &&
                method->parameters.size() == 1 &&
                method->parameters[0]->type == namedType)
            {
                store_type(expr, method->returnType);
                return method->returnType;
            }
        }

        error("no unary operator '" + std::string(Fern::format(opToken)) +
              "' defined for type '" + namedType->name + "'", expr->span);
    }

    store_type(expr, operandType);
    return operandType;
}

TypeSymbol* Binder::bind_binary(BinaryExprSyntax* expr)
{
    TypeSymbol* leftType = bind_expr(expr->left);
    TypeSymbol* rightType = bind_expr(expr->right);

    TokenKind opToken = binary_op_to_token(expr->op);

    // Derived operators: >= is > + ==, <= is < + ==
    bool isDerived = (opToken == TokenKind::GreaterEqual || opToken == TokenKind::LessEqual);
    TokenKind baseToken = TokenKind::Invalid;
    if (opToken == TokenKind::GreaterEqual)
    {
        baseToken = TokenKind::Greater;
    }
    else if (opToken == TokenKind::LessEqual)
    {
        baseToken = TokenKind::Less;
    }

    auto* namedType = leftType ? leftType->as<NamedTypeSymbol>() : nullptr;
    if (namedType)
    {
        for (auto* method : namedType->methods)
        {
            if (!method->is_operator())
            {
                continue;
            }

            if (method->operatorKind != opToken &&
                !(isDerived && (method->operatorKind == baseToken || method->operatorKind == TokenKind::Equal)))
            {
                continue;
            }

            if (method->parameters.size() != 2)
            {
                continue;
            }

            bool leftMatch = method->parameters[0]->type == leftType || !leftType;
            bool rightMatch = method->parameters[1]->type == rightType || !rightType;
            if (leftMatch && rightMatch)
            {
                if (method->operatorKind == opToken)
                {
                    store_type(expr, method->returnType);
                    return method->returnType;
                }
                if (isDerived)
                {
                    TypeSymbol* boolType = context.resolve_type_name(TokenKind::BoolKeyword);
                    store_type(expr, boolType);
                    return boolType;
                }
            }
        }

        if (isDerived)
        {
            error("derived operator '" + std::string(Fern::format(opToken)) +
                  "' requires both '" + std::string(Fern::format(baseToken)) +
                  "' and '==' operators on type '" + namedType->name + "'", expr->span);
        }
        else
        {
            error("no operator '" + std::string(Fern::format(opToken)) +
                  "' defined for type '" + namedType->name + "'", expr->span);
        }
    }

    store_type(expr, leftType);
    return leftType;
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

    std::vector<TypeSymbol*> argTypes;
    for (auto* arg : expr->arguments)
    {
        argTypes.push_back(bind_expr(arg));
    }

    MethodSymbol* method = nullptr;
    NamedTypeSymbol* targetType = nullptr;
    std::string_view methodName;

    if (auto* idExpr = expr->callee->as<IdentifierExprSyntax>())
    {
        Symbol* sym = context.bindings.get_symbol(idExpr);
        if (sym && sym->kind == SymbolKind::Method)
        {
            targetType = sym->parent ? sym->parent->as<NamedTypeSymbol>() : nullptr;
            methodName = idExpr->name.lexeme;
        }
        else if (sym && sym->kind == SymbolKind::Type)
        {
            auto* namedType = sym->as<NamedTypeSymbol>();
            if (namedType)
            {
                MethodSymbol* ctor = resolve_constructor(namedType, argTypes);
                if (ctor)
                {
                    store_symbol(expr->callee, ctor);
                }
                else
                {
                    error("no matching constructor for type '" + namedType->name + "'", expr->span);
                }
                store_type(expr, namedType);
                return namedType;
            }
        }
        else if (sym)
        {
            error("attempting to call non-method '" + std::string(idExpr->name.lexeme) + "'", idExpr->span);
        }
    }
    else if (auto* memberExpr = expr->callee->as<MemberAccessExprSyntax>())
    {
        Symbol* sym = context.bindings.get_symbol(memberExpr);
        if (sym && sym->kind == SymbolKind::Method)
        {
            targetType = sym->parent ? sym->parent->as<NamedTypeSymbol>() : nullptr;
            methodName = memberExpr->right.lexeme;
        }
        else if (sym && sym->kind == SymbolKind::Type)
        {
            auto* namedType = sym->as<NamedTypeSymbol>();
            if (namedType)
            {
                MethodSymbol* ctor = resolve_constructor(namedType, argTypes);
                if (ctor)
                {
                    store_symbol(expr->callee, ctor);
                }
                else
                {
                    error("no matching constructor for type '" + namedType->name + "'", expr->span);
                }
                store_type(expr, namedType);
                return namedType;
            }
        }
        else if (sym)
        {
            error("attempting to call non-method '" + std::string(memberExpr->right.lexeme) + "'", memberExpr->span);
        }
    }

    if (targetType && !methodName.empty())
    {
        method = resolve_method(targetType, methodName, argTypes);
        if (method)
        {
            store_symbol(expr->callee, method);
        }
        else
        {
            error("no matching overload for '" + std::string(methodName) +
                  "' on type '" + targetType->name + "'", expr->span);
        }
    }

    TypeSymbol* returnType = get_return_type(method);

    store_type(expr, returnType);
    return returnType;
}

TypeSymbol* Binder::bind_member_access(MemberAccessExprSyntax* expr)
{
    TypeSymbol* leftType = bind_expr(expr->left);

    std::string_view memberName = expr->right.lexeme;
    if (memberName.empty())
    {
        store_type(expr, nullptr);
        return nullptr;
    }

    // Namespace member access
    if (!leftType)
    {
        Symbol* leftSym = context.bindings.get_symbol(expr->left);
        auto* ns = leftSym ? leftSym->as<NamespaceSymbol>() : nullptr;
        if (ns)
        {
            Symbol* member = ns->find_member(memberName);
            if (member)
            {
                store_symbol(expr, member);
                auto* memberType = member->as<TypeSymbol>();
                store_type(expr, memberType);
                return memberType;
            }

            error("namespace '" + ns->name + "' has no member '" +
                  std::string(memberName) + "'", expr->span);
        }

        store_type(expr, nullptr);
        return nullptr;
    }

    auto* namedType = leftType->as<NamedTypeSymbol>();
    if (!namedType)
    {
        store_type(expr, nullptr);
        return nullptr;
    }

    if (auto* field = namedType->find_field(memberName))
    {
        store_symbol(expr, field);
        store_type(expr, field->type);
        return field->type;
    }

    if (auto* method = namedType->find_method(memberName))
    {
        store_symbol(expr, method);
        store_type(expr, method->returnType);
        return method->returnType;
    }

    error("type '" + namedType->name + "' has no member '" +
          std::string(memberName) + "'", expr->span);
    store_type(expr, nullptr);
    return nullptr;
}

TypeSymbol* Binder::bind_initializer(InitializerExprSyntax* expr)
{
    TypeSymbol* targetType = bind_expr(expr->target);
    NamedTypeSymbol* namedType = targetType ? targetType->as<NamedTypeSymbol>() : nullptr;

    if (!namedType)
    {
        if (auto* idExpr = expr->target->as<IdentifierExprSyntax>())
        {
            Symbol* sym = context.bindings.get_symbol(idExpr);
            if (sym && sym->kind == SymbolKind::Type)
            {
                namedType = sym->as<NamedTypeSymbol>();
            }
        }
    }

    if (!namedType)
    {
        for (auto* fieldInit : expr->initializers)
        {
            bind_expr(fieldInit->value);
        }

        store_type(expr, nullptr);
        return nullptr;
    }

    if (!expr->target->is<CallExprSyntax>())
    {
        std::vector<TypeSymbol*> emptyArgs;
        MethodSymbol* ctor = resolve_constructor(namedType, emptyArgs);
        if (!ctor)
        {
            error("type '" + namedType->name +
                  "' has no parameterless constructor for initializer list", expr->span);
        }
    }

    for (auto* fieldInit : expr->initializers)
    {
        bind_expr(fieldInit->value);

        FieldSymbol* field = namedType->find_field(fieldInit->name.lexeme);
        if (!field)
        {
            error("type '" + namedType->name + "' has no field named '" +
                  std::string(fieldInit->name.lexeme) + "'", fieldInit->span);
            continue;
        }

        context.bindings.set_decl(fieldInit, field);
    }

    store_type(expr, namedType);
    return namedType;
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

    TypeSymbol* lastType = nullptr;
    for (auto* stmt : expr->statements)
    {
        bind_stmt(stmt);

        if (auto* exprStmt = stmt->as<ExpressionStmtSyntax>())
        {
            lastType = context.bindings.get_type(exprStmt->expression);
        }
        else
        {
            lastType = nullptr;
        }
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
    else if (auto* ifStmt = stmt->as<IfStmtSyntax>())
    {
        bind_if(ifStmt);
    }
    else if (auto* whileStmt = stmt->as<WhileStmtSyntax>())
    {
        bind_while(whileStmt);
    }
    else if (auto* exprStmt = stmt->as<ExpressionStmtSyntax>())
    {
        bind_expr(exprStmt->expression);
    }
}

void Binder::bind_if(IfStmtSyntax* stmt)
{
    TypeSymbol* condType = bind_expr(stmt->condition);

    TypeSymbol* boolType = context.resolve_type_name(TokenKind::BoolKeyword);
    if (condType && boolType && condType != boolType)
    {
        error("if condition must be of type 'bool', got '" +
              condType->name + "'", stmt->condition->span);
    }

    if (stmt->thenBody)
    {
        bind_block(stmt->thenBody);
    }

    if (stmt->elseIf)
    {
        bind_if(stmt->elseIf);
    }

    if (stmt->elseBlock)
    {
        bind_block(stmt->elseBlock);
    }
}

void Binder::bind_while(WhileStmtSyntax* stmt)
{
    TypeSymbol* condType = bind_expr(stmt->condition);

    TypeSymbol* boolType = context.resolve_type_name(TokenKind::BoolKeyword);
    if (condType && boolType && condType != boolType)
    {
        error("while condition must be of type 'bool', got '" +
              condType->name + "'", stmt->condition->span);
    }

    if (stmt->body)
    {
        bind_block(stmt->body);
    }
}

void Binder::bind_return(ReturnStmtSyntax* stmt)
{
    if (stmt->value)
    {
        TypeSymbol* type = bind_expr(stmt->value);
        if (currentMethod && !currentMethod->returnType && type)
        {
            currentMethod->returnType = type;
        }
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
    context.bindings.set_decl(decl, local);
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

        error("undefined type '" + std::string(typeExpr->name.lexeme) + "'", expr->span);
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

        std::string fullPath;
        for (size_t i = 0; i < path.size(); ++i)
        {
            if (i > 0) fullPath += ".";
            fullPath += path[i];
        }
        error("undefined type '" + fullPath + "'", expr->span);
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
        if (auto* field = currentType->find_field(name))
        {
            return field;
        }
        if (auto* method = currentType->find_method(name))
        {
            return method;
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

MethodSymbol* Binder::resolve_method(NamedTypeSymbol* type, std::string_view name, const std::vector<TypeSymbol*>& argTypes)
{
    for (auto* method : type->methods)
    {
        if (method->name != name)
        {
            continue;
        }
        if (method->parameters.size() != argTypes.size())
        {
            continue;
        }

        bool match = true;
        for (size_t i = 0; i < argTypes.size(); ++i)
        {
            if (argTypes[i] && method->parameters[i]->type && argTypes[i] != method->parameters[i]->type)
            {
                match = false;
                break;
            }
        }

        if (match)
        {
            return method;
        }
    }
    return nullptr;
}

MethodSymbol* Binder::resolve_constructor(NamedTypeSymbol* type, const std::vector<TypeSymbol*>& argTypes)
{
    for (auto* method : type->methods)
    {
        if (!method->isConstructor)
        {
            continue;
        }
        if (method->parameters.size() != argTypes.size())
        {
            continue;
        }

        bool match = true;
        for (size_t i = 0; i < argTypes.size(); ++i)
        {
            if (argTypes[i] && method->parameters[i]->type && argTypes[i] != method->parameters[i]->type)
            {
                match = false;
                break;
            }
        }

        if (match)
        {
            return method;
        }
    }
    return nullptr;
}

void Binder::create_parameters(MethodSymbol* method, const std::vector<ParameterDeclSyntax*>& params)
{
    int paramIndex = 0;
    for (auto* paramAst : params)
    {
        auto paramPtr = std::make_unique<ParameterSymbol>();
        paramPtr->name = std::string(paramAst->name.lexeme);
        paramPtr->syntax = paramAst;
        paramPtr->parent = method;
        paramPtr->index = paramIndex++;

        method->parameters.push_back(context.symbols.own(std::move(paramPtr)));
    }
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
