#include "binder.hpp"

#include <algorithm>

#include <ast/ast.hpp>
#include <semantic/context.hpp>

namespace Fern
{

Binder::Binder(SemanticContext& context)
    : DiagnosticSystem("Binder")
    , context(context)
{
}

static std::string format_field_path(BaseExprSyntax* expr)
{
    if (!expr) return "";
    if (auto* id = expr->as<IdentifierExprSyntax>())
    {
        return std::string(id->name.lexeme);
    }
    if (auto* member = expr->as<MemberAccessExprSyntax>())
    {
        std::string left = format_field_path(member->left);
        if (left.empty()) return "";
        return left + "." + std::string(member->right.lexeme);
    }
    return "";
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

static std::string format_arg_types(const std::vector<TypeSymbol*>& argTypes)
{
    std::string result = "(";
    for (size_t i = 0; i < argTypes.size(); ++i)
    {
        if (i > 0)
        {
            result += ", ";
        }
        result += argTypes[i] ? argTypes[i]->name : "?";
    }
    result += ")";
    return result;
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
    typePtr->isAttribute = has_modifier(typeDecl->modifiers, Modifier::Attr);

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

    bool hasInit = std::any_of(typeDecl->declarations.begin(), typeDecl->declarations.end(),
        [](auto* member) { return member->template is<InitDeclSyntax>(); });

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
        currentNamespace = type->find_enclosing_namespace();

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

            if (method->syntax)
            {
                if (auto* funcAst = method->syntax->as<FunctionDeclSyntax>())
                {
                    if (funcAst->returnType)
                    {
                        method->returnType = resolve_type_expr(funcAst->returnType);
                    }
                    paramAsts = &funcAst->parameters;
                }
                else if (auto* opAst = method->syntax->as<OperatorDeclSyntax>())
                {
                    if (opAst->returnType)
                    {
                        method->returnType = resolve_type_expr(opAst->returnType);
                    }
                    paramAsts = &opAst->parameters;
                }
                else if (auto* initAst = method->syntax->as<InitDeclSyntax>())
                {
                    method->returnType = type;
                    paramAsts = &initAst->parameters;
                }
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
                bool hasContainingType = std::any_of(method->parameters.begin(), method->parameters.end(),
                    [type](auto* param) { return param->type == type; });

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

                bool sameSignature = std::equal(
                    a->parameters.begin(), a->parameters.end(),
                    b->parameters.begin(),
                    [](auto* pa, auto* pb) { return pa->type == pb->type; });

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

static BaseExprSyntax* extract_attribute_root(BaseExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* call = expr->as<CallExprSyntax>())
    {
        return extract_attribute_root(call->callee);
    }
    if (auto* init = expr->as<InitializerExprSyntax>())
    {
        return extract_attribute_root(init->target);
    }
    return expr;
}

void Binder::resolve_attributes(BaseDeclSyntax* decl, std::vector<ResolvedAttribute>& out)
{
    if (!decl) return;

    for (auto* attr : decl->attributes)
    {
        if (!attr || !attr->value) continue;

        auto* root = extract_attribute_root(attr->value);
        if (!root) continue;

        if (!root->is<IdentifierExprSyntax>() && !root->is<MemberAccessExprSyntax>())
        {
            error("attribute must be a type name", attr->span);
            continue;
        }

        bind_expr(root);
        Symbol* sym = context.bindings.get_symbol(root);
        if (!sym)
        {
            continue;
        }

        auto* attrType = sym->as<NamedTypeSymbol>();
        if (!attrType)
        {
            error("'" + sym->qualified_name() + "' is not a type", attr->span);
            continue;
        }

        if (!attrType->isAttribute)
        {
            error("type '" + attrType->name + "' is not an attribute type (missing 'attr' modifier)", attr->span);
            continue;
        }

        MethodSymbol* ctor = nullptr;

        if (auto* callExpr = attr->value->as<CallExprSyntax>())
        {
            bind_expr(attr->value);
            Symbol* callSym = context.bindings.get_symbol(callExpr->callee);
            if (callSym && callSym->kind == SymbolKind::Method)
            {
                ctor = callSym->as<MethodSymbol>();
            }
        }
        else if (auto* initExpr = attr->value->as<InitializerExprSyntax>())
        {
            if (!initExpr->target)
            {
                error("expected type name in attribute initializer", attr->span);
                continue;
            }

            bind_initializer(initExpr);

            if (auto* innerCall = initExpr->target->as<CallExprSyntax>())
            {
                Symbol* callSym = context.bindings.get_symbol(innerCall->callee);
                if (callSym && callSym->kind == SymbolKind::Method)
                {
                    ctor = callSym->as<MethodSymbol>();
                }
            }
            else
            {
                std::vector<TypeSymbol*> emptyArgs;
                ctor = attrType->resolve_constructor(emptyArgs);
            }

        }
        else
        {
            std::vector<TypeSymbol*> emptyArgs;
            ctor = attrType->resolve_constructor(emptyArgs);
            if (!ctor)
            {
                error("'" + attrType->name + "' must contain a parameterless constructor to construct with only an initializer list", attr->span);
            }
        }

        out.push_back(ResolvedAttribute{attrType, ctor});
    }
}

void Binder::resolve_all_attributes()
{
    for (auto* type : allTypes)
    {
        currentType = type;
        currentNamespace = type->find_enclosing_namespace();

        auto* typeDecl = type->syntax ? type->syntax->as<TypeDeclSyntax>() : nullptr;
        if (typeDecl)
        {
            resolve_attributes(typeDecl, type->resolvedAttributes);
        }

        for (auto* field : type->fields)
        {
            auto* fieldDecl = field->syntax ? field->syntax->as<FieldDeclSyntax>() : nullptr;
            if (fieldDecl)
            {
                resolve_attributes(fieldDecl, field->resolvedAttributes);
            }
        }

        for (auto* method : type->methods)
        {
            if (method->is_auto_generated())
            {
                continue;
            }

            auto* decl = dynamic_cast<BaseDeclSyntax*>(method->syntax);
            if (decl)
            {
                resolve_attributes(decl, method->resolvedAttributes);
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
    currentNamespace = currentType->find_enclosing_namespace();
    scopes.clear();

    bindingMethods.insert(method);

    BlockExprSyntax* body = nullptr;
    if (method->syntax)
    {
        if (auto* funcAst = method->syntax->as<FunctionDeclSyntax>())
        {
            body = funcAst->body;
        }
        else if (auto* opAst = method->syntax->as<OperatorDeclSyntax>())
        {
            body = opAst->body;
        }
        else if (auto* initAst = method->syntax->as<InitDeclSyntax>())
        {
            body = initAst->body;
        }
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

    if (boundMethods.contains(method) || bindingMethods.contains(method))
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
        if (auto* method = namedType->find_unary_operator(opToken, operandType))
        {
            store_type(expr, method->returnType);
            return method->returnType;
        }

        error("operator '" + std::string(Fern::format(opToken)) +
              "' cannot be applied to value of type '" + namedType->name + "'", expr->span);
    }

    store_type(expr, operandType);
    return operandType;
}

TypeSymbol* Binder::bind_binary(BinaryExprSyntax* expr)
{
    TypeSymbol* leftType = bind_expr(expr->left);
    TypeSymbol* rightType = bind_expr(expr->right);

    TokenKind opToken = binary_op_to_token(expr->op);

    auto* namedType = leftType ? leftType->as<NamedTypeSymbol>() : nullptr;
    if (!namedType)
    {
        store_type(expr, leftType);
        return leftType;
    }

    if (auto* method = namedType->find_binary_operator(opToken, leftType, rightType))
    {
        store_type(expr, method->returnType);
        return method->returnType;
    }

    // Derived comparison operators are synthesized from base operators (e.g. >= from > and ==)
    TokenKind baseToken = TokenKind::Invalid;
    if (opToken == TokenKind::GreaterEqual)
    {
        baseToken = TokenKind::Greater;
    }
    else if (opToken == TokenKind::LessEqual)
    {
        baseToken = TokenKind::Less;
    }
    else if (opToken == TokenKind::NotEqual)
    {
        baseToken = TokenKind::Equal;
    }

    if (baseToken != TokenKind::Invalid)
    {
        bool hasBase = namedType->find_binary_operator(baseToken, leftType, rightType) != nullptr;
        bool hasEqual = (baseToken == TokenKind::Equal) || namedType->find_binary_operator(TokenKind::Equal, leftType, rightType) != nullptr;

        if (hasBase && hasEqual)
        {
            TypeSymbol* boolType = context.resolve_type_name(TokenKind::BoolKeyword);
            store_type(expr, boolType);
            return boolType;
        }

        std::string leftName = leftType ? leftType->name : "?";
        std::string rightName = rightType ? rightType->name : "?";
        std::string msg = "operator '" + std::string(Fern::format(opToken)) +
              "' cannot be applied to values of type '" + leftName + "' and '" + rightName + "'";

        if (!hasBase && !hasEqual)
        {
            msg += " (requires '" + std::string(Fern::format(baseToken)) + "' and '==' operators)";
        }
        else if (!hasBase)
        {
            msg += " (requires '" + std::string(Fern::format(baseToken)) + "' operator)";
        }
        else
        {
            msg += " (requires '==' operator)";
        }

        error(msg, expr->span);
    }
    else
    {
        std::string leftName = leftType ? leftType->name : "?";
        std::string rightName = rightType ? rightType->name : "?";
        error("operator '" + std::string(Fern::format(opToken)) +
              "' cannot be applied to values of type '" + leftName + "' and '" + rightName + "'", expr->span);
    }

    store_type(expr, leftType);
    return leftType;
}

TypeSymbol* Binder::bind_assignment(AssignmentExprSyntax* expr)
{
    TypeSymbol* targetType = bind_expr(expr->target);
    bind_expr(expr->value);

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

    Symbol* calleeSym = context.bindings.get_symbol(expr->callee);

    if (calleeSym && calleeSym->kind == SymbolKind::Type)
    {
        auto* namedType = calleeSym->as<NamedTypeSymbol>();
        if (namedType)
        {
            MethodSymbol* ctor = namedType->resolve_constructor(argTypes);
            if (ctor)
            {
                store_symbol(expr->callee, ctor);
            }
            else
            {
                store_symbol(expr->callee, nullptr);
                if (namedType->has_constructor_with_count(argTypes.size()))
                {
                    error("no constructor for '" + namedType->name +
                          "' matches argument types " + format_arg_types(argTypes), expr->span);
                }
                else
                {
                    error("'" + namedType->name + "' does not contain a constructor that takes " +
                          std::to_string(argTypes.size()) + (argTypes.size() == 1 ? " argument" : " arguments"), expr->span);
                }
            }
            store_type(expr, namedType);
            return namedType;
        }
    }

    MethodSymbol* method = nullptr;
    NamedTypeSymbol* targetType = nullptr;
    std::string_view methodName;

    if (auto* idExpr = expr->callee->as<IdentifierExprSyntax>())
    {
        if (calleeSym && calleeSym->kind == SymbolKind::Method)
        {
            targetType = calleeSym->parent ? calleeSym->parent->as<NamedTypeSymbol>() : nullptr;
            methodName = idExpr->name.lexeme;
        }
        else if (calleeSym)
        {
            error("'" + std::string(idExpr->name.lexeme) + "' cannot be called as a function", idExpr->span);
        }
    }
    else if (auto* memberExpr = expr->callee->as<MemberAccessExprSyntax>())
    {
        if (calleeSym && calleeSym->kind == SymbolKind::Method)
        {
            targetType = calleeSym->parent ? calleeSym->parent->as<NamedTypeSymbol>() : nullptr;
            methodName = memberExpr->right.lexeme;
        }
        else if (calleeSym)
        {
            error("'" + std::string(memberExpr->right.lexeme) + "' cannot be called as a function", memberExpr->span);
        }
    }

    if (targetType && !methodName.empty())
    {
        method = targetType->resolve_method(methodName, argTypes);
        if (method)
        {
            store_symbol(expr->callee, method);
        }
        else
        {
            if (targetType->has_method_with_count(methodName, argTypes.size()))
            {
                error("no overload of '" + std::string(methodName) + "' on '" + targetType->name +
                      "' matches argument types " + format_arg_types(argTypes), expr->span);
            }
            else
            {
                error("'" + targetType->name + "' does not contain a method '" + std::string(methodName) +
                      "' that takes " + std::to_string(argTypes.size()) +
                      (argTypes.size() == 1 ? " argument" : " arguments"), expr->span);
            }
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

    if (!leftType)
    {
        Symbol* leftSym = context.bindings.get_symbol(expr->left);

        if (auto* ns = leftSym ? leftSym->as<NamespaceSymbol>() : nullptr)
        {
            Symbol* member = ns->find_member(memberName);
            if (member)
            {
                store_symbol(expr, member);
                store_type(expr, nullptr);
                return nullptr; // type references have no value type
            }

            error("namespace '" + ns->name + "' has no member '" +
                  std::string(memberName) + "'", expr->span);
            store_type(expr, nullptr);
            return nullptr;
        }

        if (auto* typeRef = leftSym ? leftSym->as<NamedTypeSymbol>() : nullptr)
        {
            if (auto* nested = typeRef->find_nested_type(memberName))
            {
                store_symbol(expr, nested);
                store_type(expr, nullptr);
                return nullptr;
            }

            if (auto* field = typeRef->find_field(memberName))
            {
                if (!has_modifier(field->modifiers, Modifier::Static))
                {
                    error("cannot access instance field '" + std::string(memberName) +
                          "' on type '" + typeRef->name + "'", expr->span);
                    store_type(expr, nullptr);
                    return nullptr;
                }
                store_symbol(expr, field);
                store_type(expr, field->type);
                return field->type;
            }

            if (auto* method = typeRef->find_method(memberName))
            {
                if (!has_modifier(method->modifiers, Modifier::Static))
                {
                    error("cannot access instance method '" + std::string(memberName) +
                          "' on type '" + typeRef->name + "'", expr->span);
                    store_type(expr, nullptr);
                    return nullptr;
                }
                store_symbol(expr, method);
                store_type(expr, method->returnType);
                return method->returnType;
            }

            error("type '" + typeRef->name + "' has no member '" +
                  std::string(memberName) + "'", expr->span);
            store_type(expr, nullptr);
            return nullptr;
        }

        if (leftSym)
        {
            error("'" + leftSym->name + "' is not a namespace or type", expr->span);
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
    if (!expr->target)
    {
        error("cannot infer type for initializer list, use an explicit type name", expr->span);
        for (auto* fieldInit : expr->initializers)
        {
            bind_expr(fieldInit->value);
        }
        store_type(expr, nullptr);
        return nullptr;
    }

    NamedTypeSymbol* namedType = nullptr;

    if (auto* idExpr = expr->target->as<IdentifierExprSyntax>())
    {
        Symbol* sym = resolve_name(idExpr->name.lexeme);
        if (!sym)
        {
            error("undefined name '" + std::string(idExpr->name.lexeme) + "'", idExpr->span);
            store_type(expr, nullptr);
            return nullptr;
        }

        namedType = sym->as<NamedTypeSymbol>();
        if (!namedType)
        {
            error("initializer lists can only be applied to a type or constructor call", idExpr->span);
            store_type(expr, nullptr);
            return nullptr;
        }

        MethodSymbol* defaultCtor = namedType->resolve_constructor({});
        if (defaultCtor)
        {
            store_symbol(idExpr, defaultCtor);
        }
        else
        {
            store_symbol(idExpr, sym);
            error("type '" + namedType->name + "' has no default constructor", idExpr->span);
        }
        store_type(idExpr, namedType);
    }
    else if (auto* callExpr = expr->target->as<CallExprSyntax>())
    {
        TypeSymbol* callType = bind_call(callExpr);
        namedType = callType ? callType->as<NamedTypeSymbol>() : nullptr;

        if (callExpr->callee)
        {
            Symbol* calleeSym = context.bindings.get_symbol(callExpr->callee);
            if (calleeSym)
            {
                auto* method = calleeSym->as<MethodSymbol>();
                if (method && !method->isConstructor)
                {
                    error("initializer lists can only be applied to a type or constructor call", callExpr->span);
                    namedType = nullptr;
                }
            }
        }
    }
    else
    {
        error("initializer lists can only be applied to a type or constructor call", expr->target->span);
        store_type(expr, nullptr);
        return nullptr;
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

    bind_initializer_fields(expr, namedType);
    return namedType;
}

TypeSymbol* Binder::bind_anonymous_initializer(InitializerExprSyntax* expr, NamedTypeSymbol* contextType)
{
    bind_initializer_fields(expr, contextType);
    return contextType;
}

void Binder::bind_initializer_fields(InitializerExprSyntax* expr, NamedTypeSymbol* namedType)
{
    std::vector<std::string> boundFieldPaths;
    for (auto* fieldInit : expr->initializers)
    {
        TypeSymbol* fieldType = nullptr;
        if (fieldInit->target &&
            (fieldInit->target->is<IdentifierExprSyntax>() ||
             fieldInit->target->is<MemberAccessExprSyntax>()))
        {
            fieldType = bind_field_init_target(fieldInit->target, namedType);
        }

        if (fieldInit->value)
        {
            auto* anonInit = fieldInit->value->as<InitializerExprSyntax>();
            if (anonInit && !anonInit->target)
            {
                auto* fieldNamedType = fieldType ? fieldType->as<NamedTypeSymbol>() : nullptr;
                if (fieldNamedType)
                {
                    bind_anonymous_initializer(anonInit, fieldNamedType);
                }
                else if (fieldType)
                {
                    error("cannot use initializer list for non-struct type", anonInit->span);
                    store_type(anonInit, nullptr);
                }
                else
                {
                    store_type(anonInit, nullptr);
                }
            }
            else
            {
                bind_expr(fieldInit->value);
            }
        }
    }

    for (auto* fieldInit : expr->initializers)
    {
        std::string path = format_field_path(fieldInit->target);
        if (path.empty()) continue;

        auto it = std::find(boundFieldPaths.begin(), boundFieldPaths.end(), path);
        if (it != boundFieldPaths.end())
        {
            error("duplicate field '" + path + "' in initializer", fieldInit->target->span);
        }
        else
        {
            boundFieldPaths.push_back(std::move(path));
        }
    }

    store_type(expr, namedType);
}

TypeSymbol* Binder::bind_field_init_target(BaseExprSyntax* target, NamedTypeSymbol* type)
{
    if (auto* id = target->as<IdentifierExprSyntax>())
    {
        FieldSymbol* field = type->find_field(id->name.lexeme);
        if (!field)
        {
            error("type '" + type->name + "' has no field named '" +
                  std::string(id->name.lexeme) + "'", target->span);
            return nullptr;
        }
        store_symbol(id, field);
        store_type(id, field->type);
        return field->type;
    }

    if (auto* member = target->as<MemberAccessExprSyntax>())
    {
        TypeSymbol* leftType = bind_field_init_target(member->left, type);
        if (!leftType)
        {
            return nullptr;
        }

        auto* nestedType = leftType->as<NamedTypeSymbol>();
        if (!nestedType)
        {
            error("cannot access member on non-struct type", member->span);
            return nullptr;
        }

        FieldSymbol* field = nestedType->find_field(member->right.lexeme);
        if (!field)
        {
            error("type '" + nestedType->name + "' has no field named '" +
                  std::string(member->right.lexeme) + "'", member->span);
            return nullptr;
        }
        store_symbol(member, field);
        store_type(member, field->type);
        return field->type;
    }

    error("initializer target must be a field name or member access", target->span);
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
    if (!condType)
    {
        error("if condition must be of type 'bool'", stmt->condition->span);
    }
    else if (boolType && condType != boolType)
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
    if (!condType)
    {
        error("expected condition of type 'bool'", stmt->condition->span);
    }
    else if (boolType && condType != boolType)
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

    Symbol* start = context.symbols.globalNamespace;
    if (currentType) start = currentType;
    else if (currentNamespace) start = currentNamespace;

    std::string_view path[] = {name};
    return context.symbols.lookup_from(start, path);
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
