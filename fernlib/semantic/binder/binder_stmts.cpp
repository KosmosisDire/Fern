#include "binder.hpp"

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

#pragma region Method Binding

void Binder::bind_all_methods()
{
    for (auto* method : allMethods)
    {
        auto* parentType = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
        if (!parentType) continue;
        if (parentType->is_generic_definition()) continue;
        if (parentType->is_builtin()) continue;
        bind_method(method);
    }

    for (auto* type : allTypes)
    {
        if (!type->is_generic_definition()) continue;
        size_t count = type->instantiations.size();
        for (size_t i = 0; i < count; ++i)
        {
            auto* inst = type->instantiations[i];
            if (inst->is_builtin()) continue;
            if (!inst->is_concrete_instantiation()) continue;
            context.symbols.ensure_members_populated(inst);
            for (auto* method : inst->methods)
            {
                bind_method(method);
            }
        }
    }
}

void Binder::bind_method(MethodSymbol* method)
{
    if (!method)
    {
        return;
    }

    auto* parentType = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;

    if (!method->syntax && method->is_constructor() && parentType)
    {
        lower_synthetic_constructor(method, parentType);
        return;
    }

    currentMethod = method;
    currentType = parentType;
    currentNamespace = currentType ? currentType->find_enclosing_namespace() : nullptr;
    scopes.clear();
    typeParamSubstitutions.clear();

    // For generic instantiations, set up type param -> concrete type mappings
    // so resolve_type_expr substitutes T with the actual type argument
    if (parentType && parentType->is_generic_instantiation() && parentType->genericOrigin)
    {
        auto* origin = parentType->genericOrigin;
        for (size_t i = 0; i < origin->typeParams.size() && i < parentType->typeArguments.size(); ++i)
        {
            typeParamSubstitutions[origin->typeParams[i]] = parentType->typeArguments[i];
        }
    }

    FhirBlock* body = nullptr;

    auto* callable = method->syntax ? method->syntax->as<CallableDeclSyntax>() : nullptr;
    if (callable && callable->body)
    {
        push_scope();

        for (auto* param : method->parameters)
        {
            current_scope().add(param->name, param);
        }

        body = bind_block(callable->body);

        pop_scope();
    }

    if (method->is_constructor() && parentType && body)
    {
        std::vector<FhirStmt*> defaults;
        emit_field_defaults(parentType, defaults);
        body->statements.insert(body->statements.begin(), defaults.begin(), defaults.end());
    }

    context.methods.push_back(fhir.method(method, body));

    currentMethod = nullptr;
    currentType = nullptr;
    currentNamespace = nullptr;
}

void Binder::lower_synthetic_constructor(MethodSymbol* method, NamedTypeSymbol* parentType)
{
    currentMethod = method;
    currentType = parentType;
    currentNamespace = currentType ? currentType->find_enclosing_namespace() : nullptr;

    auto* ctorBlock = fhir.block(nullptr);

    emit_field_defaults(parentType, ctorBlock->statements);

    for (auto* param : method->parameters)
    {
        auto* field = parentType->find_field(param->name);
        if (!field) continue;

        auto* fieldAccess = fhir.field_access(nullptr, fhir.this_expr(nullptr, parentType), field);
        auto* assignExpr = fhir.assign(nullptr, fieldAccess, fhir.param_ref(nullptr, param));
        ctorBlock->statements.push_back(fhir.expr_stmt(nullptr, assignExpr));
    }

    context.methods.push_back(fhir.method(method, ctorBlock));

    currentMethod = nullptr;
    currentType = nullptr;
    currentNamespace = nullptr;
}

void Binder::emit_field_defaults(NamedTypeSymbol* type, std::vector<FhirStmt*>& out)
{
    for (auto* field : type->fields)
    {
        auto* fieldDecl = field->syntax ? field->syntax->as<FieldDeclSyntax>() : nullptr;
        if (!fieldDecl || !fieldDecl->initializer) continue;

        auto* value = bind_value_expr(fieldDecl->initializer);
        if (!value) continue;

        auto* fieldAccess = fhir.field_access(nullptr, fhir.this_expr(nullptr, type), field);
        auto* assignExpr = fhir.assign(nullptr, fieldAccess, value);
        out.push_back(fhir.expr_stmt(nullptr, assignExpr));
    }
}

#pragma region Statement Binding

FhirBlock* Binder::bind_block(BlockSyntax* block)
{
    auto* node = fhir.block(block);

    auto* prevPending = pendingStmts;
    pendingStmts = &node->statements;

    push_scope();

    for (auto* stmt : block->statements)
    {
        bind_stmt(stmt, node->statements);
    }

    pop_scope();

    pendingStmts = prevPending;
    return node;
}

void Binder::bind_stmt(BaseStmtSyntax* stmt, std::vector<FhirStmt*>& out)
{
    if (!stmt) return;

    if (auto* ret = stmt->as<ReturnStmtSyntax>())
    {
        bind_return(ret, out);
    }
    else if (auto* varDecl = stmt->as<VariableDeclSyntax>())
    {
        bind_var_decl(varDecl, out);
    }
    else if (auto* ifStmt = stmt->as<IfStmtSyntax>())
    {
        bind_if(ifStmt, out);
    }
    else if (auto* whileStmt = stmt->as<WhileStmtSyntax>())
    {
        bind_while(whileStmt, out);
    }
    else if (auto* block = stmt->as<BlockSyntax>())
    {
        auto* bound = bind_block(block);
        for (auto* inner : bound->statements)
        {
            out.push_back(inner);
        }
    }
    else if (auto* exprStmt = stmt->as<ExpressionStmtSyntax>())
    {
        out.push_back(fhir.expr_stmt(exprStmt, bind_value_expr(exprStmt->expression)));
    }
}

void Binder::bind_return(ReturnStmtSyntax* stmt, std::vector<FhirStmt*>& out)
{
    FhirExpr* value = nullptr;
    if (stmt->value)
    {
        value = bind_value_expr(stmt->value);
    }

    TypeSymbol* retType = value ? value->type : nullptr;
    if (retType)
    {
        if (!currentMethod->get_return_type())
        {
            Span loc = currentMethod->syntax ? currentMethod->syntax->span : Span{};
            error("function '" + currentMethod->name +
                  "' returns a value but has no return type annotation", loc);
        }
        else if (retType != currentMethod->get_return_type())
        {
            error("return type '" + format_type_name(retType)
                + "' does not match expected '" + format_type_name(currentMethod->get_return_type()) + "'",
                stmt->span);
        }
    }

    out.push_back(fhir.return_stmt(stmt, value));
}

void Binder::bind_var_decl(VariableDeclSyntax* decl, std::vector<FhirStmt*>& out)
{
    TypeSymbol* type = nullptr;

    if (decl->type)
    {
        type = resolve_type_expr(decl->type);
    }

    FhirExpr* initExpr = nullptr;
    if (decl->initializer)
    {
        initExpr = bind_value_expr(decl->initializer, type);
        TypeSymbol* initType = initExpr ? initExpr->type : nullptr;
        if (!type)
        {
            type = initType;
        }
        if (!type && decl->initializer)
        {
            auto* arrLit = decl->initializer->as<ArrayLiteralExprSyntax>();
            if (arrLit && arrLit->elements.empty())
            {
                error("type cannot be inferred for empty array, consider adding a type annotation or explicit constructor", decl->initializer->span);
            }
        }
    }
    auto localPtr = std::make_unique<LocalSymbol>();
    localPtr->name = std::string(decl->name.lexeme);
    localPtr->type = type;
    localPtr->syntax = decl;
    localPtr->parent = currentMethod;

    auto* local = context.symbols.own(std::move(localPtr));
    current_scope().add(decl->name.lexeme, local);

    out.push_back(fhir.var_decl(decl, local, initExpr));
}

void Binder::check_bool_condition(FhirExpr* condition, const Span& span)
{
    if (condition && condition->is_error()) return;

    TypeSymbol* condType = condition ? condition->type : nullptr;
    TypeSymbol* boolType = context.resolve_type_name("bool");
    if (!condType)
    {
        error("condition must be of type 'bool'", span);
    }
    else if (boolType && condType != boolType)
    {
        error("condition must be of type 'bool', got '" +
              format_type_name(condType) + "'", span);
    }
}

void Binder::bind_if(IfStmtSyntax* stmt, std::vector<FhirStmt*>& out)
{
    FhirExpr* condition = bind_value_expr(stmt->condition);
    check_bool_condition(condition, stmt->condition->span);

    FhirBlock* thenBlock = stmt->thenBody ? bind_block(stmt->thenBody) : nullptr;

    FhirIfStmt* elseIf = nullptr;
    if (stmt->elseIf)
    {
        std::vector<FhirStmt*> elseIfStmts;
        bind_if(stmt->elseIf, elseIfStmts);
        if (!elseIfStmts.empty())
        {
            elseIf = elseIfStmts[0]->as<FhirIfStmt>();
        }
    }

    FhirBlock* elseBlock = stmt->elseBlock ? bind_block(stmt->elseBlock) : nullptr;

    out.push_back(fhir.if_stmt(stmt, condition, thenBlock, elseIf, elseBlock));
}

void Binder::bind_while(WhileStmtSyntax* stmt, std::vector<FhirStmt*>& out)
{
    FhirExpr* condition = bind_value_expr(stmt->condition);
    check_bool_condition(condition, stmt->condition->span);

    FhirBlock* body = stmt->body ? bind_block(stmt->body) : nullptr;

    out.push_back(fhir.while_stmt(stmt, condition, body));
}

}
