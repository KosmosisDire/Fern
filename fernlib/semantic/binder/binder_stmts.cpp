#include "binder.hpp"

#include "scope.hpp"

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

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
    auto* method = containing_method();
    FhirExpr* value = nullptr;
    if (stmt->value)
    {
        TypeSymbol* retType = method->get_return_type();
        value = bind_value_expr(stmt->value, retType);

        if (!retType && value && value->type)
        {
            Span loc = method->syntax ? method->syntax->span : Span{};
            error("function '" + method->name +
                  "' returns a value but has no return type annotation", loc);
        }
    }
    else if (TypeSymbol* retType = method->get_return_type())
    {
        error("function '" + method->name +
              "' expects a return of type '" + format_type_name(retType) + "'", stmt->span);
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

            if (initExpr && type)
            {
                const auto& c = initExpr->get_constant();
                if (c && !c->range_fits(type))
                {
                    error(c->format_range_message(type), decl->initializer->span);
                    initExpr = fhir.error_expr(decl->initializer, type, initExpr);
                }
            }
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
    localPtr->parent = containing_method();

    auto* local = context.symbols.own(std::move(localPtr));

    if (Scope* scope = current_block_scope())
    {
        scope->add(decl->name.lexeme, local);
    }

    out.push_back(fhir.var_decl(decl, local, initExpr));
}

void Binder::bind_if(IfStmtSyntax* stmt, std::vector<FhirStmt*>& out)
{
    FhirExpr* condition = bind_value_expr(stmt->condition, context.resolve_type_name("bool"));

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
    FhirExpr* condition = bind_value_expr(stmt->condition, context.resolve_type_name("bool"));

    FhirBlock* body = stmt->body ? bind_block(stmt->body) : nullptr;

    out.push_back(fhir.while_stmt(stmt, condition, body));
}

}
