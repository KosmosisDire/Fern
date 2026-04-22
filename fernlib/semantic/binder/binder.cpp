#include "binder.hpp"

#include "block_binder.hpp"

#include <span>

#include <ast/ast.hpp>
#include <semantic/context.hpp>

namespace Fern
{

static std::string join_path(std::span<const std::string_view> path)
{
    std::string result;
    for (size_t i = 0; i < path.size(); ++i)
    {
        if (i > 0) result += ".";
        result += path[i];
    }
    return result;
}

Binder::Binder(SemanticContext& context, AllocArena& arena)
    : DiagnosticSystem("Binder")
    , context(context)
    , arena(arena)
    , fhir(arena)
{
}

Binder::Binder(Binder& parent)
    : DiagnosticSystem("Binder")
    , next(&parent)
    , context(parent.context)
    , arena(parent.arena)
    , fhir(parent.arena)
{
}

Symbol* Binder::lookup(std::string_view name)
{
    for (Binder* b = this; b != nullptr; b = b->next)
    {
        if (Symbol* sym = b->lookup_in_single_binder(name)) return sym;
    }
    return nullptr;
}

Symbol* Binder::lookup(BaseExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* id = expr->as<IdentifierExprSyntax>())
    {
        return lookup(id->name.lexeme);
    }

    if (auto* member = expr->as<MemberAccessExprSyntax>())
    {
        Symbol* leftSym = lookup(member->left);
        if (!leftSym) return nullptr;

        if (auto* ns = leftSym->as<NamespaceSymbol>())
        {
            return ns->find_member(member->right.lexeme);
        }
        if (auto* typeRef = leftSym->as<NamedTypeSymbol>())
        {
            if (auto* nested = typeRef->find_nested_type(member->right.lexeme))
                return nested;
            if (auto* field = typeRef->find_field(member->right.lexeme))
                return field;
            if (auto* method = typeRef->find_method(member->right.lexeme))
                return method;
        }

        return nullptr;
    }

    if (auto* generic = expr->as<GenericTypeExprSyntax>())
    {
        return resolve_generic_type(generic);
    }

    return nullptr;
}

bool Binder::extract_type_path(BaseExprSyntax* expr, std::vector<std::string_view>& path)
{
    if (auto* idExpr = expr->as<IdentifierExprSyntax>())
    {
        path.push_back(idExpr->name.lexeme);
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

TypeSymbol* Binder::resolve_type_expr(BaseExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* idExpr = expr->as<IdentifierExprSyntax>())
    {
        // Check generic-instantiation substitutions first (e.g. T -> i32)
        if (auto* subs = type_param_substitutions())
        {
            auto it = subs->find(std::string(idExpr->name.lexeme));
            if (it != subs->end()) return it->second;
        }

        // Chain walk: finds builtin aliases at root, type params on TypeBinder,
        // nested types / namespace members, etc.
        if (Symbol* sym = lookup(idExpr->name.lexeme))
        {
            if (auto* type = sym->as<TypeSymbol>()) return type;
            error("'" + std::string(idExpr->name.lexeme) + "' is not a type", expr->span);
            return nullptr;
        }

        error("undefined type '" + std::string(idExpr->name.lexeme) + "'", expr->span);
        return nullptr;
    }

    if (auto* genericExpr = expr->as<GenericTypeExprSyntax>())
    {
        return resolve_generic_type(genericExpr);
    }

    if (auto* arrayExpr = expr->as<ArrayTypeExprSyntax>())
    {
        TypeSymbol* elementType = resolve_type_expr(arrayExpr->elementType);
        if (!elementType) return nullptr;

        auto* arrayType = context.symbols.get_or_declare_array_type(elementType);
        if (!arrayType)
        {
            error("Array type not found", expr->span);
            return nullptr;
        }
        return arrayType;
    }

    if (auto* memberExpr = expr->as<MemberAccessExprSyntax>())
    {
        Symbol* sym = lookup(memberExpr);
        if (!sym)
        {
            std::vector<std::string_view> path;
            extract_type_path(memberExpr, path);
            error("undefined type '" + join_path(path) + "'", expr->span);
            return nullptr;
        }
        if (auto* type = sym->as<TypeSymbol>()) return type;

        std::vector<std::string_view> path;
        extract_type_path(memberExpr, path);
        error("'" + join_path(path) + "' is not a type", expr->span);
        return nullptr;
    }

    return nullptr;
}

TypeSymbol* Binder::resolve_generic_type(GenericTypeExprSyntax* expr)
{
    std::vector<std::string_view> path;
    if (!extract_type_path(expr->base, path))
    {
        error("invalid type in generic expression", expr->base->span);
        return nullptr;
    }

    size_t arity = expr->typeArgs.size();
    NamedTypeSymbol* templ = nullptr;

    if (path.size() == 1)
    {
        // Walk enclosing namespaces looking for a type with matching name + arity
        NamespaceSymbol* startNs = containing_namespace();
        Symbol* scope = startNs ? static_cast<Symbol*>(startNs) : static_cast<Symbol*>(context.symbols.globalNamespace);
        for (auto* s = scope; s != nullptr; s = s->parent)
        {
            if (auto* ns = s->as<NamespaceSymbol>())
            {
                templ = ns->find_type(path[0], arity);
                if (templ) break;
            }
        }
    }
    else
    {
        std::span<const std::string_view> parentPath(path.data(), path.size() - 1);
        NamespaceSymbol* startNs = containing_namespace();
        Symbol* origin = startNs ? static_cast<Symbol*>(startNs) : static_cast<Symbol*>(context.symbols.globalNamespace);
        Symbol* parentSym = context.symbols.lookup_from(origin, parentPath);
        if (auto* ns = parentSym ? parentSym->as<NamespaceSymbol>() : nullptr)
        {
            templ = ns->find_type(path.back(), arity);
        }
        else if (parentSym)
        {
            error("'" + join_path({path.data(), path.size() - 1}) + "' is not a namespace", expr->base->span);
            return nullptr;
        }
    }

    if (!templ)
    {
        error("undefined type '" + join_path(path) + "'", expr->base->span);
        return nullptr;
    }

    if (!templ->is_generic_definition())
    {
        error("type '" + format_type_name(templ) + "' is not generic", expr->span);
        return nullptr;
    }

    std::vector<TypeSymbol*> typeArgs;
    for (auto* arg : expr->typeArgs)
    {
        typeArgs.push_back(resolve_type_expr(arg));
    }

    if (typeArgs.size() != templ->typeParams.size())
    {
        error("type '" + format_type_name(templ) + "' expects " +
              std::to_string(templ->typeParams.size()) + " type argument(s), got " +
              std::to_string(typeArgs.size()), expr->span);
        return nullptr;
    }

    for (auto* arg : typeArgs)
    {
        if (!arg) return nullptr;
    }

    // Self-reference detection: if every arg matches the template's own type
    // params in order (e.g. Box<T> inside Box<T>), return the template itself.
    if (typeArgs.size() == templ->typeParamSymbols.size())
    {
        bool isSelfRef = true;
        for (size_t i = 0; i < typeArgs.size(); ++i)
        {
            if (typeArgs[i] != templ->typeParamSymbols[i])
            {
                isSelfRef = false;
                break;
            }
        }
        if (isSelfRef) return templ;
    }

    return context.symbols.get_or_declare_generic_instance(templ, typeArgs);
}

FhirBlock* Binder::bind_block(BlockSyntax* block)
{
    auto* node = fhir.block(block);

    BlockBinder inner(*this);
    inner.set_pending_statements(&node->statements);

    for (auto* stmt : block->statements)
    {
        inner.bind_stmt(stmt, node->statements);
    }

    return node;
}

void Binder::report(const Diagnostic& diag)
{
    if (next) next->report(diag);
    else context.diagnostics.report(diag);
}

void Binder::info(std::string_view msg, const Span& loc)
{
    if (next) next->info(msg, loc);
    else context.diagnostics.info(msg, loc);
}

void Binder::warn(std::string_view msg, const Span& loc)
{
    if (next) next->warn(msg, loc);
    else context.diagnostics.warn(msg, loc);
}

void Binder::error(std::string_view msg, const Span& loc)
{
    if (next) next->error(msg, loc);
    else context.diagnostics.error(msg, loc);
}

// Emits field-default assignments for a constructor body. Called from the
// constructor-binding path when producing synthetic and declared constructors.
void Binder::emit_field_defaults(NamedTypeSymbol* type, std::vector<FhirStmt*>& out)
{
    for (auto* field : type->fields)
    {
        auto* fieldDecl = field->syntax ? field->syntax->as<FieldDeclSyntax>() : nullptr;
        if (!fieldDecl || !fieldDecl->initializer) continue;

        auto* value = bind_value_expr(fieldDecl->initializer, field->type);
        if (!value) continue;

        auto* fieldAccess = fhir.field_access(nullptr, fhir.this_expr(nullptr, type), field);
        auto* assignExpr = fhir.assign(nullptr, fieldAccess, value);
        out.push_back(fhir.expr_stmt(nullptr, assignExpr));
    }
}

}
