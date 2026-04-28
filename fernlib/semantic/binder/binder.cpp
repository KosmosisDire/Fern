#include "binder.hpp"

#include "block_binder.hpp"

#include <span>

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/symbol/symbol.hpp>

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
    : context(context)
    , arena(arena)
    , diag(context.diag)
    , fhir(arena)
{
}

Binder::Binder(Binder& parent)
    : next(&parent)
    , context(parent.context)
    , arena(parent.arena)
    , diag(parent.diag)
    , fhir(parent.arena)
{
}

bool LookupResult::is_method_group() const
{
    return !symbols.empty() && symbols[0]->kind == SymbolKind::Method;
}

LookupResult Binder::lookup(std::string_view name)
{
    for (Binder* b = this; b != nullptr; b = b->next)
    {
        LookupResult result = b->lookup_in_single_binder(name);
        if (!result.empty()) return result;
    }
    return {};
}

bool Binder::collect_type_path(TypeExprSyntax* expr, std::vector<std::string_view>& path)
{
    if (auto* idExpr = expr->as<IdentifierExprSyntax>())
    {
        path.push_back(idExpr->name.lexeme);
        return true;
    }
    if (auto* genName = expr->as<GenericNameExprSyntax>())
    {
        path.push_back(genName->name.lexeme);
        return true;
    }
    if (auto* qualExpr = expr->as<QualifiedNameExprSyntax>())
    {
        if (!collect_type_path(qualExpr->left, path)) return false;
        if (!qualExpr->right) return false;
        path.push_back(qualExpr->right->name.lexeme);
        return true;
    }
    return false;
}

TypeSymbol* Binder::resolve_type_expr(TypeExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* idExpr = expr->as<IdentifierExprSyntax>())
    {
        if (auto* subs = type_param_substitutions())
        {
            auto it = subs->find(std::string(idExpr->name.lexeme));
            if (it != subs->end()) return it->second;
        }

        LookupResult result = lookup(idExpr->name.lexeme);
        if (!result.empty())
        {
            if (Symbol* sym = result.single())
            {
                if (auto* type = sym->as<TypeSymbol>()) return type;
            }
            diag.error("'" + std::string(idExpr->name.lexeme) + "' is not a type", expr->span);
            return nullptr;
        }

        diag.error("undefined type '" + std::string(idExpr->name.lexeme) + "'", expr->span);
        return nullptr;
    }

    if (auto* genName = expr->as<GenericNameExprSyntax>())
    {
        return resolve_generic_name(genName);
    }

    if (auto* arrayExpr = expr->as<ArrayTypeExprSyntax>())
    {
        TypeSymbol* elementType = resolve_type_expr(arrayExpr->elementType);
        if (!elementType) return nullptr;

        auto* arrayType = context.symbols.get_or_declare_array_type(elementType);
        if (!arrayType)
        {
            diag.error("Array type not found", expr->span);
            return nullptr;
        }
        return arrayType;
    }

    if (auto* qualExpr = expr->as<QualifiedNameExprSyntax>())
    {
        if (auto* genRight = qualExpr->right ? qualExpr->right->as<GenericNameExprSyntax>() : nullptr)
        {
            Symbol* parentSym = resolve_namespace_or_type(qualExpr->left);
            if (!parentSym)
            {
                std::vector<std::string_view> path;
                collect_type_path(qualExpr->left, path);
                diag.error("undefined type '" + join_path(path) + "." + std::string(genRight->name.lexeme) + "'", qualExpr->left->span);
                return nullptr;
            }
            return resolve_generic_name(genRight, parentSym);
        }

        Symbol* sym = resolve_namespace_or_type(qualExpr);
        if (!sym)
        {
            std::vector<std::string_view> path;
            collect_type_path(qualExpr, path);
            diag.error("undefined type '" + join_path(path) + "'", expr->span);
            return nullptr;
        }
        if (auto* type = sym->as<TypeSymbol>()) return type;

        std::vector<std::string_view> path;
        collect_type_path(qualExpr, path);
        diag.error("'" + join_path(path) + "' is not a type", expr->span);
        return nullptr;
    }

    return nullptr;
}

Symbol* Binder::resolve_namespace_or_type(TypeExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* id = expr->as<IdentifierExprSyntax>())
    {
        return lookup(id->name.lexeme).single();
    }

    if (auto* gen = expr->as<GenericNameExprSyntax>())
    {
        return resolve_generic_name(gen);
    }

    if (auto* qual = expr->as<QualifiedNameExprSyntax>())
    {
        if (!qual->right) return nullptr;
        Symbol* left = resolve_namespace_or_type(qual->left);
        if (!left) return nullptr;

        if (auto* genRight = qual->right->as<GenericNameExprSyntax>())
        {
            return resolve_generic_name(genRight, left);
        }

        std::string_view rightName = qual->right->name.lexeme;
        if (auto* ns = left->as<NamespaceSymbol>())
            return ns->find_member(rightName);
        if (auto* type = left->as<NamedTypeSymbol>())
            return type->find_non_method_member(rightName);
        return nullptr;
    }

    return nullptr;
}

TypeSymbol* Binder::resolve_generic_name(GenericNameExprSyntax* gen, Symbol* parentScope)
{
    size_t arity = gen->typeArgs.size();
    std::string_view name = gen->name.lexeme;
    NamedTypeSymbol* templ = nullptr;

    if (!parentScope)
    {
        NamespaceSymbol* startNs = containing_namespace();
        Symbol* scope = startNs ? static_cast<Symbol*>(startNs) : static_cast<Symbol*>(context.symbols.globalNamespace);
        for (auto* s = scope; s != nullptr; s = s->parent)
        {
            if (auto* ns = s->as<NamespaceSymbol>())
            {
                templ = ns->find_type(name, arity);
                if (templ) break;
            }
        }
    }
    else if (auto* ns = parentScope->as<NamespaceSymbol>())
    {
        templ = ns->find_type(name, arity);
    }
    else if (auto* parentType = parentScope->as<NamedTypeSymbol>())
    {
        auto* nested = parentType->find_nested_type(name);
        if (nested && nested->typeParams.size() == arity) templ = nested;
    }
    else
    {
        diag.error("'" + parentScope->qualified_name() + "' is not a namespace", gen->span);
        return nullptr;
    }

    if (!templ)
    {
        std::string fullName;
        if (parentScope)
        {
            fullName = parentScope->qualified_name() + "." + std::string(name);
        }
        else
        {
            fullName = std::string(name);
        }
        diag.error("undefined type '" + fullName + "'", gen->span);
        return nullptr;
    }

    if (!templ->is_generic_definition())
    {
        diag.error("type '" + format_type_name(templ) + "' is not generic", gen->span);
        return nullptr;
    }

    std::vector<TypeSymbol*> typeArgs;
    for (auto* arg : gen->typeArgs)
    {
        typeArgs.push_back(resolve_type_expr(arg));
    }

    for (auto* arg : typeArgs)
    {
        if (!arg) return nullptr;
    }

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

FhirTypeRef* Binder::build_type_ref_tree(TypeExprSyntax* expr, TypeSymbol* type)
{
    if (!expr) return nullptr;

    std::span<TypeExprSyntax* const> typeArgs;
    if (auto* arr = expr->as<ArrayTypeExprSyntax>())
    {
        typeArgs = std::span<TypeExprSyntax* const>(&arr->elementType, 1);
    }
    else if (auto* gen = expr->as<GenericNameExprSyntax>())
    {
        typeArgs = gen->typeArgs;
    }
    else if (auto* qual = expr->as<QualifiedNameExprSyntax>())
    {
        if (auto* genRight = qual->right ? qual->right->as<GenericNameExprSyntax>() : nullptr)
            typeArgs = genRight->typeArgs;
    }

    std::span<TypeSymbol* const> childTypes;
    if (auto* named = type ? type->as<NamedTypeSymbol>() : nullptr)
    {
        childTypes = named->typeArguments;
    }

    std::vector<FhirTypeRef*> args;
    args.reserve(typeArgs.size());
    for (size_t i = 0; i < typeArgs.size(); ++i)
    {
        TypeSymbol* childType = i < childTypes.size() ? childTypes[i] : nullptr;
        args.push_back(build_type_ref_tree(typeArgs[i], childType));
    }

    return fhir.type_ref(expr, type, std::move(args));
}

FhirTypeRef* Binder::bind_type_ref(TypeExprSyntax* expr)
{
    if (!expr) return nullptr;
    TypeSymbol* type = resolve_type_expr(expr);
    return build_type_ref_tree(expr, type);
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

        auto* fieldAccess = fhir.field_ref(nullptr, fhir.this_expr(nullptr, type), field);
        auto* assignExpr = fhir.assign(nullptr, fieldAccess, value);
        out.push_back(fhir.expr_stmt(nullptr, assignExpr));
    }
}

}
