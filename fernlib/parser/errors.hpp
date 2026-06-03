#pragma once

#include <arena.hpp>
#include <ast/ast.hpp>
#include <common/diagnostic.hpp>

namespace Fern
{

inline ErrorDeclSyntax* make_error_decl(BaseDeclSyntax* decl, DiagnosticCode code, Diagnostics& diag, AllocArena& arena)
{
    diag.report(code, decl->span);
    auto* err = arena.alloc<ErrorDeclSyntax>();
    err->wrapped = decl;
    err->span = decl->span;
    return err;
}

inline ErrorStmtSyntax* make_error_stmt(BaseSyntax* node, DiagnosticCode code, Diagnostics& diag, AllocArena& arena)
{
    diag.report(code, node->span);
    auto* err = arena.alloc<ErrorStmtSyntax>();
    err->wrapped = node;
    err->span = node->span;
    return err;
}

// Namespace and root bodies only accept namespaces and types.
inline BaseDeclSyntax* expect_namespace_member(BaseDeclSyntax* decl, Diagnostics& diag, AllocArena& arena)
{
    if (!decl) return decl;
    if (decl->is<NamespaceDeclSyntax>() || decl->is<TypeDeclSyntax>()) return decl;
    if (decl->is<ErrorDeclSyntax>()) return decl;
    return make_error_decl(decl, DiagnosticCode::Err_BadNamespaceContent, diag, arena);
}

// Type bodies accept fields, callables of any kind, and nested types.
inline BaseDeclSyntax* expect_type_member(BaseDeclSyntax* decl, Diagnostics& diag, AllocArena& arena)
{
    if (!decl) return decl;
    if (decl->is<FieldDeclSyntax>()) return decl;
    if (decl->is<CallableDeclSyntax>()) return decl;
    if (decl->is<TypeDeclSyntax>()) return decl;
    if (decl->is<ErrorDeclSyntax>()) return decl;
    return make_error_decl(decl, DiagnosticCode::Err_BadTypeContent, diag, arena);
}

// Function bodies only hold statements or local declarations
inline BaseStmtSyntax* expect_function_body_stmt(BaseStmtSyntax* stmt, Diagnostics& diag, AllocArena& arena)
{
    if (!stmt) return stmt;
    if (!stmt->is<BaseDeclSyntax>()) return stmt;
    if (stmt->is<VariableDeclSyntax>()) return stmt;
    if (stmt->is<ErrorDeclSyntax>()) return stmt;
    return make_error_stmt(stmt, DiagnosticCode::Err_BadFunctionBodyContent, diag, arena);
}

// Local variables cannot carry attributes or modifiers
inline void validate_local(VariableDeclSyntax* var, Diagnostics& diag)
{
    if (!var) return;
    if (!var->attributes.empty())
    {
        diag.report(DiagnosticCode::Err_AttrOnLocal, var->attributes.front()->span);
    }
    if (var->modifiers != Modifier::None)
    {
        diag.report(DiagnosticCode::Err_ModifierOnLocal, var->span);
    }
}


// ref/attr apply only to type declarations
// implicit/explicit apply only to cast declarations, and a cast requires exactly one of them
inline void validate_modifier(BaseDeclSyntax* decl, Diagnostics& diag)
{
    if (!decl) return;

    if (!decl->is<TypeDeclSyntax>())
    {
        if (auto* tok = decl->modifier_token(TokenKind::Ref))
        {
            diag.report(DiagnosticCode::Err_BadModifierTarget, tok->span, "ref", "type");
        }
        if (auto* tok = decl->modifier_token(TokenKind::Attr))
        {
            diag.report(DiagnosticCode::Err_BadModifierTarget, tok->span, "attr", "type");
        }
    }

    auto* callable = decl->as<CallableDeclSyntax>();
    bool isCast = callable && callable->callableKind == CallableKind::Cast;
    const Token* implicitTok = decl->modifier_token(TokenKind::Implicit);
    const Token* explicitTok = decl->modifier_token(TokenKind::Explicit);

    if (!isCast)
    {
        if (implicitTok)
        {
            diag.report(DiagnosticCode::Err_BadModifierTarget, implicitTok->span, "implicit", "cast");
        }
        if (explicitTok)
        {
            diag.report(DiagnosticCode::Err_BadModifierTarget, explicitTok->span, "explicit", "cast");
        }
    }
    if (isCast && !implicitTok && !explicitTok)
    {
        diag.report(DiagnosticCode::Err_CastMissingExplicitness, decl->span);
    }
    if (implicitTok && explicitTok)
    {
        diag.report(DiagnosticCode::Err_CastBothExplicitness, decl->span);
    }
}

}
