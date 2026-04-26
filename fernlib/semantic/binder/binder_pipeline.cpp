#include "binder_pipeline.hpp"

#include <algorithm>

#include <ast/ast.hpp>
#include <semantic/binder/binder.hpp>
#include <semantic/context.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

#pragma region Declaration

void BinderPipeline::declare_symbols(RootSyntax* ast)
{
    if (!ast) return;

    auto* globalNs = context.symbols.globalNamespace;

    for (auto* decl : ast->declarations)
    {
        if (auto* nsDecl = decl->as<NamespaceDeclSyntax>())
        {
            define_namespace(nsDecl, globalNs);
        }
        else if (auto* typeDecl = decl->as<TypeDeclSyntax>())
        {
            define_type(typeDecl, globalNs);
        }
    }
}

static bool collect_namespace_segments(BaseExprSyntax* name, std::vector<BaseExprSyntax*>& out)
{
    if (!name) return false;

    if (name->is<IdentifierExprSyntax>())
    {
        out.push_back(name);
        return true;
    }
    if (auto* member = name->as<MemberAccessExprSyntax>())
    {
        if (!collect_namespace_segments(member->left, out)) return false;
        out.push_back(member);
        return true;
    }
    return false;
}

static std::string_view segment_name(BaseExprSyntax* segment)
{
    if (auto* ident = segment->as<IdentifierExprSyntax>()) return ident->name.lexeme;
    if (auto* member = segment->as<MemberAccessExprSyntax>())
        return member->right ? member->right->name.lexeme : std::string_view{};
    return {};
}

void BinderPipeline::define_namespace(NamespaceDeclSyntax* nsDecl, NamespaceSymbol* parentNs)
{
    if (!nsDecl || !parentNs) return;

    std::vector<BaseExprSyntax*> segments;
    if (!collect_namespace_segments(nsDecl->name, segments) || segments.empty()) return;

    NamespaceSymbol* ns = parentNs;
    for (auto* segment : segments)
    {
        ns = context.symbols.get_or_declare_namespace(ns, segment_name(segment), segment);
        if (!ns) return;
    }

    for (auto* decl : nsDecl->declarations)
    {
        if (auto* nestedNs = decl->as<NamespaceDeclSyntax>())
        {
            define_namespace(nestedNs, ns);
        }
        else if (auto* typeDecl = decl->as<TypeDeclSyntax>())
        {
            define_type(typeDecl, ns);
        }
    }
}

NamedTypeSymbol* BinderPipeline::define_type(TypeDeclSyntax* typeDecl, Symbol* parent)
{
    if (!typeDecl || !parent) return nullptr;

    auto* type = context.symbols.declare_type(parent, typeDecl);

    for (auto& param : typeDecl->typeParams)
    {
        type->typeParams.push_back(std::string(param.lexeme));
    }

    for (int i = 0; i < static_cast<int>(type->typeParams.size()); ++i)
    {
        context.symbols.declare_type_param(type, i, type->typeParams[i]);
    }

    int fieldIndex = 0;
    for (auto* member : typeDecl->declarations)
    {
        if (auto* fieldAst = member->as<FieldDeclSyntax>())
        {
            context.symbols.declare_field(type, fieldAst, fieldIndex++);
        }
        else if (auto* callableAst = member->as<CallableDeclSyntax>())
        {
            auto* method = context.symbols.declare_method(type, callableAst);

            for (int i = 0; i < static_cast<int>(callableAst->parameters.size()); ++i)
            {
                context.symbols.declare_parameter(method, callableAst->parameters[i], i);
            }

            if (callableAst->callableKind == CallableKind::Operator)
            {
                bool isIndexGet = callableAst->name.kind == TokenKind::IndexOp;
                bool isIndexSet = callableAst->name.kind == TokenKind::IndexSetOp;

                if (isIndexGet && method->parameters.size() != 2)
                {
                    context.diag.error("operator '[]' must have 2 parameters (self, index), but has " +
                          std::to_string(method->parameters.size()), callableAst->span);
                }
                else if (isIndexSet && method->parameters.size() != 3)
                {
                    context.diag.error("operator '[]=' must have 3 parameters (self, index, value), but has " +
                          std::to_string(method->parameters.size()), callableAst->span);
                }
                else if (!isIndexGet && !isIndexSet &&
                         (method->parameters.size() < 1 || method->parameters.size() > 2))
                {
                    context.diag.error("operator '" + std::string(Fern::format(callableAst->name.kind)) +
                          "' must have 1 parameter (unary) or 2 parameters (binary), but has " +
                          std::to_string(method->parameters.size()), callableAst->span);
                }
            }
        }
        else if (auto* nestedTypeDecl = member->as<TypeDeclSyntax>())
        {
            define_type(nestedTypeDecl, type);
        }
    }

    bool hasInit = std::any_of(typeDecl->declarations.begin(), typeDecl->declarations.end(),
        [](auto* member)
        {
            auto* callable = member->template as<CallableDeclSyntax>();
            return callable && callable->callableKind == CallableKind::Constructor;
        });

    if (!hasInit)
    {
        auto* method = context.symbols.declare_method(type, "init", Modifier::Public, CallableKind::Constructor);

        for (int i = 0; i < static_cast<int>(type->fields.size()); ++i)
        {
            context.symbols.declare_parameter(method, type->fields[i]->name, i);
        }
    }

    type->membersPopulated = true;
    return type;
}

#pragma region Type Resolution

void BinderPipeline::resolve_signatures()
{
    for (auto* type : context.symbols.allTypes)
    {
        Binder& tBinder = context.type_binder(type);

        for (auto* field : type->fields)
        {
            auto* fieldAst = field->syntax ? field->syntax->as<FieldDeclSyntax>() : nullptr;
            if (fieldAst && fieldAst->type)
            {
                field->type = tBinder.resolve_type_expr(fieldAst->type);
            }
        }

        for (auto* method : type->methods)
        {
            auto* callable = method->syntax ? method->syntax->as<CallableDeclSyntax>() : nullptr;

            if (callable)
            {
                if (method->is_literal())
                {
                    if (callable->returnType)
                    {
                        auto* annotated = tBinder.resolve_type_expr(callable->returnType);
                        if (annotated && annotated != type)
                        {
                            context.diag.error("literal '" + method->name + "' must return '"
                                  + format_type_name(type) + "', not '"
                                  + format_type_name(annotated) + "'", callable->returnType->span);
                        }
                    }
                    method->set_return_type(type);
                }
                else if (callable->returnType)
                {
                    method->set_return_type(tBinder.resolve_type_expr(callable->returnType));
                }
                else if (method->is_constructor())
                {
                    method->set_return_type(type);
                }

                for (size_t i = 0; i < method->parameters.size() && i < callable->parameters.size(); ++i)
                {
                    if (callable->parameters[i]->type)
                    {
                        method->parameters[i]->type = tBinder.resolve_type_expr(callable->parameters[i]->type);
                    }
                }
            }
            else if (method->is_constructor())
            {
                method->set_return_type(type);
                for (size_t i = 0; i < method->parameters.size() && i < type->fields.size(); ++i)
                {
                    method->parameters[i]->type = type->fields[i]->type;
                }
            }
        }
    }
}

#pragma region Attribute Resolution

void BinderPipeline::resolve_attributes()
{
    for (auto* type : context.symbols.allTypes)
    {
        Binder& tBinder = context.type_binder(type);

        auto* typeDecl = type->syntax ? type->syntax->as<TypeDeclSyntax>() : nullptr;
        if (typeDecl)
        {
            tBinder.resolve_attributes(typeDecl, type->resolvedAttributes);
        }

        for (auto* field : type->fields)
        {
            auto* fieldDecl = field->syntax ? field->syntax->as<FieldDeclSyntax>() : nullptr;
            if (fieldDecl)
            {
                tBinder.resolve_attributes(fieldDecl, field->resolvedAttributes);
            }
        }

        for (auto* method : type->methods)
        {
            if (method->is_auto_generated())
            {
                continue;
            }

            auto* callableDecl = method->syntax->as<CallableDeclSyntax>();
            if (callableDecl)
            {
                tBinder.resolve_attributes(callableDecl, method->resolvedAttributes);
            }
        }
    }
}

#pragma region Validation

void BinderPipeline::check_duplicate_methods(NamedTypeSymbol* type)
{
    for (size_t i = 0; i < type->methods.size(); ++i)
    {
        auto* a = type->methods[i];
        for (size_t j = i + 1; j < type->methods.size(); ++j)
        {
            auto* b = type->methods[j];
            if (a->callableKind != b->callableKind || a->name != b->name || a->parameters.size() != b->parameters.size())
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
                switch (b->callableKind)
                {
                    case CallableKind::Constructor:
                        context.diag.error("duplicate constructor on type '" + format_type_name(type) + "'", loc);
                        break;
                    case CallableKind::Operator:
                        context.diag.error("duplicate operator '" + b->name + "' on type '" + format_type_name(type) + "'", loc);
                        break;
                    case CallableKind::Function:
                        context.diag.error("duplicate method '" + b->name + "' on type '" + format_type_name(type) + "'", loc);
                        break;
                    case CallableKind::Literal:
                        context.diag.error("duplicate literal '" + b->name + "' on type '" + format_type_name(type) + "'", loc);
                        break;
                    case CallableKind::Cast:
                        context.diag.error("duplicate cast on type '" + format_type_name(type) + "'", loc);
                        break;
                }
            }
        }
    }
}

void BinderPipeline::validate_signatures()
{
    for (auto* type : context.symbols.allTypes)
    {
        for (auto* method : type->methods)
        {
            if (method->is_operator())
            {
                bool hasContainingType = std::any_of(method->parameters.begin(), method->parameters.end(),
                    [type](auto* param) { return param->type == type; });

                if (!hasContainingType)
                {
                    Span loc = method->syntax ? method->syntax->span : Span{};
                    context.diag.error("operator '" + method->name +
                          "' must have at least one parameter of containing type '" +
                          format_type_name(type) + "'", loc);
                }
            }

            if (method->is_literal())
            {
                Span loc = method->syntax ? method->syntax->span : Span{};
                if (method->parameters.size() != 1)
                {
                    context.diag.error("literal '" + method->name + "' must have exactly 1 parameter", loc);
                }
                else if (auto* paramType = method->parameters[0]->type)
                {
                    auto* namedParamType = paramType->as<NamedTypeSymbol>();
                    if (!namedParamType || !namedParamType->allows_custom_literals())
                    {
                        context.diag.error("literal '" + method->name +
                              "' parameter type '" + format_type_name(paramType) +
                              "' does not allow custom literals", loc);
                    }
                }
            }
        }

        check_duplicate_methods(type);
    }
}

#pragma region Method Body Binding

void BinderPipeline::bind_methods()
{
    for (auto* method : context.symbols.allMethods)
    {
        auto* parentType = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
        if (!parentType) continue;
        if (parentType->is_generic_definition()) continue;
        if (parentType->is_builtin()) continue;
        context.bind_single_method(method);
    }

    for (auto* type : context.symbols.allTypes)
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
                context.bind_single_method(method);
            }
        }
    }
}

}
