#include <semantic/context.hpp>

#include <unordered_map>

#include <ast/ast.hpp>
#include <semantic/binder/binder.hpp>
#include <semantic/binder/fhir_builder.hpp>
#include <semantic/binder/method_binder.hpp>
#include <semantic/binder/namespace_binder.hpp>
#include <semantic/binder/root_binder.hpp>
#include <semantic/binder/type_binder.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

static const std::unordered_map<std::string_view, std::pair<std::string_view, std::string_view>> typeAliases =
{
    {"i32",  {"Core", "I32"}},
    {"f32",  {"Core", "F32"}},
    {"bool", {"Core", "Bool"}},
    {"u8",   {"Core", "U8"}},
    {"char", {"Core", "Char"}},
    {"string", {"Core", "String"}},
};

SemanticContext::SemanticContext(AllocArena& arena, Diagnostics& diag)
    : arena(arena)
    , diag(diag)
{
    rootBinder = std::make_unique<RootBinder>(*this, arena);
}

SemanticContext::~SemanticContext() = default;

TypeSymbol* SemanticContext::resolve_type_name(std::string_view alias)
{
    auto it = typeAliases.find(alias);
    if (it != typeAliases.end())
    {
        Symbol* sym = symbols.lookup({it->second.first, it->second.second});
        if (sym) return sym->as<TypeSymbol>();
    }
    return nullptr;
}

TypeSymbol* SemanticContext::resolve_type_name(Token name)
{
    TypeSymbol* aliased = resolve_type_name(name.lexeme);
    if (aliased) return aliased;

    Symbol* sym = symbols.lookup({name.lexeme});
    if (sym) return sym->as<TypeSymbol>();
    return nullptr;
}

std::string SemanticContext::format() const
{
    return symbols.format();
}

Binder& SemanticContext::namespace_binder(NamespaceSymbol* ns)
{
    if (!ns) return *rootBinder;

    auto it = nsBinders.find(ns);
    if (it != nsBinders.end()) return *it->second;

    auto* parentNs = ns->parent ? ns->parent->as<NamespaceSymbol>() : nullptr;
    Binder& parent = namespace_binder(parentNs);

    auto binder = std::make_unique<NamespaceBinder>(parent, ns);
    Binder& ref = *binder;
    nsBinders[ns] = std::move(binder);
    return ref;
}

Binder& SemanticContext::type_binder(NamedTypeSymbol* type)
{
    if (!type) return *rootBinder;

    auto it = typeBinders.find(type);
    if (it != typeBinders.end()) return *it->second;

    Binder& parent = namespace_binder(type->find_enclosing_namespace());

    auto binder = std::make_unique<TypeBinder>(parent, type);

    if (type->is_generic_instantiation() && type->genericOrigin)
    {
        auto* origin = type->genericOrigin;
        auto& subs = binder->substitutions();
        for (size_t i = 0; i < origin->typeParams.size() && i < type->typeArguments.size(); ++i)
        {
            subs[origin->typeParams[i]] = type->typeArguments[i];
        }
    }

    Binder& ref = *binder;
    typeBinders[type] = std::move(binder);
    return ref;
}

Binder& SemanticContext::method_binder(MethodSymbol* method)
{
    if (!method) return *rootBinder;

    auto it = methodBinders.find(method);
    if (it != methodBinders.end()) return *it->second;

    auto* parentType = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
    Binder& parent = type_binder(parentType);

    auto binder = std::make_unique<MethodBinder>(parent, method);
    Binder& ref = *binder;
    methodBinders[method] = std::move(binder);
    return ref;
}

FhirMethod* SemanticContext::bind_single_method(MethodSymbol* method)
{
    if (!method) return nullptr;

    auto it = boundMethods.find(method);
    if (it != boundMethods.end()) return it->second;

    FhirMethod* result = bind_method(method);
    boundMethods[method] = result;
    if (result) methods.push_back(result);
    return result;
}

FhirMethod* SemanticContext::bind_method(MethodSymbol* method)
{
    auto* parentType = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;

    if (!method->syntax && method->is_constructor() && parentType)
    {
        return lower_synthetic_constructor(method, parentType);
    }

    Binder& mBinder = method_binder(method);

    FhirBlock* body = nullptr;
    auto* callable = method->syntax ? method->syntax->as<CallableDeclSyntax>() : nullptr;
    if (callable && callable->body)
    {
        body = mBinder.bind_block(callable->body);
    }

    if (method->is_constructor() && parentType && body)
    {
        std::vector<FhirStmt*> defaults;
        mBinder.emit_field_defaults(parentType, defaults);
        body->statements.insert(body->statements.begin(), defaults.begin(), defaults.end());
    }

    FhirBuilder fhir(arena);
    return fhir.method(method, body);
}

FhirMethod* SemanticContext::lower_synthetic_constructor(MethodSymbol* method, NamedTypeSymbol* parentType)
{
    Binder& mBinder = method_binder(method);
    FhirBuilder fhir(arena);

    auto* ctorBlock = fhir.block(nullptr);

    mBinder.emit_field_defaults(parentType, ctorBlock->statements);

    for (auto* param : method->parameters)
    {
        auto* field = parentType->find_field(param->name);
        if (!field) continue;

        auto* fieldAccess = fhir.field_ref(nullptr, fhir.this_expr(nullptr, parentType), field);
        auto* assignExpr = fhir.assign(nullptr, fieldAccess, fhir.param_ref(nullptr, param));
        ctorBlock->statements.push_back(fhir.expr_stmt(nullptr, assignExpr));
    }

    return fhir.method(method, ctorBlock);
}

}
