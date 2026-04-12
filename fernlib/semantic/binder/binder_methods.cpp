#include "binder.hpp"

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

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

// binds not only methods, but also all other callable members (constructors, operators, literal methods) since they all share the same syntax and binding logic
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


// output an assignment for each field initializer in a given type
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
