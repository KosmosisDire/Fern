#include "binder.hpp"

#include <algorithm>

#include <ast/ast.hpp>
#include <semantic/context.hpp>

namespace Fern
{

void Binder::check_duplicate_methods(NamedTypeSymbol* type)
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
                        error("duplicate constructor on type '" + format_type_name(type) + "'", loc);
                        break;
                    case CallableKind::Operator:
                        error("duplicate operator '" + b->name + "' on type '" + format_type_name(type) + "'", loc);
                        break;
                    case CallableKind::Function:
                        error("duplicate method '" + b->name + "' on type '" + format_type_name(type) + "'", loc);
                        break;
                    case CallableKind::Literal:
                        error("duplicate literal '" + b->name + "' on type '" + format_type_name(type) + "'", loc);
                        break;
                    case CallableKind::Cast:
                        error("duplicate cast on type '" + format_type_name(type) + "'", loc);
                        break;
                }
            }
        }
    }
}

void Binder::validate_all_types()
{
    for (auto* type : allTypes)
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
                    error("operator '" + method->name +
                          "' must have at least one parameter of containing type '" +
                          format_type_name(type) + "'", loc);
                }
            }

            if (method->is_literal())
            {
                Span loc = method->syntax ? method->syntax->span : Span{};
                if (method->parameters.size() != 1)
                {
                    error("literal '" + method->name + "' must have exactly 1 parameter", loc);
                }
                else if (auto* paramType = method->parameters[0]->type)
                {
                    auto* namedParamType = paramType->as<NamedTypeSymbol>();
                    if (!namedParamType || !namedParamType->allows_custom_literals())
                    {
                        error("literal '" + method->name +
                              "' parameter type '" + format_type_name(paramType) +
                              "' does not allow custom literals", loc);
                    }
                }
            }
        }

        check_duplicate_methods(type);
    }
}

}
