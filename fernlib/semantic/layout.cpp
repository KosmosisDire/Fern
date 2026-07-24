#include <semantic/layout.hpp>

#include <optional>

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/symbol/fmt.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

static int align_up(int offset, int align)
{
    if (align <= 1) return offset;
    return (offset + align - 1) / align * align;
}

// A size argument on @BuiltinType marks a scalar. Absent means a handle sized value type like String.
static std::optional<int> builtin_scalar_size(NamedTypeSymbol* type)
{
    for (const auto& attr : type->resolvedAttributes)
    {
        if (attr.type && attr.type->qualified_name() == "Core.BuiltinType"
            && !attr.arguments.empty() && attr.arguments[0].kind == ConstantValue::Kind::Int)
        {
            return static_cast<int>(attr.arguments[0].intValue);
        }
    }
    return std::nullopt;
}

void LayoutPass::run()
{
    for (auto* type : context.symbols.allTypes)
    {
        if (type->is_generic_definition()) continue;
        compute(type);
    }

    // Cache the count, populating an instantiation can append new siblings to the same vector.
    for (auto* type : context.symbols.allTypes)
    {
        if (!type->is_generic_definition()) continue;
        size_t count = type->instantiations.size();
        for (size_t i = 0; i < count; ++i)
        {
            auto* inst = type->instantiations[i];
            if (inst->is_concrete_instantiation()) compute(inst);
        }
    }
}

// Sibling of has_default_impl in symbol.cpp, which walks the same field graph for definite assignment.
// This pass owns the recursive value field diagnostic.
void LayoutPass::compute(NamedTypeSymbol* type)
{
    if (!type || type->layoutState == LayoutState::Computed) return;

    // Instantiations reached through field recursion may not be populated yet.
    context.symbols.ensure_members_populated(type);

    // Ref types are handles. Their pointee never contributes to this type's size, so a ref field
    // breaks value cycles the same way a C pointer does. Marking the type computed before walking
    // its payload lets a field of the type itself terminate at the handle size.
    if (type->is_ref())
    {
        type->sizeInBytes = target.pointerSize;
        type->alignment = target.pointerAlign;
        type->layoutState = LayoutState::Computed;

        int payloadAlign = 1;
        int offset = place_fields(type, payloadAlign);
        type->payloadSize = offset == 0 ? 1 : align_up(offset, payloadAlign);
        type->payloadAlign = payloadAlign;
        return;
    }

    // Builtins are leaves. A scalar takes its width from the attribute and never recurses into its
    // value field, which aliases offset 0 of the whole scalar. No size argument means a handle.
    if (type->is_builtin())
    {
        if (auto scalarSize = builtin_scalar_size(type))
        {
            type->sizeInBytes = *scalarSize;
            type->alignment = *scalarSize;
        }
        else
        {
            type->sizeInBytes = target.pointerSize;
            type->alignment = target.pointerAlign;
        }

        // Mark computed before the field walk so a scalar whose value field is its own type terminates.
        type->layoutState = LayoutState::Computed;

        int fieldAlign = 1;
        place_fields(type, fieldAlign);
        return;
    }

    type->layoutState = LayoutState::InProgress;

    int structAlign = 1;
    int offset = place_fields(type, structAlign);

    // An empty value type still occupies one byte so distinct objects get distinct addresses.
    type->sizeInBytes = offset == 0 ? 1 : align_up(offset, structAlign);
    type->alignment = structAlign;
    type->layoutState = LayoutState::Computed;
}

// Assigns each field the next aligned offset and returns the total span of the fields.
int LayoutPass::place_fields(NamedTypeSymbol* type, int& structAlign)
{
    int offset = 0;
    for (auto* field : type->fields)
    {
        auto* fieldType = field->type ? field->type->as<NamedTypeSymbol>() : nullptr;
        if (!fieldType) continue;

        if (fieldType->layoutState == LayoutState::InProgress)
        {
            Span loc = field->syntax ? field->syntax->span : Span{};
            context.diag.report(DiagnosticCode::Err_RecursiveValueField, loc, format_type(type), field->name);
            continue;
        }

        compute(fieldType);

        offset = align_up(offset, fieldType->alignment);
        field->offset = offset;
        offset += fieldType->sizeInBytes;
        if (fieldType->alignment > structAlign) structAlign = fieldType->alignment;
    }
    return offset;
}

}
