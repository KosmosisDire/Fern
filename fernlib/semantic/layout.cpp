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

    place_statics();
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
        type->isMemoryValue = false;
        type->sizeInBytes = target.pointerSize;
        type->strideInBytes = target.pointerSize;
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
        type->isMemoryValue = false;
        if (auto scalarSize = type->builtin_scalar_size())
        {
            type->sizeInBytes = *scalarSize;
            type->alignment = *scalarSize;
        }
        else
        {
            type->sizeInBytes = target.pointerSize;
            type->alignment = target.pointerAlign;
        }
        type->strideInBytes = type->sizeInBytes;

        // Mark computed before the field walk so a scalar whose value field is its own type terminates.
        type->layoutState = LayoutState::Computed;

        int fieldAlign = 1;
        place_fields(type, fieldAlign);
        return;
    }

    type->isMemoryValue = true;
    type->layoutState = LayoutState::InProgress;

    int structAlign = 1;
    int offset = place_fields(type, structAlign);

    // An empty value type has no bytes of its own but still strides one, so distinct objects in an
    // array get distinct addresses.
    type->sizeInBytes = offset;
    type->strideInBytes = offset == 0 ? 1 : align_up(offset, structAlign);
    type->alignment = structAlign;
    type->layoutState = LayoutState::Computed;
}

// Assigns each instance field the next aligned offset and returns the total span of the fields.
int LayoutPass::place_fields(NamedTypeSymbol* type, int& structAlign)
{
    int offset = 0;
    for (auto* field : type->fields)
    {
        if (has_modifier(field->modifiers, Modifier::Static)) continue;
        offset = place_field(type, field, offset, structAlign);
    }
    return offset;
}

// Static fields of every type share one region, in type then field declaration order
void LayoutPass::place_statics()
{
    StaticLayout& region = context.staticLayout;
    for (auto* type : context.symbols.allTypes)
    {
        if (type->is_generic_definition()) continue;
        for (auto* field : type->fields)
        {
            if (!has_modifier(field->modifiers, Modifier::Static)) continue;
            region.sizeInBytes = place_field(type, field, region.sizeInBytes, region.alignment);
        }
    }
}

// Puts one field at the next aligned spot after offset and returns the offset past it
int LayoutPass::place_field(NamedTypeSymbol* owner, FieldSymbol* field, int offset, int& align)
{
    auto* fieldType = field->type ? field->type->as<NamedTypeSymbol>() : nullptr;
    if (!fieldType) return offset;

    if (fieldType->layoutState == LayoutState::InProgress)
    {
        Span loc = field->syntax ? field->syntax->span : Span{};
        context.diag.report(DiagnosticCode::Err_RecursiveValueField, loc, format_type(owner), field->name);
        return offset;
    }

    compute(fieldType);

    offset = align_up(offset, fieldType->alignment);
    field->offset = offset;
    if (fieldType->alignment > align) align = fieldType->alignment;
    return offset + fieldType->strideInBytes;
}

}
