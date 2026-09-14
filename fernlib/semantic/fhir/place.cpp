#include <semantic/fhir/fhir.hpp>

namespace Fern
{

// The storage a vetted ref method's result lives in, decided by its receiver alone. Arguments are
// copies that no ref method may return a place inside, so they never matter. A ref type or static
// receiver leaves only heap or pointer storage, a value type receiver puts the result inside the
// receiver's own storage.
static PlaceStorage receiver_storage(const FhirExpr* receiver)
{
    if (!receiver) return PlaceStorage::Heap;
    auto* type = receiver->type ? receiver->type->as<NamedTypeSymbol>() : nullptr;
    if (type && type->is_ref()) return PlaceStorage::Heap;
    return receiver->place_storage();
}

PlaceStorage FhirExpr::place_storage() const
{
    if (is<FhirLocalRefExpr>() || is<FhirParamRefExpr>()) return PlaceStorage::Frame;

    // A value type receiver is passed by address, so this is the caller's storage. A ref type this
    // is a handle sitting in a frame slot.
    if (auto* self = as<FhirThisExpr>())
    {
        auto* selfType = self->type ? self->type->as<NamedTypeSymbol>() : nullptr;
        return selfType && selfType->is_ref() ? PlaceStorage::Frame : PlaceStorage::Receiver;
    }

    // A ref type's field lives on the heap through any handle and a static lives with its type. A
    // value type's instance field is a component of whatever holds the value, so it lives where that
    // does.
    if (auto* field = as<FhirFieldRefExpr>())
    {
        auto* symbol = field->symbol;
        auto* owner = symbol && symbol->parent ? symbol->parent->as<NamedTypeSymbol>() : nullptr;
        if (owner && owner->is_ref()) return PlaceStorage::Heap;
        if (symbol && has_modifier(symbol->modifiers, Modifier::Static)) return PlaceStorage::Heap;
        return field->thisRef ? field->thisRef->place_storage() : PlaceStorage::Temporary;
    }

    if (auto* idx = as<FhirIndexExpr>())
    {
        if (!idx->returns_ref()) return PlaceStorage::Temporary;
        if (idx->getter->intrinsic() == IntrinsicKind::PtrIndex) return PlaceStorage::Pointer;
        return receiver_storage(idx->object);
    }

    if (auto* call = as<FhirCallExpr>())
    {
        if (!call->returns_ref()) return PlaceStorage::Temporary;
        return receiver_storage(call->callee ? call->callee->thisRef : nullptr);
    }

    return PlaceStorage::Temporary;
}

}
