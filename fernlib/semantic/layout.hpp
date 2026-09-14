#pragma once

#include <semantic/target.hpp>

namespace Fern
{

struct SemanticContext;
struct NamedTypeSymbol;
struct FieldSymbol;

// The one block that holds every static field. A static field's offset is relative to this block.
struct StaticLayout
{
    int sizeInBytes = 0;
    int alignment = 1;
};

// Computes size, alignment, and field offsets for every concrete value and ref type, and reports
// recursive value fields. Runs after signatures and generic instantiations exist.
class LayoutPass
{
public:
    explicit LayoutPass(SemanticContext& context);

    void run();

    // Idempotent, so a type instantiated after run, like a lowering temp's Ptr<T>, can be laid out on demand
    void compute(NamedTypeSymbol* type);

private:
    SemanticContext& context;
    const TargetInfo& target;

    int place_fields(NamedTypeSymbol* type, int& structAlign);
    void place_statics();
    int place_field(NamedTypeSymbol* owner, FieldSymbol* field, int offset, int& align);
};

}
