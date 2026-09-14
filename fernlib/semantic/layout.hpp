#pragma once

namespace Fern
{

struct SemanticContext;
struct NamedTypeSymbol;
struct FieldSymbol;

// Byte layout parameters for the target platform. One canonical instance drives the pass so pointer
// width can change later without touching the algorithm.
struct TargetInfo
{
    int pointerSize = 8;
    int pointerAlign = 8;
    // Heap blocks behind Array and String hold an i32 length at offset 0 then element data at this offset
    int blockHeaderSize = 8;
};

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
    LayoutPass(SemanticContext& context, TargetInfo target) : context(context), target(target) {}

    void run();

    // Idempotent, so a type instantiated after run, like a lowering temp's Ptr<T>, can be laid out on demand
    void compute(NamedTypeSymbol* type);

private:
    SemanticContext& context;
    TargetInfo target;

    int place_fields(NamedTypeSymbol* type, int& structAlign);
    void place_statics();
    int place_field(NamedTypeSymbol* owner, FieldSymbol* field, int offset, int& align);
};

}
