#pragma once

namespace Fern
{

struct SemanticContext;
struct NamedTypeSymbol;

// Byte layout parameters for the target platform. One canonical instance drives the pass so pointer
// width can change later without touching the algorithm.
struct TargetInfo
{
    int pointerSize = 8;
    int pointerAlign = 8;
};

// Computes size, alignment, and field offsets for every concrete value and ref type, and reports
// recursive value fields. Runs after signatures and generic instantiations exist.
class LayoutPass
{
public:
    LayoutPass(SemanticContext& context, TargetInfo target) : context(context), target(target) {}

    void run();

private:
    SemanticContext& context;
    TargetInfo target;

    void compute(NamedTypeSymbol* type);
    int place_fields(NamedTypeSymbol* type, int& structAlign);
};

}
