#pragma once

#include "fhir.hpp"

namespace Fern
{

struct Symbol;

struct FhirHitResult
{
    FhirNode* node = nullptr;
    Symbol* nameTarget = nullptr;
};

class FhirHitTest
{
public:
    static FhirHitResult find(FhirNode* root, uint32_t line, uint32_t col);
};

}
