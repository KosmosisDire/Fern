#pragma once

#include <vector>

namespace Fern
{

struct MethodSymbol;
struct TypeSymbol;

struct OverloadMatch
{
    MethodSymbol* method = nullptr;
    int exactCount = 0;
    int implicitCount = 0;
    int explicitCount = 0;
    int failCount = 0;

    bool is_callable() const { return method && failCount == 0; }
};

struct OverloadResult
{
    OverloadMatch best;
    OverloadMatch bestFailure;
    std::vector<MethodSymbol*> ambiguousCandidates;
    std::vector<OverloadMatch> candidates;
    bool ambiguous = false;
};

namespace Overload
{
    OverloadMatch grade(MethodSymbol* method, const std::vector<TypeSymbol*>& argTypes);
    OverloadResult resolve(const std::vector<OverloadMatch>& candidates);
}

}
