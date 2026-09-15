#include "overload.hpp"
#include "symbol.hpp"

#include <algorithm>

namespace Fern::Overload
{

// C# picks the signed integer over the unsigned one of the same width when nothing else separates two
// candidates, so p + 1 steps by isize and not usize
static bool is_signed_over_unsigned(TypeSymbol* a, TypeSymbol* b)
{
    auto* left = a ? a->as<NamedTypeSymbol>() : nullptr;
    auto* right = b ? b->as<NamedTypeSymbol>() : nullptr;
    if (!left || !right || !left->is_integer() || !right->is_integer()) return false;
    return !left->is_unsigned() && right->is_unsigned() && left->builtin_scalar_size() == right->builtin_scalar_size();
}

static bool has_better_parameters(const OverloadMatch& a, const OverloadMatch& b)
{
    bool better = false;
    size_t count = std::min(a.method->parameters.size(), b.method->parameters.size());
    for (size_t i = 0; i < count; ++i)
    {
        TypeSymbol* paramA = a.method->parameters[i]->type;
        TypeSymbol* paramB = b.method->parameters[i]->type;
        if (is_signed_over_unsigned(paramB, paramA)) return false;
        if (is_signed_over_unsigned(paramA, paramB)) better = true;
    }
    return better;
}

static bool is_better_match(const OverloadMatch& a, const OverloadMatch& b)
{
    if (a.failCount != b.failCount) return a.failCount < b.failCount;
    if (a.exactCount != b.exactCount) return a.exactCount > b.exactCount;
    return has_better_parameters(a, b);
}

static bool is_better_failure(const OverloadMatch& a, const OverloadMatch& b)
{
    if (a.failCount != b.failCount) return a.failCount < b.failCount;
    if (a.explicitCount != b.explicitCount) return a.explicitCount > b.explicitCount;
    return a.exactCount > b.exactCount;
}

OverloadMatch grade(MethodSymbol* method, const std::vector<OverloadArg>& args)
{
    OverloadMatch match;
    match.method = method;

    if (method->parameters.size() != args.size())
    {
        match.failCount = static_cast<int>(args.size());
        return match;
    }

    for (size_t i = 0; i < args.size(); ++i)
    {
        TypeSymbol* paramType = method->parameters[i]->type;
        switch (NamedTypeSymbol::get_conversion(args[i], paramType).level)
        {
            case Convertibility::Exact:    ++match.exactCount; break;
            case Convertibility::Implicit: ++match.implicitCount; break;
            case Convertibility::Explicit: ++match.explicitCount; ++match.failCount; break;
            default:                       ++match.failCount; break;
        }
    }

    return match;
}

static OverloadResult pick_best(const std::vector<OverloadMatch>& pool)
{
    OverloadResult result;
    OverloadMatch best = pool[0];
    bool tied = false;

    for (size_t i = 1; i < pool.size(); ++i)
    {
        if (is_better_match(pool[i], best))
        {
            best = pool[i];
            tied = false;
            result.ambiguousCandidates.clear();
        }
        else if (!is_better_match(best, pool[i]))
        {
            if (!tied)
                result.ambiguousCandidates.push_back(best.method);
            result.ambiguousCandidates.push_back(pool[i].method);
            tied = true;
        }
    }

    if (tied)
    {
        result.ambiguous = true;
        return result;
    }

    result.best = best;
    return result;
}

OverloadResult resolve(const std::vector<OverloadMatch>& candidates)
{
    OverloadResult result;
    result.candidates = candidates;
    if (candidates.empty()) return result;

    std::vector<OverloadMatch> applicable;
    for (const auto& c : candidates)
    {
        if (c.is_callable())
            applicable.push_back(c);
    }

    if (applicable.empty())
    {
        OverloadMatch bestFail = candidates[0];
        for (size_t i = 1; i < candidates.size(); ++i)
        {
            if (is_better_failure(candidates[i], bestFail))
                bestFail = candidates[i];
        }
        result.bestFailure = bestFail;
        return result;
    }

    OverloadResult picked = pick_best(applicable);
    picked.candidates = candidates;
    return picked;
}

}
