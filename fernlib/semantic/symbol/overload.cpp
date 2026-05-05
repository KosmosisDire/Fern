#include "overload.hpp"
#include "symbol.hpp"

namespace Fern::Overload
{

static bool is_better_match(const OverloadMatch& a, const OverloadMatch& b)
{
    if (a.failCount != b.failCount) return a.failCount < b.failCount;
    return a.exactCount > b.exactCount;
}

OverloadMatch grade(MethodSymbol* method, const std::vector<TypeSymbol*>& argTypes)
{
    OverloadMatch match;
    match.method = method;

    if (method->parameters.size() != argTypes.size())
    {
        match.failCount = static_cast<int>(argTypes.size());
        return match;
    }

    for (size_t i = 0; i < argTypes.size(); ++i)
    {
        switch (NamedTypeSymbol::get_conversion(argTypes[i], method->parameters[i]->type).level)
        {
            case Convertibility::Exact:    ++match.exactCount; break;
            case Convertibility::Implicit: ++match.implicitCount; break;
            default:                       ++match.failCount; break;
        }
    }

    return match;
}

static OverloadResult pick_best(const std::vector<OverloadMatch>& pool)
{
    OverloadMatch best = pool[0];
    bool tied = false;
    std::vector<MethodSymbol*> tiedMethods;

    for (size_t i = 1; i < pool.size(); ++i)
    {
        if (is_better_match(pool[i], best))
        {
            best = pool[i];
            tied = false;
            tiedMethods.clear();
        }
        else if (!is_better_match(best, pool[i]))
        {
            if (!tied)
                tiedMethods.push_back(best.method);
            tiedMethods.push_back(pool[i].method);
            tied = true;
        }
    }

    if (tied)
        return {{}, tiedMethods, true};

    return {best};
}

OverloadResult resolve(const std::vector<OverloadMatch>& candidates)
{
    if (candidates.empty()) return {};

    std::vector<OverloadMatch> applicable;
    for (const auto& c : candidates)
    {
        if (c.is_callable())
            applicable.push_back(c);
    }

    return pick_best(applicable.empty() ? candidates : applicable);
}

}
