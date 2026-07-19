#pragma once

#include <span>
#include <string_view>

namespace Fern
{

// Registry of every valid intrinsic tag name. @Intrinsic("name") arguments
// are validated against this list. This is the single contract between
// core.fn and every backend.
class Intrinsics
{
public:
    static std::span<const std::string_view> all();
    static bool is_known(std::string_view name);
};

}
