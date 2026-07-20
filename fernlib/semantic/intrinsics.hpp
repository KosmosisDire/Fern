#pragma once

#include <string_view>

namespace Fern
{

// Every intrinsic the compiler knows. @Intrinsic("name") arguments are validated
// against this set. This is the single contract between core.fn and every backend.
enum class IntrinsicKind
{
    None,
    #define INTRINSIC(name, tag) name,
    #include <semantic/intrinsics.def>
    #undef INTRINSIC
};

// Returns None when the tag is not a known intrinsic
IntrinsicKind intrinsic_from_name(std::string_view tag);

std::string_view format(IntrinsicKind kind);

}
