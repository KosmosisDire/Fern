#include "intrinsics.hpp"

namespace Fern
{

IntrinsicKind intrinsic_from_name(std::string_view tag)
{
    #define INTRINSIC(name, tagText) if (tag == tagText) return IntrinsicKind::name;
    #include <semantic/intrinsics.def>
    #undef INTRINSIC
    return IntrinsicKind::None;
}

std::string_view format(IntrinsicKind kind)
{
    switch (kind)
    {
        case IntrinsicKind::None: return "";
        #define INTRINSIC(name, tagText) case IntrinsicKind::name: return tagText;
        #include <semantic/intrinsics.def>
        #undef INTRINSIC
    }
    return "";
}

}
