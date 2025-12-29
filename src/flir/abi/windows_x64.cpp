#include "windows_x64.hpp"

namespace Fern::FLIR::ABI
{

WindowsX64Rules::WindowsX64Rules(IRTypeSystem* type_system)
    : types(type_system)
{
}

#pragma region Classification

ArgInfo WindowsX64Rules::classify_param(IRTypePtr type)
{
    if (!type->is_struct())
        return ArgInfo::direct();

    size_t size = type->get_size();

    if (can_coerce(size))
        return ArgInfo::coerce(get_coerce_type(size));
    else
        return ArgInfo::indirect();
}

ArgInfo WindowsX64Rules::classify_return(IRTypePtr type)
{
    if (type->is_void())
        return ArgInfo::direct();

    if (!type->is_struct())
        return ArgInfo::direct();

    size_t size = type->get_size();

    if (can_coerce(size))
        return ArgInfo::coerce(get_coerce_type(size));
    else
        return ArgInfo::sret();
}

#pragma region Helpers

bool WindowsX64Rules::can_coerce(size_t size)
{
    // Windows x64: structs of 1, 2, 4, or 8 bytes are passed in registers
    return size == 1 || size == 2 || size == 4 || size == 8;
}

IRTypePtr WindowsX64Rules::get_coerce_type(size_t size)
{
    switch (size)
    {
        case 1: return types->get_i8();
        case 2: return types->get_i16();
        case 4: return types->get_i32();
        case 8: return types->get_i64();
        default: return nullptr;
    }
}

} // namespace Fern::FLIR::ABI
