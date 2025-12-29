#pragma once

#include "../ir_type.hpp"
#include <vector>

namespace Fern::FLIR::ABI
{

#pragma region Transform Types

enum class Transform
{
    Direct,     // Pass as-is (primitives, pointers)
    Coerce,     // Struct -> integer (i8/i16/i32/i64)
    Indirect,   // Struct -> pointer (large structs)
    Sret,       // Return -> hidden first parameter
    Split,      // Struct -> multiple arguments (System V, ARM)
    Vector      // Struct -> vector type (System V SSE)
};

#pragma region Argument Info

struct ArgInfo
{
    Transform transform = Transform::Direct;

    // For Coerce/Vector: the type to coerce to
    IRTypePtr coerced_type = nullptr;

    // For Split: the types to split into
    std::vector<IRTypePtr> split_types;

    static ArgInfo direct()
    {
        return { Transform::Direct };
    }

    static ArgInfo coerce(IRTypePtr type)
    {
        ArgInfo info;
        info.transform = Transform::Coerce;
        info.coerced_type = type;
        return info;
    }

    static ArgInfo indirect()
    {
        return { Transform::Indirect };
    }

    static ArgInfo sret()
    {
        return { Transform::Sret };
    }

    static ArgInfo split(std::vector<IRTypePtr> types)
    {
        ArgInfo info;
        info.transform = Transform::Split;
        info.split_types = std::move(types);
        return info;
    }

    static ArgInfo vector(IRTypePtr type)
    {
        ArgInfo info;
        info.transform = Transform::Vector;
        info.coerced_type = type;
        return info;
    }

    bool is_direct() const { return transform == Transform::Direct; }
    bool is_coerce() const { return transform == Transform::Coerce; }
    bool is_indirect() const { return transform == Transform::Indirect; }
    bool is_sret() const { return transform == Transform::Sret; }
    bool is_split() const { return transform == Transform::Split; }
    bool is_vector() const { return transform == Transform::Vector; }

    bool requires_transformation() const { return transform != Transform::Direct; }
};

#pragma region Function ABI Info

struct FunctionABIInfo
{
    ArgInfo return_info;
    std::vector<ArgInfo> param_infos;

    bool requires_transformation() const
    {
        if (return_info.requires_transformation())
            return true;
        for (const auto& param : param_infos)
            if (param.requires_transformation())
                return true;
        return false;
    }
};

} // namespace Fern::FLIR::ABI
