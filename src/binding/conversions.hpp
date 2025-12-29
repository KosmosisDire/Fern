#pragma once

#include "semantic/type.hpp"
#include "semantic/symbol.hpp"
#include <string>
#include <unordered_map>
#include "common/token_kind.hpp"

// inspired by https://github.com/dotnet/roslyn/blob/main/src/Compilers/CSharp/Portable/Binder/Semantics/Conversions/ConversionEasyOut.cs
// from the roslyn project, licensed under the MIT license.
namespace Fern
{
    /**
     * @enum ConversionKind
     * @brief Represents the kind of type conversion between two types
     */
    enum class ConversionKind : uint8_t
    {
        Identity,           // No conversion needed
        ImplicitNumeric,    // int to float, etc.
        ImplicitReference,  // Derived to base, null to pointer
        ExplicitNumeric,    // float to int
        ExplicitReference,  // Base to derived
        NoConversion        // No valid conversion
    };

    inline std::string to_string(ConversionKind kind)
    {
        switch (kind)
        {
        case ConversionKind::Identity: return "Identity";
        case ConversionKind::ImplicitNumeric: return "ImplicitNumeric";
        case ConversionKind::ImplicitReference: return "ImplicitReference";
        case ConversionKind::ExplicitNumeric: return "ExplicitNumeric";
        case ConversionKind::ExplicitReference: return "ExplicitReference";
        case ConversionKind::NoConversion: return "NoConversion";
        default: return "UnknownConversion";
        }
    }

    /**
     * @class Conversions
     * @brief Manages type conversion rules between primitive types
     */
    class Conversions
    {
    private:
        // Short names for readability in the matrix
        static constexpr ConversionKind NOC = ConversionKind::NoConversion;
        static constexpr ConversionKind IDN = ConversionKind::Identity;
        static constexpr ConversionKind IMP = ConversionKind::ImplicitNumeric;
        static constexpr ConversionKind EXP = ConversionKind::ExplicitNumeric;

        // Conversion matrix using normalized indices
        // Rows = source type, Columns = target type
        // IMP = Implicit (widening, safe), EXP = Explicit (narrowing, lossy), IDN = Identity, NOC = No conversion
        static constexpr ConversionKind conversionMatrix[15][15] = {
            // Converting FROM (row) TO (column):
            //          void  bool  char  i8    u8    i16   u16   i32   u32   i64   u64   f16   f32   f64   string
            /*  void */  {IDN,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC},
            /*  bool */  {NOC,  IDN,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  NOC},
            /*  char */  {NOC,  EXP,  IDN,  IMP,  EXP,  IMP,  EXP,  IMP,  EXP,  IMP,  EXP,  IMP,  IMP,  IMP,  NOC},
            /*   i8 */   {NOC,  EXP,  EXP,  IDN,  EXP,  IMP,  EXP,  IMP,  EXP,  IMP,  EXP,  IMP,  IMP,  IMP,  NOC},
            /*   u8 */   {NOC,  EXP,  EXP,  EXP,  IDN,  EXP,  IMP,  EXP,  IMP,  EXP,  IMP,  IMP,  IMP,  IMP,  NOC},
            /*  i16 */   {NOC,  EXP,  EXP,  EXP,  EXP,  IDN,  EXP,  IMP,  EXP,  IMP,  EXP,  IMP,  IMP,  IMP,  NOC},
            /*  u16 */   {NOC,  EXP,  EXP,  EXP,  EXP,  EXP,  IDN,  EXP,  IMP,  EXP,  IMP,  IMP,  IMP,  IMP,  NOC},
            /*  i32 */   {NOC,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  IDN,  EXP,  IMP,  EXP,  IMP,  IMP,  IMP,  NOC},
            /*  u32 */   {NOC,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  IDN,  EXP,  IMP,  IMP,  IMP,  IMP,  NOC},
            /*  i64 */   {NOC,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  IDN,  EXP,  IMP,  IMP,  IMP,  NOC},
            /*  u64 */   {NOC,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  IDN,  IMP,  IMP,  IMP,  NOC},
            /*  f16 */   {NOC,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  IDN,  IMP,  IMP,  NOC},
            /*  f32 */   {NOC,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  IDN,  IMP,  NOC},
            /*  f64 */   {NOC,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  EXP,  IDN,  NOC},
            /*string */  {NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  NOC,  IDN},
        };

    public:
        /**
         * Classify the conversion between two primitive types
         */
        static ConversionKind classify_conversion(LiteralKind source, LiteralKind target)
        {
            int sourceIdx = get_conversion_matrix_index(source);
            int targetIdx = get_conversion_matrix_index(target);

            // Return NoConversion for invalid/null types
            if (sourceIdx < 0 || targetIdx < 0)
                return ConversionKind::NoConversion;

            return conversionMatrix[sourceIdx][targetIdx];
        }

        /**
         * Classify the conversion between two types (TypePtr version)
         */
        static ConversionKind classify_conversion(TypePtr sourceType, TypePtr targetType)
        {
            // Handle array conversions
            auto sourceArray = sourceType->as<ArrayType>();
            auto targetArray = targetType->as<ArrayType>();

            if (sourceArray && targetArray)
            {
                // Array to array conversion
                // Check if element types match
                if (sourceArray->element == targetArray->element ||
                    sourceArray->element->get_name() == targetArray->element->get_name())
                {
                    // Sized array to unsized array is allowed (char[12] -> char[])
                    // Unsized to unsized is allowed (char[] -> char[])
                    // Sized to same-sized is allowed (char[12] -> char[12])
                    // But sized to different-sized is not (char[12] -> char[10])
                    if (targetArray->size == -1 ||              // Target is unsized array
                        sourceArray->size == -1 ||              // Source is unsized array
                        sourceArray->size == targetArray->size) // Same size
                    {
                        return ConversionKind::Identity; // Treat as identity for parameter passing
                    }
                }
                return ConversionKind::NoConversion;
            }

            // Handle array to pointer decay
            auto targetPointer = targetType->as<PointerType>();
            if (sourceArray && targetPointer)
            {
                // Check if array element type matches pointer target type
                if (sourceArray->element == targetPointer->pointee ||
                    sourceArray->element->get_name() == targetPointer->pointee->get_name())
                {
                    return ConversionKind::Identity; // Array-to-pointer decay is implicit
                }
            }

            // Handle pointer types
            auto sourcePointer = sourceType->as<PointerType>();
            if (sourcePointer && targetPointer)
            {
                if (sourcePointer->pointee == targetPointer->pointee ||
                    sourcePointer->pointee->get_name() == targetPointer->pointee->get_name())
                {
                    return ConversionKind::Identity;
                }
                // void* is implicitly convertible to/from any pointer type (like C)
                if (sourcePointer->pointee->is_void() || targetPointer->pointee->is_void())
                {
                    return ConversionKind::ImplicitReference;
                }
                // Allow explicit casts between different pointer types
                // Pointers are inherently unsafe, so allow conversions with explicit cast
                return ConversionKind::ExplicitReference;
            }

            // Handle null type conversions
            auto sourcePrim = sourceType->as<PrimitiveType>();
            auto targetPrim = targetType->as<PrimitiveType>();
            auto targetNamed = targetType->as<NamedType>();
            auto sourceNamed = sourceType->as<NamedType>();

            // null -> reference type (for null assignment)
            if (sourcePrim && sourcePrim->kind == LiteralKind::Null && targetNamed)
            {
                if (targetNamed->symbol && targetNamed->symbol->is_ref())
                {
                    return ConversionKind::ImplicitReference;
                }
            }

            // null -> pointer type
            if (sourcePrim && sourcePrim->kind == LiteralKind::Null && targetPointer)
            {
                return ConversionKind::ImplicitReference;
            }

            // reference type -> null (for null comparison)
            if (sourceNamed && targetPrim && targetPrim->kind == LiteralKind::Null)
            {
                if (sourceNamed->symbol && sourceNamed->symbol->is_ref())
                {
                    return ConversionKind::ImplicitReference;
                }
            }

            // pointer type -> null (for null comparison)
            if (sourcePointer && targetPrim && targetPrim->kind == LiteralKind::Null)
            {
                return ConversionKind::ImplicitReference;
            }

            // Handle other primitive type conversions
            if (sourcePrim && targetPrim)
            {
                return classify_conversion(sourcePrim->kind, targetPrim->kind);
            }

            // For all other types, only identity conversions
            if (sourceType == targetType || sourceType->get_name() == targetType->get_name())
                return ConversionKind::Identity;

            return ConversionKind::NoConversion;
        }
        
        /**
         * Check if a conversion is implicit (can be done automatically)
         */
        static bool is_implicit_conversion(ConversionKind kind)
        {
            return kind == ConversionKind::Identity ||
                   kind == ConversionKind::ImplicitNumeric ||
                   kind == ConversionKind::ImplicitReference;
        }

        /**
         * Check if a conversion requires an explicit cast
         */
        static bool is_explicit_conversion(ConversionKind kind)
        {
            return kind == ConversionKind::ExplicitNumeric ||
                   kind == ConversionKind::ExplicitReference;
        }

        /**
         * Check if any conversion is possible
         */
        static bool is_conversion_possible(ConversionKind kind)
        {
            return kind != ConversionKind::NoConversion;
        }
    };

} // namespace Fern