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
    // Type conversion (implicit or explicit)
    enum class ConversionKind : uint8_t
    {
        Identity,           // No conversion needed
        ImplicitNumeric,   // int to long, float to double
        ImplicitReference, // Derived to base
        ExplicitNumeric,   // double to int
        ExplicitReference, // Base to derived  
        Boxing,            // Value type to object
        Unboxing,          // Object to value type
        UserDefined,      // User-defined conversion operator
        NoConversion      // No valid conversion
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
        case ConversionKind::Boxing: return "Boxing";
        case ConversionKind::Unboxing: return "Unboxing";
        case ConversionKind::UserDefined: return "UserDefined";
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

        // Matrix indices (LiteralKind values are non-sequential from TokenKind)
        static constexpr int VOID_IDX = 0;
        static constexpr int BOOL_IDX = 1;
        static constexpr int CHAR_IDX = 2;
        static constexpr int I32_IDX = 3;
        static constexpr int F32_IDX = 4;
        static constexpr int STRING_IDX = 5;
        
        // Conversion matrix using normalized indices
        // Rows = source type, Columns = target type
        static constexpr ConversionKind conversionMatrix[6][6] = {
            // Converting FROM (row) TO (column):
            //          void  bool  char  i32   f32   string
            /*  void */  {IDN, NOC,  NOC,  NOC,  NOC,  NOC},
            /*  bool */  {NOC, IDN,  EXP,  EXP,  EXP,  NOC},
            /*  char */  {NOC, EXP,  IDN,  IMP,  IMP,  NOC},
            /*  i32 */   {NOC, EXP,  EXP,  IDN,  IMP,  NOC},
            /*  f32 */   {NOC, EXP,  EXP,  EXP,  IDN,  NOC},
            /*string */  {NOC, NOC,  NOC,  NOC,  NOC,  IDN},
        };

        // Convert LiteralKind to matrix index
        static constexpr int to_matrix_index(LiteralKind kind)
        {
            switch (kind)
            {
            case LiteralKind::Void:   return VOID_IDX;
            case LiteralKind::Bool:   return BOOL_IDX;
            case LiteralKind::Char:   return CHAR_IDX;
            case LiteralKind::I32:    return I32_IDX;
            case LiteralKind::F32:    return F32_IDX;
            case LiteralKind::String: return STRING_IDX;
            default:                  return -1;  // Invalid/Null
            }
        }

    public:
        /**
         * Get primitive type kind from string name
         */
        static LiteralKind get_primitive_kind(const std::string &typeName)
        {
            static const std::unordered_map<std::string, LiteralKind> typeMap = {
                {"void", LiteralKind::Void},
                {"bool", LiteralKind::Bool},
                {"char", LiteralKind::Char},
                {"i32", LiteralKind::I32},
                {"f32", LiteralKind::F32},
                {"string", LiteralKind::String}};
                
            auto it = typeMap.find(typeName);
            if (it != typeMap.end())
                return it->second;

            // Return void as default for unknown types
            return LiteralKind::Void;
        }

        /**
         * Classify the conversion between two primitive types
         */
        static ConversionKind classify_conversion(LiteralKind source, LiteralKind target)
        {
            int sourceIdx = to_matrix_index(source);
            int targetIdx = to_matrix_index(target);

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

            // For all other types (TypeReference, GenericType, etc.), only identity conversions
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

        /**
         * Get a human-readable description of the conversion
         */
        static std::string describe_conversion(ConversionKind kind)
        {
            switch (kind)
            {
            case ConversionKind::NoConversion:
                return "no conversion";
            case ConversionKind::Identity:
                return "identity";
            case ConversionKind::ImplicitNumeric:
                return "implicit numeric conversion";
            case ConversionKind::ExplicitNumeric:
                return "explicit numeric conversion";
            default:
                return "unknown conversion";
            }
        }
    };

} // namespace Fern