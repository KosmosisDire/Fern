#include <vm/vm.hpp>

#include <cmath>
#include <cstdint>
#include <cstring>
#include <format>
#include <limits>

#include <common/float16.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

#pragma region Numeric Helpers

// Integer add, sub, mul, and neg wrap two's complement
static int8_t wrap_add(int8_t a, int8_t b) { return static_cast<int8_t>(static_cast<uint8_t>(a) + static_cast<uint8_t>(b)); }
static int8_t wrap_sub(int8_t a, int8_t b) { return static_cast<int8_t>(static_cast<uint8_t>(a) - static_cast<uint8_t>(b)); }
static int8_t wrap_mul(int8_t a, int8_t b) { return static_cast<int8_t>(static_cast<uint8_t>(a) * static_cast<uint8_t>(b)); }
static int8_t wrap_neg(int8_t a) { return static_cast<int8_t>(0u - static_cast<uint8_t>(a)); }

static int16_t wrap_add(int16_t a, int16_t b) { return static_cast<int16_t>(static_cast<uint16_t>(a) + static_cast<uint16_t>(b)); }
static int16_t wrap_sub(int16_t a, int16_t b) { return static_cast<int16_t>(static_cast<uint16_t>(a) - static_cast<uint16_t>(b)); }
static int16_t wrap_mul(int16_t a, int16_t b) { return static_cast<int16_t>(static_cast<uint16_t>(a) * static_cast<uint16_t>(b)); }
static int16_t wrap_neg(int16_t a) { return static_cast<int16_t>(0u - static_cast<uint16_t>(a)); }

static int32_t wrap_add(int32_t a, int32_t b) { return static_cast<int32_t>(static_cast<uint32_t>(a) + static_cast<uint32_t>(b)); }
static int32_t wrap_sub(int32_t a, int32_t b) { return static_cast<int32_t>(static_cast<uint32_t>(a) - static_cast<uint32_t>(b)); }
static int32_t wrap_mul(int32_t a, int32_t b) { return static_cast<int32_t>(static_cast<uint32_t>(a) * static_cast<uint32_t>(b)); }
static int32_t wrap_neg(int32_t a) { return static_cast<int32_t>(0u - static_cast<uint32_t>(a)); }

static int64_t wrap_add(int64_t a, int64_t b) { return static_cast<int64_t>(static_cast<uint64_t>(a) + static_cast<uint64_t>(b)); }
static int64_t wrap_sub(int64_t a, int64_t b) { return static_cast<int64_t>(static_cast<uint64_t>(a) - static_cast<uint64_t>(b)); }
static int64_t wrap_mul(int64_t a, int64_t b) { return static_cast<int64_t>(static_cast<uint64_t>(a) * static_cast<uint64_t>(b)); }
static int64_t wrap_neg(int64_t a) { return static_cast<int64_t>(0ull - static_cast<uint64_t>(a)); }

static uint8_t wrap_add(uint8_t a, uint8_t b) { return static_cast<uint8_t>(static_cast<unsigned>(a) + b); }
static uint8_t wrap_sub(uint8_t a, uint8_t b) { return static_cast<uint8_t>(static_cast<unsigned>(a) - b); }
static uint8_t wrap_mul(uint8_t a, uint8_t b) { return static_cast<uint8_t>(static_cast<unsigned>(a) * b); }

static uint16_t wrap_add(uint16_t a, uint16_t b) { return static_cast<uint16_t>(static_cast<unsigned>(a) + b); }
static uint16_t wrap_sub(uint16_t a, uint16_t b) { return static_cast<uint16_t>(static_cast<unsigned>(a) - b); }
static uint16_t wrap_mul(uint16_t a, uint16_t b) { return static_cast<uint16_t>(static_cast<unsigned>(a) * b); }

static uint32_t wrap_add(uint32_t a, uint32_t b) { return a + b; }
static uint32_t wrap_sub(uint32_t a, uint32_t b) { return a - b; }
static uint32_t wrap_mul(uint32_t a, uint32_t b) { return a * b; }

static uint64_t wrap_add(uint64_t a, uint64_t b) { return a + b; }
static uint64_t wrap_sub(uint64_t a, uint64_t b) { return a - b; }
static uint64_t wrap_mul(uint64_t a, uint64_t b) { return a * b; }

// Float to int saturates, matching llvm.fptosi.sat and the wasm saturating conversions. NaN becomes 0.
// The bound compares are exact because every integer limit rounds to a representable double at or
// past the true edge, and values inside the edge truncate into range anyway.
template <typename Int>
static Int saturate_int(double f)
{
    if (f != f) return 0;
    if (f >= static_cast<double>(std::numeric_limits<Int>::max())) return std::numeric_limits<Int>::max();
    if (f <= static_cast<double>(std::numeric_limits<Int>::min())) return std::numeric_limits<Int>::min();
    return static_cast<Int>(f);
}

#pragma region Conversions

Value Interpreter::convert(IntrinsicKind kind, Value operand)
{
    switch (kind)
    {
        // i8
        case IntrinsicKind::I8FromI16:  return Value::make_i8(static_cast<int8_t>(operand.as_i16()));
        case IntrinsicKind::I8FromI32:  return Value::make_i8(static_cast<int8_t>(operand.as_i32()));
        case IntrinsicKind::I8FromI64:  return Value::make_i8(static_cast<int8_t>(operand.as_i64()));
        case IntrinsicKind::I8FromU8:   return Value::make_i8(static_cast<int8_t>(operand.as_u8()));
        case IntrinsicKind::I8FromU16:  return Value::make_i8(static_cast<int8_t>(operand.as_u16()));
        case IntrinsicKind::I8FromU32:  return Value::make_i8(static_cast<int8_t>(operand.as_u32()));
        case IntrinsicKind::I8FromU64:  return Value::make_i8(static_cast<int8_t>(operand.as_u64()));
        case IntrinsicKind::I8FromF16:  return Value::make_i8(saturate_int<int8_t>(f16_to_float(operand.as_f16())));
        case IntrinsicKind::I8FromF32:  return Value::make_i8(saturate_int<int8_t>(operand.as_f32()));
        case IntrinsicKind::I8FromF64:  return Value::make_i8(saturate_int<int8_t>(operand.as_f64()));

        // i16
        case IntrinsicKind::I16FromI8:  return Value::make_i16(static_cast<int16_t>(operand.as_i8()));
        case IntrinsicKind::I16FromI32: return Value::make_i16(static_cast<int16_t>(operand.as_i32()));
        case IntrinsicKind::I16FromI64: return Value::make_i16(static_cast<int16_t>(operand.as_i64()));
        case IntrinsicKind::I16FromU8:  return Value::make_i16(static_cast<int16_t>(operand.as_u8()));
        case IntrinsicKind::I16FromU16: return Value::make_i16(static_cast<int16_t>(operand.as_u16()));
        case IntrinsicKind::I16FromU32: return Value::make_i16(static_cast<int16_t>(operand.as_u32()));
        case IntrinsicKind::I16FromU64: return Value::make_i16(static_cast<int16_t>(operand.as_u64()));
        case IntrinsicKind::I16FromF16: return Value::make_i16(saturate_int<int16_t>(f16_to_float(operand.as_f16())));
        case IntrinsicKind::I16FromF32: return Value::make_i16(saturate_int<int16_t>(operand.as_f32()));
        case IntrinsicKind::I16FromF64: return Value::make_i16(saturate_int<int16_t>(operand.as_f64()));

        // i32
        case IntrinsicKind::I32FromI8:   return Value::make_i32(static_cast<int32_t>(operand.as_i8()));
        case IntrinsicKind::I32FromI16:  return Value::make_i32(static_cast<int32_t>(operand.as_i16()));
        case IntrinsicKind::I32FromI64:  return Value::make_i32(static_cast<int32_t>(operand.as_i64()));
        case IntrinsicKind::I32FromU8:   return Value::make_i32(static_cast<int32_t>(operand.as_u8()));
        case IntrinsicKind::I32FromU16:  return Value::make_i32(static_cast<int32_t>(operand.as_u16()));
        case IntrinsicKind::I32FromU32:  return Value::make_i32(static_cast<int32_t>(operand.as_u32()));
        case IntrinsicKind::I32FromU64:  return Value::make_i32(static_cast<int32_t>(operand.as_u64()));
        case IntrinsicKind::I32FromF16:  return Value::make_i32(saturate_int<int32_t>(f16_to_float(operand.as_f16())));
        case IntrinsicKind::I32FromF32:  return Value::make_i32(saturate_int<int32_t>(operand.as_f32()));
        case IntrinsicKind::I32FromF64:  return Value::make_i32(saturate_int<int32_t>(operand.as_f64()));
        case IntrinsicKind::I32FromBool: return Value::make_i32(operand.as_bool() ? 1 : 0);

        // i64
        case IntrinsicKind::I64FromI8:   return Value::make_i64(static_cast<int64_t>(operand.as_i8()));
        case IntrinsicKind::I64FromI16:  return Value::make_i64(static_cast<int64_t>(operand.as_i16()));
        case IntrinsicKind::I64FromI32:  return Value::make_i64(static_cast<int64_t>(operand.as_i32()));
        case IntrinsicKind::I64FromU8:   return Value::make_i64(static_cast<int64_t>(operand.as_u8()));
        case IntrinsicKind::I64FromU16:  return Value::make_i64(static_cast<int64_t>(operand.as_u16()));
        case IntrinsicKind::I64FromU32:  return Value::make_i64(static_cast<int64_t>(operand.as_u32()));
        case IntrinsicKind::I64FromU64:  return Value::make_i64(static_cast<int64_t>(operand.as_u64()));
        case IntrinsicKind::I64FromF16:  return Value::make_i64(saturate_int<int64_t>(f16_to_float(operand.as_f16())));
        case IntrinsicKind::I64FromF32:  return Value::make_i64(saturate_int<int64_t>(operand.as_f32()));
        case IntrinsicKind::I64FromF64:  return Value::make_i64(saturate_int<int64_t>(operand.as_f64()));
        case IntrinsicKind::I64FromBool: return Value::make_i64(operand.as_bool() ? 1 : 0);

        // u8
        case IntrinsicKind::U8FromI8:   return Value::make_u8(static_cast<uint8_t>(operand.as_i8()));
        case IntrinsicKind::U8FromI16:  return Value::make_u8(static_cast<uint8_t>(operand.as_i16()));
        case IntrinsicKind::U8FromI32:  return Value::make_u8(static_cast<uint8_t>(operand.as_i32()));
        case IntrinsicKind::U8FromI64:  return Value::make_u8(static_cast<uint8_t>(operand.as_i64()));
        case IntrinsicKind::U8FromU16:  return Value::make_u8(static_cast<uint8_t>(operand.as_u16()));
        case IntrinsicKind::U8FromU32:  return Value::make_u8(static_cast<uint8_t>(operand.as_u32()));
        case IntrinsicKind::U8FromU64:  return Value::make_u8(static_cast<uint8_t>(operand.as_u64()));
        case IntrinsicKind::U8FromF16:  return Value::make_u8(saturate_int<uint8_t>(f16_to_float(operand.as_f16())));
        case IntrinsicKind::U8FromF32:  return Value::make_u8(saturate_int<uint8_t>(operand.as_f32()));
        case IntrinsicKind::U8FromF64:  return Value::make_u8(saturate_int<uint8_t>(operand.as_f64()));

        // u16
        case IntrinsicKind::U16FromI8:  return Value::make_u16(static_cast<uint16_t>(operand.as_i8()));
        case IntrinsicKind::U16FromI16: return Value::make_u16(static_cast<uint16_t>(operand.as_i16()));
        case IntrinsicKind::U16FromI32: return Value::make_u16(static_cast<uint16_t>(operand.as_i32()));
        case IntrinsicKind::U16FromI64: return Value::make_u16(static_cast<uint16_t>(operand.as_i64()));
        case IntrinsicKind::U16FromU8:  return Value::make_u16(static_cast<uint16_t>(operand.as_u8()));
        case IntrinsicKind::U16FromU32: return Value::make_u16(static_cast<uint16_t>(operand.as_u32()));
        case IntrinsicKind::U16FromU64: return Value::make_u16(static_cast<uint16_t>(operand.as_u64()));
        case IntrinsicKind::U16FromF16: return Value::make_u16(saturate_int<uint16_t>(f16_to_float(operand.as_f16())));
        case IntrinsicKind::U16FromF32: return Value::make_u16(saturate_int<uint16_t>(operand.as_f32()));
        case IntrinsicKind::U16FromF64: return Value::make_u16(saturate_int<uint16_t>(operand.as_f64()));

        // u32
        case IntrinsicKind::U32FromI8:  return Value::make_u32(static_cast<uint32_t>(operand.as_i8()));
        case IntrinsicKind::U32FromI16: return Value::make_u32(static_cast<uint32_t>(operand.as_i16()));
        case IntrinsicKind::U32FromI32: return Value::make_u32(static_cast<uint32_t>(operand.as_i32()));
        case IntrinsicKind::U32FromI64: return Value::make_u32(static_cast<uint32_t>(operand.as_i64()));
        case IntrinsicKind::U32FromU8:  return Value::make_u32(static_cast<uint32_t>(operand.as_u8()));
        case IntrinsicKind::U32FromU16: return Value::make_u32(static_cast<uint32_t>(operand.as_u16()));
        case IntrinsicKind::U32FromU64: return Value::make_u32(static_cast<uint32_t>(operand.as_u64()));
        case IntrinsicKind::U32FromF16: return Value::make_u32(saturate_int<uint32_t>(f16_to_float(operand.as_f16())));
        case IntrinsicKind::U32FromF32: return Value::make_u32(saturate_int<uint32_t>(operand.as_f32()));
        case IntrinsicKind::U32FromF64: return Value::make_u32(saturate_int<uint32_t>(operand.as_f64()));

        // u64
        case IntrinsicKind::U64FromI8:  return Value::make_u64(static_cast<uint64_t>(operand.as_i8()));
        case IntrinsicKind::U64FromI16: return Value::make_u64(static_cast<uint64_t>(operand.as_i16()));
        case IntrinsicKind::U64FromI32: return Value::make_u64(static_cast<uint64_t>(operand.as_i32()));
        case IntrinsicKind::U64FromI64: return Value::make_u64(static_cast<uint64_t>(operand.as_i64()));
        case IntrinsicKind::U64FromU8:  return Value::make_u64(static_cast<uint64_t>(operand.as_u8()));
        case IntrinsicKind::U64FromU16: return Value::make_u64(static_cast<uint64_t>(operand.as_u16()));
        case IntrinsicKind::U64FromU32: return Value::make_u64(static_cast<uint64_t>(operand.as_u32()));
        case IntrinsicKind::U64FromF16: return Value::make_u64(saturate_int<uint64_t>(f16_to_float(operand.as_f16())));
        case IntrinsicKind::U64FromF32: return Value::make_u64(saturate_int<uint64_t>(operand.as_f32()));
        case IntrinsicKind::U64FromF64: return Value::make_u64(saturate_int<uint64_t>(operand.as_f64()));

        // f16 rounds through one double to half conversion, int to double is exact at every relevant magnitude
        case IntrinsicKind::F16FromI8:  return Value::make_f16(f16_from_double(static_cast<double>(operand.as_i8())));
        case IntrinsicKind::F16FromI16: return Value::make_f16(f16_from_double(static_cast<double>(operand.as_i16())));
        case IntrinsicKind::F16FromI32: return Value::make_f16(f16_from_double(static_cast<double>(operand.as_i32())));
        case IntrinsicKind::F16FromI64: return Value::make_f16(f16_from_double(static_cast<double>(operand.as_i64())));
        case IntrinsicKind::F16FromU8:  return Value::make_f16(f16_from_double(static_cast<double>(operand.as_u8())));
        case IntrinsicKind::F16FromU16: return Value::make_f16(f16_from_double(static_cast<double>(operand.as_u16())));
        case IntrinsicKind::F16FromU32: return Value::make_f16(f16_from_double(static_cast<double>(operand.as_u32())));
        case IntrinsicKind::F16FromU64: return Value::make_f16(f16_from_double(static_cast<double>(operand.as_u64())));
        case IntrinsicKind::F16FromF32: return Value::make_f16(f16_from_double(operand.as_f32()));
        case IntrinsicKind::F16FromF64: return Value::make_f16(f16_from_double(operand.as_f64()));

        // f32
        case IntrinsicKind::F32FromI8:  return Value::make_f32(static_cast<float>(operand.as_i8()));
        case IntrinsicKind::F32FromI16: return Value::make_f32(static_cast<float>(operand.as_i16()));
        case IntrinsicKind::F32FromI32: return Value::make_f32(static_cast<float>(operand.as_i32()));
        case IntrinsicKind::F32FromI64: return Value::make_f32(static_cast<float>(operand.as_i64()));
        case IntrinsicKind::F32FromU8:  return Value::make_f32(static_cast<float>(operand.as_u8()));
        case IntrinsicKind::F32FromU16: return Value::make_f32(static_cast<float>(operand.as_u16()));
        case IntrinsicKind::F32FromU32: return Value::make_f32(static_cast<float>(operand.as_u32()));
        case IntrinsicKind::F32FromU64: return Value::make_f32(static_cast<float>(operand.as_u64()));
        case IntrinsicKind::F32FromF16: return Value::make_f32(f16_to_float(operand.as_f16()));
        case IntrinsicKind::F32FromF64: return Value::make_f32(static_cast<float>(operand.as_f64()));

        // f64
        case IntrinsicKind::F64FromI8:  return Value::make_f64(static_cast<double>(operand.as_i8()));
        case IntrinsicKind::F64FromI16: return Value::make_f64(static_cast<double>(operand.as_i16()));
        case IntrinsicKind::F64FromI32: return Value::make_f64(static_cast<double>(operand.as_i32()));
        case IntrinsicKind::F64FromI64: return Value::make_f64(static_cast<double>(operand.as_i64()));
        case IntrinsicKind::F64FromU8:  return Value::make_f64(static_cast<double>(operand.as_u8()));
        case IntrinsicKind::F64FromU16: return Value::make_f64(static_cast<double>(operand.as_u16()));
        case IntrinsicKind::F64FromU32: return Value::make_f64(static_cast<double>(operand.as_u32()));
        case IntrinsicKind::F64FromU64: return Value::make_f64(static_cast<double>(operand.as_u64()));
        case IntrinsicKind::F64FromF16: return Value::make_f64(static_cast<double>(f16_to_float(operand.as_f16())));
        case IntrinsicKind::F64FromF32: return Value::make_f64(static_cast<double>(operand.as_f32()));

        // bool
        case IntrinsicKind::BoolFromI32: return Value::make_bool(operand.as_i32() != 0);
        case IntrinsicKind::BoolFromI64: return Value::make_bool(operand.as_i64() != 0);

        default: break;
    }
    throw VmError{std::format("conversion not implemented: {}", format(kind))};
}

#pragma region Intrinsic Dispatch

// Reads the element type of an intrinsic method's Array<T> receiver.
static TypeSymbol* array_elem_type(MethodSymbol* method)
{
    auto* parent = method && method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
    if (parent && !parent->typeArguments.empty()) return parent->typeArguments[0];
    return nullptr;
}

static uint64_t elem_size(TypeSymbol* type)
{
    auto* named = type ? type->as<NamedTypeSymbol>() : nullptr;
    return named ? static_cast<uint64_t>(named->sizeInBytes) : 0;
}

Value Interpreter::exec_intrinsic(FlirIntrinsic* node)
{
    IntrinsicKind kind = node->method ? node->method->intrinsic() : IntrinsicKind::None;

    Value self;
    bool hasSelf = node->thisArg != nullptr;
    if (hasSelf) self = eval(node->thisArg);

    std::vector<Value> args;
    args.reserve(node->args.size());
    for (auto* arg : node->args)
        args.push_back(eval(arg));

    uint64_t header = static_cast<uint64_t>(target.blockHeaderSize);

    switch (kind)
    {
        // i8 arithmetic and compares
        case IntrinsicKind::I8Neg: return Value::make_i8(wrap_neg(args[0].as_i8()));
        case IntrinsicKind::I8Pos: return Value::make_i8(args[0].as_i8());
        case IntrinsicKind::I8Add: return Value::make_i8(wrap_add(args[0].as_i8(), args[1].as_i8()));
        case IntrinsicKind::I8Sub: return Value::make_i8(wrap_sub(args[0].as_i8(), args[1].as_i8()));
        case IntrinsicKind::I8Mul: return Value::make_i8(wrap_mul(args[0].as_i8(), args[1].as_i8()));
        case IntrinsicKind::I8Div:
        {
            int8_t a = args[0].as_i8();
            int8_t b = args[1].as_i8();
            if (b == 0) throw VmError{"division by zero"};
            if (a == INT8_MIN && b == -1) throw VmError{"division overflow"};
            return Value::make_i8(static_cast<int8_t>(a / b));
        }
        case IntrinsicKind::I8Mod:
        {
            int8_t a = args[0].as_i8();
            int8_t b = args[1].as_i8();
            if (b == 0) throw VmError{"division by zero"};
            if (a == INT8_MIN && b == -1) throw VmError{"division overflow"};
            return Value::make_i8(static_cast<int8_t>(a % b));
        }
        case IntrinsicKind::I8Eq: return Value::make_bool(args[0].as_i8() == args[1].as_i8());
        case IntrinsicKind::I8Ne: return Value::make_bool(args[0].as_i8() != args[1].as_i8());
        case IntrinsicKind::I8Gt: return Value::make_bool(args[0].as_i8() > args[1].as_i8());
        case IntrinsicKind::I8Lt: return Value::make_bool(args[0].as_i8() < args[1].as_i8());
        case IntrinsicKind::I8Ge: return Value::make_bool(args[0].as_i8() >= args[1].as_i8());
        case IntrinsicKind::I8Le: return Value::make_bool(args[0].as_i8() <= args[1].as_i8());

        // i16 arithmetic and compares
        case IntrinsicKind::I16Neg: return Value::make_i16(wrap_neg(args[0].as_i16()));
        case IntrinsicKind::I16Pos: return Value::make_i16(args[0].as_i16());
        case IntrinsicKind::I16Add: return Value::make_i16(wrap_add(args[0].as_i16(), args[1].as_i16()));
        case IntrinsicKind::I16Sub: return Value::make_i16(wrap_sub(args[0].as_i16(), args[1].as_i16()));
        case IntrinsicKind::I16Mul: return Value::make_i16(wrap_mul(args[0].as_i16(), args[1].as_i16()));
        case IntrinsicKind::I16Div:
        {
            int16_t a = args[0].as_i16();
            int16_t b = args[1].as_i16();
            if (b == 0) throw VmError{"division by zero"};
            if (a == INT16_MIN && b == -1) throw VmError{"division overflow"};
            return Value::make_i16(static_cast<int16_t>(a / b));
        }
        case IntrinsicKind::I16Mod:
        {
            int16_t a = args[0].as_i16();
            int16_t b = args[1].as_i16();
            if (b == 0) throw VmError{"division by zero"};
            if (a == INT16_MIN && b == -1) throw VmError{"division overflow"};
            return Value::make_i16(static_cast<int16_t>(a % b));
        }
        case IntrinsicKind::I16Eq: return Value::make_bool(args[0].as_i16() == args[1].as_i16());
        case IntrinsicKind::I16Ne: return Value::make_bool(args[0].as_i16() != args[1].as_i16());
        case IntrinsicKind::I16Gt: return Value::make_bool(args[0].as_i16() > args[1].as_i16());
        case IntrinsicKind::I16Lt: return Value::make_bool(args[0].as_i16() < args[1].as_i16());
        case IntrinsicKind::I16Ge: return Value::make_bool(args[0].as_i16() >= args[1].as_i16());
        case IntrinsicKind::I16Le: return Value::make_bool(args[0].as_i16() <= args[1].as_i16());

        // i32 arithmetic and compares
        case IntrinsicKind::I32Neg: return Value::make_i32(wrap_neg(args[0].as_i32()));
        case IntrinsicKind::I32Pos: return Value::make_i32(args[0].as_i32());
        case IntrinsicKind::I32Add: return Value::make_i32(wrap_add(args[0].as_i32(), args[1].as_i32()));
        case IntrinsicKind::I32Sub: return Value::make_i32(wrap_sub(args[0].as_i32(), args[1].as_i32()));
        case IntrinsicKind::I32Mul: return Value::make_i32(wrap_mul(args[0].as_i32(), args[1].as_i32()));
        case IntrinsicKind::I32Div:
        {
            int32_t a = args[0].as_i32();
            int32_t b = args[1].as_i32();
            if (b == 0) throw VmError{"division by zero"};
            if (a == INT32_MIN && b == -1) throw VmError{"division overflow"};
            return Value::make_i32(a / b);
        }
        case IntrinsicKind::I32Mod:
        {
            int32_t a = args[0].as_i32();
            int32_t b = args[1].as_i32();
            if (b == 0) throw VmError{"division by zero"};
            if (a == INT32_MIN && b == -1) throw VmError{"division overflow"};
            return Value::make_i32(a % b);
        }
        case IntrinsicKind::I32Eq: return Value::make_bool(args[0].as_i32() == args[1].as_i32());
        case IntrinsicKind::I32Ne: return Value::make_bool(args[0].as_i32() != args[1].as_i32());
        case IntrinsicKind::I32Gt: return Value::make_bool(args[0].as_i32() > args[1].as_i32());
        case IntrinsicKind::I32Lt: return Value::make_bool(args[0].as_i32() < args[1].as_i32());
        case IntrinsicKind::I32Ge: return Value::make_bool(args[0].as_i32() >= args[1].as_i32());
        case IntrinsicKind::I32Le: return Value::make_bool(args[0].as_i32() <= args[1].as_i32());

        // i64 arithmetic and compares
        case IntrinsicKind::I64Neg: return Value::make_i64(wrap_neg(args[0].as_i64()));
        case IntrinsicKind::I64Pos: return Value::make_i64(args[0].as_i64());
        case IntrinsicKind::I64Add: return Value::make_i64(wrap_add(args[0].as_i64(), args[1].as_i64()));
        case IntrinsicKind::I64Sub: return Value::make_i64(wrap_sub(args[0].as_i64(), args[1].as_i64()));
        case IntrinsicKind::I64Mul: return Value::make_i64(wrap_mul(args[0].as_i64(), args[1].as_i64()));
        case IntrinsicKind::I64Div:
        {
            int64_t a = args[0].as_i64();
            int64_t b = args[1].as_i64();
            if (b == 0) throw VmError{"division by zero"};
            if (a == INT64_MIN && b == -1) throw VmError{"division overflow"};
            return Value::make_i64(a / b);
        }
        case IntrinsicKind::I64Mod:
        {
            int64_t a = args[0].as_i64();
            int64_t b = args[1].as_i64();
            if (b == 0) throw VmError{"division by zero"};
            if (a == INT64_MIN && b == -1) throw VmError{"division overflow"};
            return Value::make_i64(a % b);
        }
        case IntrinsicKind::I64Eq: return Value::make_bool(args[0].as_i64() == args[1].as_i64());
        case IntrinsicKind::I64Ne: return Value::make_bool(args[0].as_i64() != args[1].as_i64());
        case IntrinsicKind::I64Gt: return Value::make_bool(args[0].as_i64() > args[1].as_i64());
        case IntrinsicKind::I64Lt: return Value::make_bool(args[0].as_i64() < args[1].as_i64());
        case IntrinsicKind::I64Ge: return Value::make_bool(args[0].as_i64() >= args[1].as_i64());
        case IntrinsicKind::I64Le: return Value::make_bool(args[0].as_i64() <= args[1].as_i64());

        // u8 arithmetic and compares
        case IntrinsicKind::U8Add: return Value::make_u8(wrap_add(args[0].as_u8(), args[1].as_u8()));
        case IntrinsicKind::U8Sub: return Value::make_u8(wrap_sub(args[0].as_u8(), args[1].as_u8()));
        case IntrinsicKind::U8Mul: return Value::make_u8(wrap_mul(args[0].as_u8(), args[1].as_u8()));
        case IntrinsicKind::U8Div:
        {
            uint8_t b = args[1].as_u8();
            if (b == 0) throw VmError{"division by zero"};
            return Value::make_u8(static_cast<uint8_t>(args[0].as_u8() / b));
        }
        case IntrinsicKind::U8Mod:
        {
            uint8_t b = args[1].as_u8();
            if (b == 0) throw VmError{"division by zero"};
            return Value::make_u8(static_cast<uint8_t>(args[0].as_u8() % b));
        }
        case IntrinsicKind::U8Eq: return Value::make_bool(args[0].as_u8() == args[1].as_u8());
        case IntrinsicKind::U8Ne: return Value::make_bool(args[0].as_u8() != args[1].as_u8());
        case IntrinsicKind::U8Gt: return Value::make_bool(args[0].as_u8() > args[1].as_u8());
        case IntrinsicKind::U8Lt: return Value::make_bool(args[0].as_u8() < args[1].as_u8());
        case IntrinsicKind::U8Ge: return Value::make_bool(args[0].as_u8() >= args[1].as_u8());
        case IntrinsicKind::U8Le: return Value::make_bool(args[0].as_u8() <= args[1].as_u8());

        // u16 arithmetic and compares
        case IntrinsicKind::U16Add: return Value::make_u16(wrap_add(args[0].as_u16(), args[1].as_u16()));
        case IntrinsicKind::U16Sub: return Value::make_u16(wrap_sub(args[0].as_u16(), args[1].as_u16()));
        case IntrinsicKind::U16Mul: return Value::make_u16(wrap_mul(args[0].as_u16(), args[1].as_u16()));
        case IntrinsicKind::U16Div:
        {
            uint16_t b = args[1].as_u16();
            if (b == 0) throw VmError{"division by zero"};
            return Value::make_u16(static_cast<uint16_t>(args[0].as_u16() / b));
        }
        case IntrinsicKind::U16Mod:
        {
            uint16_t b = args[1].as_u16();
            if (b == 0) throw VmError{"division by zero"};
            return Value::make_u16(static_cast<uint16_t>(args[0].as_u16() % b));
        }
        case IntrinsicKind::U16Eq: return Value::make_bool(args[0].as_u16() == args[1].as_u16());
        case IntrinsicKind::U16Ne: return Value::make_bool(args[0].as_u16() != args[1].as_u16());
        case IntrinsicKind::U16Gt: return Value::make_bool(args[0].as_u16() > args[1].as_u16());
        case IntrinsicKind::U16Lt: return Value::make_bool(args[0].as_u16() < args[1].as_u16());
        case IntrinsicKind::U16Ge: return Value::make_bool(args[0].as_u16() >= args[1].as_u16());
        case IntrinsicKind::U16Le: return Value::make_bool(args[0].as_u16() <= args[1].as_u16());

        // u32 arithmetic and compares
        case IntrinsicKind::U32Add: return Value::make_u32(wrap_add(args[0].as_u32(), args[1].as_u32()));
        case IntrinsicKind::U32Sub: return Value::make_u32(wrap_sub(args[0].as_u32(), args[1].as_u32()));
        case IntrinsicKind::U32Mul: return Value::make_u32(wrap_mul(args[0].as_u32(), args[1].as_u32()));
        case IntrinsicKind::U32Div:
        {
            uint32_t b = args[1].as_u32();
            if (b == 0) throw VmError{"division by zero"};
            return Value::make_u32(args[0].as_u32() / b);
        }
        case IntrinsicKind::U32Mod:
        {
            uint32_t b = args[1].as_u32();
            if (b == 0) throw VmError{"division by zero"};
            return Value::make_u32(args[0].as_u32() % b);
        }
        case IntrinsicKind::U32Eq: return Value::make_bool(args[0].as_u32() == args[1].as_u32());
        case IntrinsicKind::U32Ne: return Value::make_bool(args[0].as_u32() != args[1].as_u32());
        case IntrinsicKind::U32Gt: return Value::make_bool(args[0].as_u32() > args[1].as_u32());
        case IntrinsicKind::U32Lt: return Value::make_bool(args[0].as_u32() < args[1].as_u32());
        case IntrinsicKind::U32Ge: return Value::make_bool(args[0].as_u32() >= args[1].as_u32());
        case IntrinsicKind::U32Le: return Value::make_bool(args[0].as_u32() <= args[1].as_u32());

        // u64 arithmetic and compares
        case IntrinsicKind::U64Add: return Value::make_u64(wrap_add(args[0].as_u64(), args[1].as_u64()));
        case IntrinsicKind::U64Sub: return Value::make_u64(wrap_sub(args[0].as_u64(), args[1].as_u64()));
        case IntrinsicKind::U64Mul: return Value::make_u64(wrap_mul(args[0].as_u64(), args[1].as_u64()));
        case IntrinsicKind::U64Div:
        {
            uint64_t b = args[1].as_u64();
            if (b == 0) throw VmError{"division by zero"};
            return Value::make_u64(args[0].as_u64() / b);
        }
        case IntrinsicKind::U64Mod:
        {
            uint64_t b = args[1].as_u64();
            if (b == 0) throw VmError{"division by zero"};
            return Value::make_u64(args[0].as_u64() % b);
        }
        case IntrinsicKind::U64Eq: return Value::make_bool(args[0].as_u64() == args[1].as_u64());
        case IntrinsicKind::U64Ne: return Value::make_bool(args[0].as_u64() != args[1].as_u64());
        case IntrinsicKind::U64Gt: return Value::make_bool(args[0].as_u64() > args[1].as_u64());
        case IntrinsicKind::U64Lt: return Value::make_bool(args[0].as_u64() < args[1].as_u64());
        case IntrinsicKind::U64Ge: return Value::make_bool(args[0].as_u64() >= args[1].as_u64());
        case IntrinsicKind::U64Le: return Value::make_bool(args[0].as_u64() <= args[1].as_u64());

        // f16 widens to float, computes there, and rounds back, which is correctly rounded for every op
        case IntrinsicKind::F16Neg: return Value::make_f16(static_cast<uint16_t>(args[0].as_f16() ^ 0x8000));
        case IntrinsicKind::F16Pos: return Value::make_f16(args[0].as_f16());
        case IntrinsicKind::F16Add: return Value::make_f16(f16_from_double(f16_to_float(args[0].as_f16()) + f16_to_float(args[1].as_f16())));
        case IntrinsicKind::F16Sub: return Value::make_f16(f16_from_double(f16_to_float(args[0].as_f16()) - f16_to_float(args[1].as_f16())));
        case IntrinsicKind::F16Mul: return Value::make_f16(f16_from_double(f16_to_float(args[0].as_f16()) * f16_to_float(args[1].as_f16())));
        case IntrinsicKind::F16Div: return Value::make_f16(f16_from_double(f16_to_float(args[0].as_f16()) / f16_to_float(args[1].as_f16())));
        case IntrinsicKind::F16Mod: return Value::make_f16(f16_from_double(std::fmod(f16_to_float(args[0].as_f16()), f16_to_float(args[1].as_f16()))));
        case IntrinsicKind::F16Eq: return Value::make_bool(f16_to_float(args[0].as_f16()) == f16_to_float(args[1].as_f16()));
        case IntrinsicKind::F16Ne: return Value::make_bool(f16_to_float(args[0].as_f16()) != f16_to_float(args[1].as_f16()));
        case IntrinsicKind::F16Gt: return Value::make_bool(f16_to_float(args[0].as_f16()) > f16_to_float(args[1].as_f16()));
        case IntrinsicKind::F16Lt: return Value::make_bool(f16_to_float(args[0].as_f16()) < f16_to_float(args[1].as_f16()));
        case IntrinsicKind::F16Ge: return Value::make_bool(f16_to_float(args[0].as_f16()) >= f16_to_float(args[1].as_f16()));
        case IntrinsicKind::F16Le: return Value::make_bool(f16_to_float(args[0].as_f16()) <= f16_to_float(args[1].as_f16()));

        // f32 arithmetic and compares. Division by zero follows IEEE, no error.
        case IntrinsicKind::F32Neg: return Value::make_f32(-args[0].as_f32());
        case IntrinsicKind::F32Pos: return Value::make_f32(args[0].as_f32());
        case IntrinsicKind::F32Add: return Value::make_f32(args[0].as_f32() + args[1].as_f32());
        case IntrinsicKind::F32Sub: return Value::make_f32(args[0].as_f32() - args[1].as_f32());
        case IntrinsicKind::F32Mul: return Value::make_f32(args[0].as_f32() * args[1].as_f32());
        case IntrinsicKind::F32Div: return Value::make_f32(args[0].as_f32() / args[1].as_f32());
        case IntrinsicKind::F32Mod: return Value::make_f32(std::fmod(args[0].as_f32(), args[1].as_f32()));
        case IntrinsicKind::F32Eq: return Value::make_bool(args[0].as_f32() == args[1].as_f32());
        case IntrinsicKind::F32Ne: return Value::make_bool(args[0].as_f32() != args[1].as_f32());
        case IntrinsicKind::F32Gt: return Value::make_bool(args[0].as_f32() > args[1].as_f32());
        case IntrinsicKind::F32Lt: return Value::make_bool(args[0].as_f32() < args[1].as_f32());
        case IntrinsicKind::F32Ge: return Value::make_bool(args[0].as_f32() >= args[1].as_f32());
        case IntrinsicKind::F32Le: return Value::make_bool(args[0].as_f32() <= args[1].as_f32());

        // f64 arithmetic and compares. Division by zero follows IEEE, no error.
        case IntrinsicKind::F64Neg: return Value::make_f64(-args[0].as_f64());
        case IntrinsicKind::F64Pos: return Value::make_f64(args[0].as_f64());
        case IntrinsicKind::F64Add: return Value::make_f64(args[0].as_f64() + args[1].as_f64());
        case IntrinsicKind::F64Sub: return Value::make_f64(args[0].as_f64() - args[1].as_f64());
        case IntrinsicKind::F64Mul: return Value::make_f64(args[0].as_f64() * args[1].as_f64());
        case IntrinsicKind::F64Div: return Value::make_f64(args[0].as_f64() / args[1].as_f64());
        case IntrinsicKind::F64Mod: return Value::make_f64(std::fmod(args[0].as_f64(), args[1].as_f64()));
        case IntrinsicKind::F64Eq: return Value::make_bool(args[0].as_f64() == args[1].as_f64());
        case IntrinsicKind::F64Ne: return Value::make_bool(args[0].as_f64() != args[1].as_f64());
        case IntrinsicKind::F64Gt: return Value::make_bool(args[0].as_f64() > args[1].as_f64());
        case IntrinsicKind::F64Lt: return Value::make_bool(args[0].as_f64() < args[1].as_f64());
        case IntrinsicKind::F64Ge: return Value::make_bool(args[0].as_f64() >= args[1].as_f64());
        case IntrinsicKind::F64Le: return Value::make_bool(args[0].as_f64() <= args[1].as_f64());

        // bool
        case IntrinsicKind::BoolNot: return Value::make_bool(!args[0].as_bool());
        case IntrinsicKind::BoolEq: return Value::make_bool(args[0].as_bool() == args[1].as_bool());
        case IntrinsicKind::BoolNe: return Value::make_bool(args[0].as_bool() != args[1].as_bool());

        // c8 compares on the code unit value
        case IntrinsicKind::C8Eq: return Value::make_bool(args[0].as_c8() == args[1].as_c8());
        case IntrinsicKind::C8Ne: return Value::make_bool(args[0].as_c8() != args[1].as_c8());
        case IntrinsicKind::C8Gt: return Value::make_bool(args[0].as_c8() > args[1].as_c8());
        case IntrinsicKind::C8Lt: return Value::make_bool(args[0].as_c8() < args[1].as_c8());
        case IntrinsicKind::C8Ge: return Value::make_bool(args[0].as_c8() >= args[1].as_c8());
        case IntrinsicKind::C8Le: return Value::make_bool(args[0].as_c8() <= args[1].as_c8());

        // string
        case IntrinsicKind::StringEmpty:
        {
            uint64_t addr = memory.alloc(header + 1, stringType);
            memory.write_u32(addr, 0);
            return Value::make_addr(addr);
        }
        case IntrinsicKind::StringInit:
        {
            int32_t length = args[0].as_i32();
            if (length < 0) throw VmError{std::format("negative length {}", length)};
            uint64_t addr = memory.alloc(header + static_cast<uint64_t>(length) + 1, stringType);
            memory.write_u32(addr, static_cast<uint32_t>(length));
            return Value::make_addr(addr);
        }
        case IntrinsicKind::StringEq:
        case IntrinsicKind::StringNe:
        {
            uint64_t a = args[0].as_addr();
            uint64_t b = args[1].as_addr();
            uint32_t la = memory.read_u32(a);
            uint32_t lb = memory.read_u32(b);
            bool equal = la == lb;
            if (equal && la > 0)
                equal = std::memcmp(memory.host_ptr(a + header, la), memory.host_ptr(b + header, la), la) == 0;
            return Value::make_bool(kind == IntrinsicKind::StringEq ? equal : !equal);
        }
        case IntrinsicKind::StringConcat:
        {
            uint64_t a = args[0].as_addr();
            uint64_t b = args[1].as_addr();
            uint32_t la = memory.read_u32(a);
            uint32_t lb = memory.read_u32(b);
            uint64_t total = static_cast<uint64_t>(la) + lb;
            uint64_t addr = memory.alloc(header + total + 1, stringType);
            memory.write_u32(addr, static_cast<uint32_t>(total));
            memory.copy(addr + header, a + header, la);
            memory.copy(addr + header + la, b + header, lb);
            return Value::make_addr(addr);
        }

        // core
        case IntrinsicKind::Panic:
            throw VmError{std::format("panic: {}", read_string(args[0].as_addr()))};

        // array
        case IntrinsicKind::ArrayEmpty:
        {
            uint64_t addr = memory.alloc(header, node->method->parent->as<NamedTypeSymbol>());
            memory.write_u32(addr, 0);
            return Value::make_addr(addr);
        }
        case IntrinsicKind::ArrayInit:
        {
            int32_t length = args[0].as_i32();
            if (length < 0) throw VmError{std::format("negative length {}", length)};
            uint64_t stride = elem_size(array_elem_type(node->method));
            uint64_t addr = memory.alloc(header + static_cast<uint64_t>(length) * stride, node->method->parent->as<NamedTypeSymbol>());
            memory.write_u32(addr, static_cast<uint32_t>(length));
            return Value::make_addr(addr);
        }
        case IntrinsicKind::ArrayCopyTo:
        {
            uint64_t src = self.as_addr();
            uint64_t dest = args[0].as_addr();
            int32_t destIndex = args[1].as_i32();
            int32_t srcIndex = args[2].as_i32();
            int32_t count = args[3].as_i32();
            if (count < 0) throw VmError{std::format("negative length {}", count)};

            uint32_t srcLen = memory.read_u32(src);
            uint32_t destLen = memory.read_u32(dest);
            if (srcIndex < 0 || static_cast<int64_t>(srcIndex) + count > srcLen)
                throw VmError{std::format("index {} out of range (length {})", srcIndex, srcLen)};
            if (destIndex < 0 || static_cast<int64_t>(destIndex) + count > destLen)
                throw VmError{std::format("index {} out of range (length {})", destIndex, destLen)};

            uint64_t stride = elem_size(array_elem_type(node->method));
            memory.copy(dest + header + static_cast<uint64_t>(destIndex) * stride,
                        src + header + static_cast<uint64_t>(srcIndex) * stride,
                        static_cast<uint64_t>(count) * stride);
            return Value{};
        }

        default: break;
    }

    // Literal tags, bool.and, bool.or, array.get, array.set, and string.get never survive lowering.
    throw VmError{std::format("unreachable intrinsic '{}'", format(kind))};
}

}
