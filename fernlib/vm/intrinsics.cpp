#include <vm/vm.hpp>

#include <cstdint>
#include <cstring>
#include <format>

#include <semantic/symbol/symbol.hpp>

namespace Fern
{

#pragma region Numeric Helpers

// Integer add, sub, mul, and neg wrap two's complement
static int32_t wrap_add(int32_t a, int32_t b) { return static_cast<int32_t>(static_cast<uint32_t>(a) + static_cast<uint32_t>(b)); }
static int32_t wrap_sub(int32_t a, int32_t b) { return static_cast<int32_t>(static_cast<uint32_t>(a) - static_cast<uint32_t>(b)); }
static int32_t wrap_mul(int32_t a, int32_t b) { return static_cast<int32_t>(static_cast<uint32_t>(a) * static_cast<uint32_t>(b)); }
static int32_t wrap_neg(int32_t a) { return static_cast<int32_t>(0u - static_cast<uint32_t>(a)); }

static uint8_t wrap_add(uint8_t a, uint8_t b) { return static_cast<uint8_t>(static_cast<unsigned>(a) + b); }
static uint8_t wrap_sub(uint8_t a, uint8_t b) { return static_cast<uint8_t>(static_cast<unsigned>(a) - b); }
static uint8_t wrap_mul(uint8_t a, uint8_t b) { return static_cast<uint8_t>(static_cast<unsigned>(a) * b); }

// Float to int saturates, matching llvm.fptosi.sat and the wasm saturating conversions. NaN becomes 0.
static int32_t saturate_i32(float f)
{
    if (f != f) return 0;
    if (f >= 2147483648.0f) return INT32_MAX;
    if (f < -2147483648.0f) return INT32_MIN;
    return static_cast<int32_t>(f);
}

static uint8_t saturate_u8(float f)
{
    if (f != f) return 0;
    if (f <= 0.0f) return 0;
    if (f >= 255.0f) return 255;
    return static_cast<uint8_t>(f);
}

#pragma region Conversions

Value Interpreter::convert(IntrinsicKind kind, Value operand)
{
    switch (kind)
    {
        case IntrinsicKind::I32FromU8:   return Value::make_i32(static_cast<int32_t>(operand.as_u8()));
        case IntrinsicKind::I32FromF32:  return Value::make_i32(saturate_i32(operand.as_f32()));
        case IntrinsicKind::I32FromBool: return Value::make_i32(operand.as_bool() ? 1 : 0);
        case IntrinsicKind::F32FromI32:  return Value::make_f32(static_cast<float>(operand.as_i32()));
        case IntrinsicKind::F32FromU8:   return Value::make_f32(static_cast<float>(operand.as_u8()));
        case IntrinsicKind::U8FromI32:   return Value::make_u8(static_cast<uint8_t>(static_cast<uint32_t>(operand.as_i32())));
        case IntrinsicKind::U8FromF32:   return Value::make_u8(saturate_u8(operand.as_f32()));
        case IntrinsicKind::BoolFromI32: return Value::make_bool(operand.as_i32() != 0);
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
        case IntrinsicKind::I32Eq: return Value::make_bool(args[0].as_i32() == args[1].as_i32());
        case IntrinsicKind::I32Ne: return Value::make_bool(args[0].as_i32() != args[1].as_i32());
        case IntrinsicKind::I32Gt: return Value::make_bool(args[0].as_i32() > args[1].as_i32());
        case IntrinsicKind::I32Lt: return Value::make_bool(args[0].as_i32() < args[1].as_i32());
        case IntrinsicKind::I32Ge: return Value::make_bool(args[0].as_i32() >= args[1].as_i32());
        case IntrinsicKind::I32Le: return Value::make_bool(args[0].as_i32() <= args[1].as_i32());

        // f32 arithmetic and compares. Division by zero follows IEEE, no error.
        case IntrinsicKind::F32Neg: return Value::make_f32(-args[0].as_f32());
        case IntrinsicKind::F32Pos: return Value::make_f32(args[0].as_f32());
        case IntrinsicKind::F32Add: return Value::make_f32(args[0].as_f32() + args[1].as_f32());
        case IntrinsicKind::F32Sub: return Value::make_f32(args[0].as_f32() - args[1].as_f32());
        case IntrinsicKind::F32Mul: return Value::make_f32(args[0].as_f32() * args[1].as_f32());
        case IntrinsicKind::F32Div: return Value::make_f32(args[0].as_f32() / args[1].as_f32());
        case IntrinsicKind::F32Eq: return Value::make_bool(args[0].as_f32() == args[1].as_f32());
        case IntrinsicKind::F32Ne: return Value::make_bool(args[0].as_f32() != args[1].as_f32());
        case IntrinsicKind::F32Gt: return Value::make_bool(args[0].as_f32() > args[1].as_f32());
        case IntrinsicKind::F32Lt: return Value::make_bool(args[0].as_f32() < args[1].as_f32());
        case IntrinsicKind::F32Ge: return Value::make_bool(args[0].as_f32() >= args[1].as_f32());
        case IntrinsicKind::F32Le: return Value::make_bool(args[0].as_f32() <= args[1].as_f32());

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
        case IntrinsicKind::U8Eq: return Value::make_bool(args[0].as_u8() == args[1].as_u8());
        case IntrinsicKind::U8Ne: return Value::make_bool(args[0].as_u8() != args[1].as_u8());
        case IntrinsicKind::U8Gt: return Value::make_bool(args[0].as_u8() > args[1].as_u8());
        case IntrinsicKind::U8Lt: return Value::make_bool(args[0].as_u8() < args[1].as_u8());
        case IntrinsicKind::U8Ge: return Value::make_bool(args[0].as_u8() >= args[1].as_u8());
        case IntrinsicKind::U8Le: return Value::make_bool(args[0].as_u8() <= args[1].as_u8());

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
