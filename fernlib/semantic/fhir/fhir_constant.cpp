#include <semantic/fhir/fhir.hpp>
#include <semantic/symbol/symbol.hpp>

#include <climits>
#include <cmath>
#include <format>

namespace Fern
{

#pragma region Range Check

bool ConstantValue::range_fits(TypeSymbol* target) const
{
    if (kind != Kind::Int) return true;
    auto* named = target ? target->as<NamedTypeSymbol>() : nullptr;
    if (!named) return true;

    std::optional<IntRange> range = named->integer_range();
    if (!range) return true;

    return intValue >= range->min && intValue <= range->max;
}

#pragma region Lazy Dispatch

const std::optional<ConstantValue>& FhirExpr::get_constant() const
{
    if (constantComputed) return constantCache;
    constantComputed = true;

    if (auto* lit = as<FhirLiteralExpr>())
        constantCache = lit->compute_constant();
    else if (auto* intr = as<FhirOpExpr>())
        constantCache = intr->compute_constant();
    else if (auto* cast = as<FhirCastExpr>())
        constantCache = cast->compute_constant();

    return constantCache;
}

#pragma region Literal

std::optional<ConstantValue> FhirLiteralExpr::compute_constant() const
{
    return value;
}

#pragma region Intrinsic Evaluators

// Folding computes in 64 bits, so a result past that ceiling is out of range for every integer type
static bool add_overflows(int64_t a, int64_t b)
{
    int64_t r = static_cast<int64_t>(static_cast<uint64_t>(a) + static_cast<uint64_t>(b));
    return ((a ^ r) & (b ^ r)) < 0;
}

static bool sub_overflows(int64_t a, int64_t b)
{
    int64_t r = static_cast<int64_t>(static_cast<uint64_t>(a) - static_cast<uint64_t>(b));
    return ((a ^ b) & (a ^ r)) < 0;
}

static bool mul_overflows(int64_t a, int64_t b)
{
    if (a == 0 || b == 0) return false;
    if (a == -1) return b == INT64_MIN;
    if (b == -1) return a == INT64_MIN;
    if (a > 0) return b > 0 ? a > INT64_MAX / b : b < INT64_MIN / a;
    return b > 0 ? a < INT64_MIN / b : a < INT64_MAX / b;
}

// The lowest value of a type has no positive counterpart, so dividing it by minus one has no result in
// range. The 64 bit fallback keeps the division defined when the type has no range to check against
static bool divide_overflows(int64_t a, int64_t b, const std::optional<IntRange>& range)
{
    return b == -1 && a == (range ? range->min : INT64_MIN);
}

// Both operands share a kind by the time these run. The caller checks the result against the op type,
// so these only report what cannot be computed in 64 bits at all
static std::optional<ConstantValue> fold_binary(IntrinsicKind kind, const ConstantValue& a, const ConstantValue& b,
                                                const std::optional<IntRange>& range, bool& overflowed)
{
    const bool isInt = a.kind == ConstantValue::Kind::Int;
    const bool isFloat = a.kind == ConstantValue::Kind::Float;
    const bool isBool = a.kind == ConstantValue::Kind::Bool;

    switch (kind)
    {
        case IntrinsicKind::I32Add:
        case IntrinsicKind::I64Add:
        case IntrinsicKind::U8Add:
            if (!isInt) return std::nullopt;
            if (add_overflows(a.intValue, b.intValue)) { overflowed = true; return std::nullopt; }
            return ConstantValue::make_int(a.intValue + b.intValue);
        case IntrinsicKind::I32Sub:
        case IntrinsicKind::I64Sub:
        case IntrinsicKind::U8Sub:
            if (!isInt) return std::nullopt;
            if (sub_overflows(a.intValue, b.intValue)) { overflowed = true; return std::nullopt; }
            return ConstantValue::make_int(a.intValue - b.intValue);
        case IntrinsicKind::I32Mul:
        case IntrinsicKind::I64Mul:
        case IntrinsicKind::U8Mul:
            if (!isInt) return std::nullopt;
            if (mul_overflows(a.intValue, b.intValue)) { overflowed = true; return std::nullopt; }
            return ConstantValue::make_int(a.intValue * b.intValue);
        case IntrinsicKind::I32Div:
        case IntrinsicKind::I64Div:
        case IntrinsicKind::U8Div:
            if (!isInt || b.intValue == 0) return std::nullopt;
            if (divide_overflows(a.intValue, b.intValue, range)) { overflowed = true; return std::nullopt; }
            return ConstantValue::make_int(a.intValue / b.intValue);
        case IntrinsicKind::I32Mod:
        case IntrinsicKind::I64Mod:
        case IntrinsicKind::U8Mod:
            if (!isInt || b.intValue == 0) return std::nullopt;
            if (divide_overflows(a.intValue, b.intValue, range)) { overflowed = true; return std::nullopt; }
            return ConstantValue::make_int(a.intValue % b.intValue);

        case IntrinsicKind::F32Add:
            if (!isFloat) return std::nullopt;
            return ConstantValue::make_float(a.floatValue + b.floatValue);
        case IntrinsicKind::F32Sub:
            if (!isFloat) return std::nullopt;
            return ConstantValue::make_float(a.floatValue - b.floatValue);
        case IntrinsicKind::F32Mul:
            if (!isFloat) return std::nullopt;
            return ConstantValue::make_float(a.floatValue * b.floatValue);
        case IntrinsicKind::F32Div:
            if (!isFloat || b.floatValue == 0.0) return std::nullopt;
            return ConstantValue::make_float(a.floatValue / b.floatValue);
        case IntrinsicKind::F32Mod:
            if (!isFloat || b.floatValue == 0.0) return std::nullopt;
            return ConstantValue::make_float(std::fmod(a.floatValue, b.floatValue));

        case IntrinsicKind::I32Gt:
        case IntrinsicKind::I64Gt:
        case IntrinsicKind::U8Gt:
        case IntrinsicKind::C8Gt:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_bool(a.intValue > b.intValue);
        case IntrinsicKind::I32Lt:
        case IntrinsicKind::I64Lt:
        case IntrinsicKind::U8Lt:
        case IntrinsicKind::C8Lt:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_bool(a.intValue < b.intValue);
        case IntrinsicKind::I32Ge:
        case IntrinsicKind::I64Ge:
        case IntrinsicKind::U8Ge:
        case IntrinsicKind::C8Ge:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_bool(a.intValue >= b.intValue);
        case IntrinsicKind::I32Le:
        case IntrinsicKind::I64Le:
        case IntrinsicKind::U8Le:
        case IntrinsicKind::C8Le:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_bool(a.intValue <= b.intValue);
        case IntrinsicKind::I32Eq:
        case IntrinsicKind::I64Eq:
        case IntrinsicKind::U8Eq:
        case IntrinsicKind::C8Eq:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_bool(a.intValue == b.intValue);
        case IntrinsicKind::I32Ne:
        case IntrinsicKind::I64Ne:
        case IntrinsicKind::U8Ne:
        case IntrinsicKind::C8Ne:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_bool(a.intValue != b.intValue);

        case IntrinsicKind::F32Gt:
            if (!isFloat) return std::nullopt;
            return ConstantValue::make_bool(a.floatValue > b.floatValue);
        case IntrinsicKind::F32Lt:
            if (!isFloat) return std::nullopt;
            return ConstantValue::make_bool(a.floatValue < b.floatValue);
        case IntrinsicKind::F32Ge:
            if (!isFloat) return std::nullopt;
            return ConstantValue::make_bool(a.floatValue >= b.floatValue);
        case IntrinsicKind::F32Le:
            if (!isFloat) return std::nullopt;
            return ConstantValue::make_bool(a.floatValue <= b.floatValue);
        case IntrinsicKind::F32Eq:
            if (!isFloat) return std::nullopt;
            return ConstantValue::make_bool(a.floatValue == b.floatValue);
        case IntrinsicKind::F32Ne:
            if (!isFloat) return std::nullopt;
            return ConstantValue::make_bool(a.floatValue != b.floatValue);

        case IntrinsicKind::BoolAnd:
            if (!isBool) return std::nullopt;
            return ConstantValue::make_bool(a.boolValue && b.boolValue);
        case IntrinsicKind::BoolOr:
            if (!isBool) return std::nullopt;
            return ConstantValue::make_bool(a.boolValue || b.boolValue);
        case IntrinsicKind::BoolEq:
            if (!isBool) return std::nullopt;
            return ConstantValue::make_bool(a.boolValue == b.boolValue);
        case IntrinsicKind::BoolNe:
            if (!isBool) return std::nullopt;
            return ConstantValue::make_bool(a.boolValue != b.boolValue);

        default:
            return std::nullopt;
    }
}

static std::optional<ConstantValue> fold_unary(IntrinsicKind kind, const ConstantValue& a, bool& overflowed)
{
    switch (kind)
    {
        case IntrinsicKind::I32Neg:
        case IntrinsicKind::I64Neg:
            if (a.kind != ConstantValue::Kind::Int) return std::nullopt;
            if (a.intValue == INT64_MIN) { overflowed = true; return std::nullopt; }
            return ConstantValue::make_int(-a.intValue);
        case IntrinsicKind::F32Neg:
            if (a.kind != ConstantValue::Kind::Float) return std::nullopt;
            return ConstantValue::make_float(-a.floatValue);
        case IntrinsicKind::I32Pos:
        case IntrinsicKind::I64Pos:
            if (a.kind != ConstantValue::Kind::Int) return std::nullopt;
            return a;
        case IntrinsicKind::F32Pos:
            if (a.kind != ConstantValue::Kind::Float) return std::nullopt;
            return a;
        case IntrinsicKind::BoolNot:
            if (a.kind != ConstantValue::Kind::Bool) return std::nullopt;
            return ConstantValue::make_bool(!a.boolValue);
        default:
            return std::nullopt;
    }
}

#pragma region Intrinsic

std::optional<ConstantValue> FhirOpExpr::compute_constant() const
{
    constantOverflowed = false;

    for (auto* arg : args)
    {
        if (!arg || !arg->get_constant())
            return std::nullopt;
    }

    std::optional<ConstantValue> folded;

    if (args.size() == 1)
    {
        folded = fold_unary(op, *args[0]->get_constant(), constantOverflowed);
    }
    else if (args.size() == 2)
    {
        ConstantValue a = *args[0]->get_constant();
        ConstantValue b = *args[1]->get_constant();

        if (a.kind == ConstantValue::Kind::Int && b.kind == ConstantValue::Kind::Float)
            a = ConstantValue::make_float(static_cast<double>(a.intValue));
        else if (a.kind == ConstantValue::Kind::Float && b.kind == ConstantValue::Kind::Int)
            b = ConstantValue::make_float(static_cast<double>(b.intValue));

        if (a.kind != b.kind) return std::nullopt;

        auto* named = type ? type->as<NamedTypeSymbol>() : nullptr;
        std::optional<IntRange> range = named ? named->integer_range() : std::nullopt;
        folded = fold_binary(op, a, b, range, constantOverflowed);
    }
    else
    {
        return std::nullopt;
    }

    // The fold is declined on overflow so no value outside the op type can reach a backend
    if (folded && !folded->range_fits(type))
    {
        constantOverflowed = true;
        return std::nullopt;
    }

    return folded;
}

// Forces the fold so the flag is current. The binder reports from this, lazy callers only read the cache
bool FhirOpExpr::constant_overflows() const
{
    get_constant();
    return constantOverflowed;
}

#pragma region Cast

// Narrowing keeps the low bits and rereads the sign there, which is what the runtime conversion does
static std::optional<ConstantValue> wrap_to_int(int64_t value, NamedTypeSymbol* target)
{
    std::optional<IntRange> range = target->integer_range();
    std::optional<int> size = target->builtin_scalar_size();
    if (!range || !size) return std::nullopt;

    int bits = *size * 8;
    if (bits >= 64) return ConstantValue::make_int(value);

    uint64_t mask = (1ull << bits) - 1;
    uint64_t kept = static_cast<uint64_t>(value) & mask;
    bool negative = range->min < 0 && (kept & (1ull << (bits - 1))) != 0;

    return ConstantValue::make_int(static_cast<int64_t>(negative ? kept | ~mask : kept));
}

// Float to int saturates in every backend, so the fold clamps instead of casting out of range
static std::optional<ConstantValue> saturate_to_int(double value, NamedTypeSymbol* target)
{
    std::optional<IntRange> range = target->integer_range();
    if (!range) return std::nullopt;

    if (std::isnan(value)) return ConstantValue::make_int(0);
    if (value <= static_cast<double>(range->min)) return ConstantValue::make_int(range->min);
    if (value >= static_cast<double>(range->max)) return ConstantValue::make_int(range->max);

    return ConstantValue::make_int(static_cast<int64_t>(value));
}

std::optional<ConstantValue> FhirCastExpr::compute_constant() const
{
    if (!operand) return std::nullopt;
    const auto& inner = operand->get_constant();
    if (!inner) return std::nullopt;

    auto* targetNamed = type ? type->as<NamedTypeSymbol>() : nullptr;
    if (!targetNamed) return std::nullopt;

    if (targetNamed->is_float())
    {
        if (inner->kind == ConstantValue::Kind::Float) return *inner;
        if (inner->kind == ConstantValue::Kind::Int)
            return ConstantValue::make_float(static_cast<double>(inner->intValue));
        return std::nullopt;
    }

    if (targetNamed->is_integer())
    {
        if (inner->kind == ConstantValue::Kind::Int) return wrap_to_int(inner->intValue, targetNamed);
        if (inner->kind == ConstantValue::Kind::Float) return saturate_to_int(inner->floatValue, targetNamed);
        return std::nullopt;
    }

    return std::nullopt;
}

}
