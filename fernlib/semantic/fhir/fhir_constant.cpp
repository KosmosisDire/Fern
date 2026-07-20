#include <semantic/fhir/fhir.hpp>
#include <semantic/symbol/symbol.hpp>

#include <climits>
#include <format>

namespace Fern
{

#pragma region Range Check

bool ConstantValue::range_fits(TypeSymbol* target) const
{
    if (kind != Kind::Int) return true;
    auto* named = target ? target->as<NamedTypeSymbol>() : nullptr;
    if (!named) return true;

    if (named->name == "U8")  return intValue >= 0 && intValue <= 255;
    if (named->name == "I32") return intValue >= INT32_MIN && intValue <= INT32_MAX;
    return true;
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

// Both operands share a kind by the time these run. Per type wrapping splits its tag out of a shared case
static std::optional<ConstantValue> fold_binary(IntrinsicKind kind, const ConstantValue& a, const ConstantValue& b)
{
    const bool isInt = a.kind == ConstantValue::Kind::Int;
    const bool isFloat = a.kind == ConstantValue::Kind::Float;
    const bool isBool = a.kind == ConstantValue::Kind::Bool;

    switch (kind)
    {
        case IntrinsicKind::I32Add:
        case IntrinsicKind::U8Add:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_int(a.intValue + b.intValue);
        case IntrinsicKind::I32Sub:
        case IntrinsicKind::U8Sub:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_int(a.intValue - b.intValue);
        case IntrinsicKind::I32Mul:
        case IntrinsicKind::U8Mul:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_int(a.intValue * b.intValue);
        case IntrinsicKind::I32Div:
        case IntrinsicKind::U8Div:
            if (!isInt || b.intValue == 0) return std::nullopt;
            return ConstantValue::make_int(a.intValue / b.intValue);

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

        case IntrinsicKind::I32Gt:
        case IntrinsicKind::U8Gt:
        case IntrinsicKind::CharGt:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_bool(a.intValue > b.intValue);
        case IntrinsicKind::I32Lt:
        case IntrinsicKind::U8Lt:
        case IntrinsicKind::CharLt:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_bool(a.intValue < b.intValue);
        case IntrinsicKind::I32Ge:
        case IntrinsicKind::U8Ge:
        case IntrinsicKind::CharGe:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_bool(a.intValue >= b.intValue);
        case IntrinsicKind::I32Le:
        case IntrinsicKind::U8Le:
        case IntrinsicKind::CharLe:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_bool(a.intValue <= b.intValue);
        case IntrinsicKind::I32Eq:
        case IntrinsicKind::U8Eq:
        case IntrinsicKind::CharEq:
            if (!isInt) return std::nullopt;
            return ConstantValue::make_bool(a.intValue == b.intValue);
        case IntrinsicKind::I32Ne:
        case IntrinsicKind::U8Ne:
        case IntrinsicKind::CharNe:
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

static std::optional<ConstantValue> fold_unary(IntrinsicKind kind, const ConstantValue& a)
{
    switch (kind)
    {
        case IntrinsicKind::I32Neg:
            if (a.kind != ConstantValue::Kind::Int) return std::nullopt;
            return ConstantValue::make_int(-a.intValue);
        case IntrinsicKind::F32Neg:
            if (a.kind != ConstantValue::Kind::Float) return std::nullopt;
            return ConstantValue::make_float(-a.floatValue);
        case IntrinsicKind::I32Pos:
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
    for (auto* arg : args)
    {
        if (!arg || !arg->get_constant())
            return std::nullopt;
    }

    if (args.size() == 1)
    {
        return fold_unary(op, *args[0]->get_constant());
    }

    if (args.size() == 2)
    {
        ConstantValue a = *args[0]->get_constant();
        ConstantValue b = *args[1]->get_constant();

        if (a.kind == ConstantValue::Kind::Int && b.kind == ConstantValue::Kind::Float)
            a = ConstantValue::make_float(static_cast<double>(a.intValue));
        else if (a.kind == ConstantValue::Kind::Float && b.kind == ConstantValue::Kind::Int)
            b = ConstantValue::make_float(static_cast<double>(b.intValue));

        if (a.kind != b.kind) return std::nullopt;

        return fold_binary(op, a, b);
    }

    return std::nullopt;
}

#pragma region Cast

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
        if (inner->kind == ConstantValue::Kind::Int) return *inner;
        if (inner->kind == ConstantValue::Kind::Float)
            return ConstantValue::make_int(static_cast<int64_t>(inner->floatValue));
        return std::nullopt;
    }

    return std::nullopt;
}

}
