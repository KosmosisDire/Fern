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

std::string ConstantValue::format_range_message(TypeSymbol* target) const
{
    auto* named = target ? target->as<NamedTypeSymbol>() : nullptr;
    if (named && named->name == "U8")
        return std::format("value {} is out of range for u8 (0-255)", intValue);
    if (named && named->name == "I32")
        return std::format("value {} is out of range for i32 (-2147483648 to 2147483647)", intValue);
    return std::format("value {} is out of range for {}", intValue, format_type_name(target));
}

#pragma region Lazy Dispatch

const std::optional<ConstantValue>& FhirExpr::get_constant() const
{
    if (constantComputed) return constantCache;
    constantComputed = true;

    if (auto* lit = as<FhirLiteralExpr>())
        constantCache = lit->compute_constant();
    else if (auto* intr = as<FhirIntrinsicExpr>())
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

static std::optional<ConstantValue> eval_int_binary(IntrinsicOp op, int64_t a, int64_t b)
{
    switch (op)
    {
        case IntrinsicOp::Add:          return ConstantValue::make_int(a + b);
        case IntrinsicOp::Sub:          return ConstantValue::make_int(a - b);
        case IntrinsicOp::Mul:          return ConstantValue::make_int(a * b);
        case IntrinsicOp::Div:
            if (b == 0) return std::nullopt;
            return ConstantValue::make_int(a / b);
        case IntrinsicOp::Greater:      return ConstantValue::make_bool(a > b);
        case IntrinsicOp::Less:         return ConstantValue::make_bool(a < b);
        case IntrinsicOp::GreaterEqual: return ConstantValue::make_bool(a >= b);
        case IntrinsicOp::LessEqual:    return ConstantValue::make_bool(a <= b);
        case IntrinsicOp::Equal:        return ConstantValue::make_bool(a == b);
        case IntrinsicOp::NotEqual:     return ConstantValue::make_bool(a != b);
        default:                        return std::nullopt;
    }
}

static std::optional<ConstantValue> eval_float_binary(IntrinsicOp op, double a, double b)
{
    switch (op)
    {
        case IntrinsicOp::Add:          return ConstantValue::make_float(a + b);
        case IntrinsicOp::Sub:          return ConstantValue::make_float(a - b);
        case IntrinsicOp::Mul:          return ConstantValue::make_float(a * b);
        case IntrinsicOp::Div:
            if (b == 0.0) return std::nullopt;
            return ConstantValue::make_float(a / b);
        case IntrinsicOp::Greater:      return ConstantValue::make_bool(a > b);
        case IntrinsicOp::Less:         return ConstantValue::make_bool(a < b);
        case IntrinsicOp::GreaterEqual: return ConstantValue::make_bool(a >= b);
        case IntrinsicOp::LessEqual:    return ConstantValue::make_bool(a <= b);
        case IntrinsicOp::Equal:        return ConstantValue::make_bool(a == b);
        case IntrinsicOp::NotEqual:     return ConstantValue::make_bool(a != b);
        default:                        return std::nullopt;
    }
}

static std::optional<ConstantValue> eval_bool_binary(IntrinsicOp op, bool a, bool b)
{
    switch (op)
    {
        case IntrinsicOp::And:      return ConstantValue::make_bool(a && b);
        case IntrinsicOp::Or:       return ConstantValue::make_bool(a || b);
        case IntrinsicOp::Equal:    return ConstantValue::make_bool(a == b);
        case IntrinsicOp::NotEqual: return ConstantValue::make_bool(a != b);
        default:                    return std::nullopt;
    }
}

#pragma region Intrinsic

std::optional<ConstantValue> FhirIntrinsicExpr::compute_constant() const
{
    for (auto* arg : args)
    {
        if (!arg || !arg->get_constant())
            return std::nullopt;
    }

    if (args.size() == 1)
    {
        const ConstantValue& a = *args[0]->get_constant();
        switch (op)
        {
            case IntrinsicOp::Negative:
                if (a.kind == ConstantValue::Kind::Int)   return ConstantValue::make_int(-a.intValue);
                if (a.kind == ConstantValue::Kind::Float) return ConstantValue::make_float(-a.floatValue);
                return std::nullopt;
            case IntrinsicOp::Positive:
                if (a.kind == ConstantValue::Kind::Int ||
                    a.kind == ConstantValue::Kind::Float) return a;
                return std::nullopt;
            case IntrinsicOp::Not:
                if (a.kind == ConstantValue::Kind::Bool) return ConstantValue::make_bool(!a.boolValue);
                return std::nullopt;
            default:
                return std::nullopt;
        }
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

        switch (a.kind)
        {
            case ConstantValue::Kind::Int:   return eval_int_binary(op, a.intValue, b.intValue);
            case ConstantValue::Kind::Float: return eval_float_binary(op, a.floatValue, b.floatValue);
            case ConstantValue::Kind::Bool:  return eval_bool_binary(op, a.boolValue, b.boolValue);
            default:                         return std::nullopt;
        }
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
