#include "binder.hpp"

#include <algorithm>
#include <cassert>
#include <stdexcept>

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

static std::string format_field_path(BaseExprSyntax* expr)
{
    if (!expr) return "";
    if (auto* id = expr->as<IdentifierExprSyntax>())
    {
        return std::string(id->name.lexeme);
    }
    if (auto* member = expr->as<MemberAccessExprSyntax>())
    {
        std::string left = format_field_path(member->left);
        if (left.empty()) return "";
        return left + "." + std::string(member->right.lexeme);
    }
    return "";
}

constexpr IntrinsicOp to_intrinsic_op(BinaryOp op)
{
    switch (op)
    {
        case BinaryOp::Add:          return IntrinsicOp::Add;
        case BinaryOp::Sub:          return IntrinsicOp::Sub;
        case BinaryOp::Mul:          return IntrinsicOp::Mul;
        case BinaryOp::Div:          return IntrinsicOp::Div;
        case BinaryOp::Greater:      return IntrinsicOp::Greater;
        case BinaryOp::Less:         return IntrinsicOp::Less;
        case BinaryOp::GreaterEqual: return IntrinsicOp::GreaterEqual;
        case BinaryOp::LessEqual:    return IntrinsicOp::LessEqual;
        case BinaryOp::Equal:        return IntrinsicOp::Equal;
        case BinaryOp::NotEqual:     return IntrinsicOp::NotEqual;
        case BinaryOp::And:          return IntrinsicOp::And;
        case BinaryOp::Or:           return IntrinsicOp::Or;
    }
}

constexpr IntrinsicOp to_intrinsic_op(UnaryOp op)
{
    switch (op)
    {
        case UnaryOp::Negative: return IntrinsicOp::Negative;
        case UnaryOp::Positive: return IntrinsicOp::Positive;
        case UnaryOp::Not:      return IntrinsicOp::Not;
    }
}

#pragma region Expression Binding

FhirExpr* Binder::bind_expr(BaseExprSyntax* expr, TypeSymbol* expected)
{
    if (!expr) return nullptr;

    FhirExpr* result = nullptr;

    if (auto* lit = expr->as<LiteralExprSyntax>())
        result = bind_literal(lit);
    else if (auto* id = expr->as<IdentifierExprSyntax>())
        result = bind_identifier(id);
    else if (auto* thisExpr = expr->as<ThisExprSyntax>())
        result = bind_this(thisExpr);
    else if (auto* unary = expr->as<UnaryExprSyntax>())
        result = bind_unary(unary);
    else if (auto* bin = expr->as<BinaryExprSyntax>())
        result = bind_binary(bin);
    else if (auto* assign = expr->as<AssignmentExprSyntax>())
        result = bind_assignment(assign);
    else if (auto* call = expr->as<CallExprSyntax>())
        result = bind_call(call);
    else if (auto* member = expr->as<MemberAccessExprSyntax>())
        result = bind_member_access(member);
    else if (auto* initializer = expr->as<InitializerExprSyntax>())
        result = bind_initializer(initializer);
    else if (auto* paren = expr->as<ParenExprSyntax>())
        result = bind_paren(paren, expected);
    else if (auto* generic = expr->as<GenericTypeExprSyntax>())
        result = bind_generic_type_expr(generic);
    else if (auto* indexExpr = expr->as<IndexExprSyntax>())
        result = bind_index(indexExpr);
    else if (auto* arrayLit = expr->as<ArrayLiteralExprSyntax>())
        result = bind_array_literal(arrayLit, expected);
    else if (auto* suffixExpr = expr->as<LiteralSuffixExprSyntax>())
        result = bind_suffixed_literal(suffixExpr, expected);

    if (result && result->is_error()) return result;

    if (expected && result && result->type)
    {
        if (result->type == expected)
        {
            const auto& constant = result->get_constant();
            if (constant && !constant->range_fits(expected))
            {
                error(constant->format_range_message(expected), expr->span);
                return fhir.error_expr(expr, expected, result);
            }
            return result;
        }

        if (auto* castResult = try_implicit_cast(result, expected, expr->span))
        {
            return castResult;
        }

        auto* expectedNamed = expected->as<NamedTypeSymbol>();
        auto* resultNamed = result->type->as<NamedTypeSymbol>();
        if (expectedNamed && expectedNamed->is_integer() &&
            resultNamed && resultNamed->is_integer())
        {
            const auto& constant = result->get_constant();
            if (constant)
            {
                if (constant->range_fits(expected))
                    return fhir.cast(result->syntax, expected, result, /*isImplicit=*/true);

                error(constant->format_range_message(expected), expr->span);
                return fhir.error_expr(expr, expected, result);
            }
        }

        error("expected '" + format_type_name(expected) +
              "', got '" + format_type_name(result->type) + "'", expr->span);
        return fhir.error_expr(expr, expected, result);
    }

    return result;
}

FhirCastExpr* Binder::try_implicit_cast(FhirExpr* expr, TypeSymbol* targetType, const Span& span)
{
    if (!expr || !expr->type || !targetType) return nullptr;

    auto conv = NamedTypeSymbol::get_convertibility(expr->type, targetType);
    if (conv == Convertibility::Implicit)
        return fhir.cast(expr->syntax, targetType, expr, true);

    return nullptr;
}

FhirExpr* Binder::coerce_to_param(FhirExpr* arg, TypeSymbol* paramType)
{
    if (!arg || !paramType || arg->type == paramType) return arg;
    if (auto* cast = try_implicit_cast(arg, paramType, Span{}))
        return cast;
    return arg;
}

FhirExpr* Binder::bind_value_expr(BaseExprSyntax* expr, TypeSymbol* expected)
{
    FhirExpr* result = bind_expr(expr, expected);
    if (result) return result;
    if (!expr) return nullptr;

    if (auto* id = expr->as<IdentifierExprSyntax>())
    {
        Symbol* sym = resolve_name(id->name.lexeme);
        if (sym)
        {
            if (sym->kind == SymbolKind::Type)
                error("'" + std::string(id->name.lexeme) + "' is a type, not a value", expr->span);
            else if (sym->kind == SymbolKind::Namespace)
                error("'" + std::string(id->name.lexeme) + "' is a namespace, not a value", expr->span);
            else if (sym->kind == SymbolKind::Method)
                error("'" + std::string(id->name.lexeme) + "' is a method, not a value", expr->span);
            return fhir.error_expr(expr);
        }
    }

    return nullptr;
}

std::string Binder::process_escape_sequences(std::string_view raw, const Span& span)
{
    std::string result;
    result.reserve(raw.size());

    for (size_t i = 0; i < raw.size(); ++i)
    {
        if (raw[i] == '\\' && i + 1 < raw.size())
        {
            switch (raw[i + 1])
            {
                case 'n':  result += '\n'; ++i; break;
                case 'r':  result += '\r'; ++i; break;
                case 't':  result += '\t'; ++i; break;
                case '\\': result += '\\'; ++i; break;
                case '"':  result += '"';  ++i; break;
                case '`':  result += '`';  ++i; break;
                case '0':  result += '\0'; ++i; break;
                default:
                    error("unknown escape sequence '\\" + std::string(1, raw[i + 1]) + "'", span);
                    result += raw[i + 1];
                    ++i;
                    break;
            }
        }
        else
        {
            result += raw[i];
        }
    }

    return result;
}

MethodSymbol* Binder::resolve_literal_suffix(std::string_view suffixName, TypeSymbol* argType, TypeSymbol* expected, const Span& span)
{
    auto it = literalSuffixMap.find(std::string(suffixName));
    if (it == literalSuffixMap.end() || it->second.empty())
    {
        error("unknown literal suffix '" + std::string(suffixName) + "'", span);
        return nullptr;
    }

    std::vector<MethodSymbol*> candidates;
    for (auto* method : it->second)
    {
        if (method->parameters.size() == 1 && method->parameters[0]->type == argType)
            candidates.push_back(method);
    }

    if (candidates.empty())
    {
        error("no literal '" + std::string(suffixName) + "' accepting '"
              + (argType ? format_type_name(argType) : "?") + "'", span);
        return nullptr;
    }

    if (candidates.size() == 1)
        return candidates[0];

    if (expected)
    {
        for (auto* method : candidates)
        {
            if (method->get_return_type() == expected)
                return method;
        }
    }

    std::string msg = "ambiguous literal suffix '" + std::string(suffixName) + "', candidates:";
    for (auto* method : candidates)
    {
        auto* parent = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
        msg += "\n  " + (parent ? format_type_name(parent) : "?") + "." + method->name;
    }
    error(msg, span);
    return candidates[0];
}

FhirExpr* Binder::bind_suffixed_literal(LiteralSuffixExprSyntax* expr, TypeSymbol* expected)
{
    auto* operand = bind_value_expr(expr->operand);
    if (!operand) return nullptr;

    auto* method = resolve_literal_suffix(expr->suffix.lexeme, operand->type, expected, expr->suffix.span);
    if (!method)
        return operand;

    auto* returnType = method->get_return_type();
    auto* namedReturnType = returnType ? returnType->as<NamedTypeSymbol>() : nullptr;

    if (namedReturnType && namedReturnType->is_builtin())
    {
        const auto& constVal = operand->get_constant();
        if (constVal && !constVal->range_fits(returnType))
            error(constVal->format_range_message(returnType), expr->span);

        operand->type = returnType;
        return operand;
    }

    return fhir.call(expr, returnType, method, {operand});
}

FhirExpr* Binder::bind_literal(LiteralExprSyntax* expr)
{
    TypeSymbol* type = nullptr;

    switch (expr->token.kind)
    {
        case TokenKind::LiteralInt:    type = context.resolve_type_name("i32");    break;
        case TokenKind::LiteralFloat:  type = context.resolve_type_name("f32");    break;
        case TokenKind::LiteralBool:   type = context.resolve_type_name("bool");   break;
        case TokenKind::LiteralString:
        case TokenKind::LiteralMultilineString:
        case TokenKind::LiteralRawString:
        case TokenKind::LiteralRawMultilineString: type = context.resolve_type_name("string"); break;
        case TokenKind::LiteralChar: type = context.resolve_type_name("char"); break;
        default: break;
    }

    auto* node = fhir.literal(expr, type);

    try
    {
        if (expr->token.kind == TokenKind::LiteralInt)
            node->value = ConstantValue::make_int(std::stoll(std::string(expr->token.lexeme)));
        else if (expr->token.kind == TokenKind::LiteralFloat)
            node->value = ConstantValue::make_float(std::stod(std::string(expr->token.lexeme)));
        else if (expr->token.kind == TokenKind::LiteralBool)
            node->value = ConstantValue::make_bool(expr->token.lexeme == "true");
        else if (expr->token.kind == TokenKind::LiteralString)
        {
            auto raw = expr->token.lexeme.substr(1, expr->token.lexeme.size() - 2);
            if (raw.find('\\') == std::string_view::npos)
                node->value = ConstantValue::make_string(raw);
            else
                node->value = ConstantValue::make_string(arena.alloc_string(process_escape_sequences(raw, expr->span)));
        }
        else if (expr->token.kind == TokenKind::LiteralMultilineString)
        {
            auto raw = expr->token.lexeme.substr(3, expr->token.lexeme.size() - 6);

            if (!raw.empty() && raw[0] == '\n')
                raw = raw.substr(1);
            else if (raw.size() >= 2 && raw[0] == '\r' && raw[1] == '\n')
                raw = raw.substr(2);

            if (!raw.empty() && raw.back() == '\n')
                raw = raw.substr(0, raw.size() - 1);
            if (!raw.empty() && raw.back() == '\r')
                raw = raw.substr(0, raw.size() - 1);

            size_t minIndent = std::string_view::npos;
            size_t lineStart = 0;
            for (size_t i = 0; i <= raw.size(); ++i)
            {
                if (i == raw.size() || raw[i] == '\n')
                {
                    auto line = raw.substr(lineStart, i - lineStart);
                    if (!line.empty())
                    {
                        size_t indent = 0;
                        while (indent < line.size() && line[indent] == ' ')
                            ++indent;
                        if (indent < line.size())
                            minIndent = std::min(minIndent, indent);
                    }
                    lineStart = i + 1;
                }
            }

            if (minIndent == std::string_view::npos)
                minIndent = 0;

            std::string result;
            lineStart = 0;
            for (size_t i = 0; i <= raw.size(); ++i)
            {
                if (i == raw.size() || raw[i] == '\n')
                {
                    auto line = raw.substr(lineStart, i - lineStart);
                    if (!result.empty())
                        result += '\n';
                    if (line.size() > minIndent)
                        result += line.substr(minIndent);
                    lineStart = i + 1;
                }
            }

            auto processed = process_escape_sequences(result, expr->span);
            node->value = ConstantValue::make_string(arena.alloc_string(processed));
        }
        else if (expr->token.kind == TokenKind::LiteralRawString)
            node->value = ConstantValue::make_string(expr->token.lexeme.substr(1, expr->token.lexeme.size() - 2));
        else if (expr->token.kind == TokenKind::LiteralRawMultilineString)
            node->value = ConstantValue::make_string(expr->token.lexeme.substr(3, expr->token.lexeme.size() - 6));
        else if (expr->token.kind == TokenKind::LiteralChar)
        {
            auto inner = expr->token.lexeme.substr(1, expr->token.lexeme.size() - 2);
            auto processed = process_escape_sequences(inner, expr->span);
            if (processed.size() == 1)
                node->value = ConstantValue::make_int(static_cast<uint8_t>(processed[0]));
            else
            {
                error("character literal must contain exactly one character", expr->span);
                node->value = ConstantValue::make_int(0);
            }
        }
    }
    catch (const std::out_of_range&)
    {
        error("literal '" + std::string(expr->token.lexeme) + "' is out of range", expr->span);
    }

    return node;
}

FhirExpr* Binder::bind_identifier(IdentifierExprSyntax* expr)
{
    Symbol* symbol = resolve_name(expr->name.lexeme);
    if (!symbol)
    {
        error("undefined name '" + std::string(expr->name.lexeme) + "'", expr->span);
        return fhir.error_expr(expr);
    }

    // A null type on a value symbol means its type failed to resolve upstream
    // (e.g. unresolved type annotation, or inferred from an error expression).
    // That error was already reported, so we propagate poison to prevent cascades.
    switch (symbol->kind)
    {
        case SymbolKind::Local:
        {
            auto* local = symbol->as<LocalSymbol>();
            if (!local->type) return fhir.error_expr(expr);
            return fhir.local_ref(expr, local);
        }
        case SymbolKind::Parameter:
        {
            auto* param = symbol->as<ParameterSymbol>();
            if (!param->type) return fhir.error_expr(expr);
            return fhir.param_ref(expr, param);
        }
        case SymbolKind::Field:
        {
            auto* fieldSym = symbol->as<FieldSymbol>();
            if (!fieldSym->type) return fhir.error_expr(expr);
            auto* thisType = symbol->parent ? symbol->parent->as<TypeSymbol>() : nullptr;
            return fhir.field_access(expr, fhir.this_expr(expr, thisType), fieldSym);
        }
        case SymbolKind::Type:
        case SymbolKind::Namespace:
        case SymbolKind::Method:
        default:
            return nullptr;
    }
}

FhirExpr* Binder::bind_this(ThisExprSyntax* expr)
{
    if (!currentType)
    {
        error("'this' can only be used inside a type", expr->span);
        return fhir.error_expr(expr);
    }

    return fhir.this_expr(expr, currentType);
}

FhirExpr* Binder::bind_paren(ParenExprSyntax* expr, TypeSymbol* expected)
{
    return bind_value_expr(expr->expression, expected);
}

FhirExpr* Binder::bind_generic_type_expr(GenericTypeExprSyntax* expr)
{
    resolve_generic_type(expr);
    return nullptr;
}

FhirExpr* Binder::bind_unary(UnaryExprSyntax* expr)
{
    FhirExpr* operand = bind_value_expr(expr->operand);
    if (operand && operand->is_error()) return fhir.error_expr(expr);

    TypeSymbol* operandType = operand ? operand->type : nullptr;

    auto* namedType = operandType ? operandType->as<NamedTypeSymbol>() : nullptr;
    if (namedType)
    {
        TokenKind opToken = unary_op_to_token(expr->op);
        auto result = namedType->find_unary_operator(opToken, operandType);
        if (result.ambiguous)
        {
            std::string msg = "operator '" + std::string(Fern::format(opToken)) +
                  "' is ambiguous for value of type '" + format_type_name(namedType) + "':";
            for (auto* m : result.ambiguousCandidates)
                msg += "\n  op " + std::string(Fern::format(opToken)) + "(" + m->format_parameters() + ")";
            error(msg, expr->span);
            return fhir.error_expr(expr);
        }
        if (result.best.is_callable())
        {
            MethodSymbol* method = result.best.method;
            operand = coerce_to_param(operand, method->parameters[0]->type);
            if (!namedType->is_builtin())
            {
                return fhir.call(expr, method->get_return_type(), method, {operand});
            }
            return fhir.intrinsic(expr, method->get_return_type(), to_intrinsic_op(expr->op), {operand});
        }

        error("operator '" + std::string(Fern::format(opToken)) +
              "' cannot be applied to value of type '" + format_type_name(namedType) + "'", expr->span);
        return fhir.error_expr(expr);
    }

    return fhir.intrinsic(expr, operandType, to_intrinsic_op(expr->op), {operand});
}

FhirExpr* Binder::bind_binary(BinaryExprSyntax* expr)
{
    FhirExpr* lhs = bind_value_expr(expr->left);
    FhirExpr* rhs = bind_value_expr(expr->right);
    return bind_binary_op(expr->op, lhs, rhs, expr);
}

FhirExpr* Binder::try_synthesize_compound_comparison(
    BinaryOp op, TokenKind opToken, NamedTypeSymbol* namedType,
    TypeSymbol* leftType, TypeSymbol* rightType,
    FhirExpr* lhs, FhirExpr* rhs, BaseExprSyntax* syntax)
{
    // For compound comparison operators (>=, <=, !=), try to synthesize them
    // from their base operators: >= from > and ==, <= from < and ==, != from ==
    TokenKind baseToken = TokenKind::Invalid;
    if (opToken == TokenKind::GreaterEqual)
    {
        baseToken = TokenKind::Greater;
    }
    else if (opToken == TokenKind::LessEqual)
    {
        baseToken = TokenKind::Less;
    }
    else if (opToken == TokenKind::NotEqual)
    {
        baseToken = TokenKind::Equal;
    }

    if (baseToken != TokenKind::Invalid)
    {
        auto baseResult = namedType->find_binary_operator(baseToken, leftType, rightType);
        bool hasBase = baseResult.best.is_callable();
        bool hasEqual = (baseToken == TokenKind::Equal) ||
                        namedType->find_binary_operator(TokenKind::Equal, leftType, rightType).best.is_callable();

        if (hasBase && hasEqual)
        {
            MethodSymbol* baseMethod = baseResult.best.method;
            lhs = coerce_to_param(lhs, baseMethod->parameters[0]->type);
            rhs = coerce_to_param(rhs, baseMethod->parameters[1]->type);
            TypeSymbol* boolType = context.resolve_type_name("bool");
            return fhir.intrinsic(syntax, boolType, to_intrinsic_op(op), {lhs, rhs});
        }

        std::string leftName = format_type_name(leftType);
        std::string rightName = format_type_name(rightType);
        std::string msg = "operator '" + std::string(Fern::format(opToken)) +
              "' cannot be applied to values of type '" + leftName + "' and '" + rightName + "'";

        if (!hasBase && !hasEqual)
        {
            msg += " (requires '" + std::string(Fern::format(baseToken)) + "' and '==' operators)";
        }
        else if (!hasBase)
        {
            msg += " (requires '" + std::string(Fern::format(baseToken)) + "' operator)";
        }
        else
        {
            msg += " (requires '==' operator)";
        }

        error(msg, syntax->span);
        return fhir.error_expr(syntax);
    }

    std::string leftName = format_type_name(leftType);
    std::string rightName = format_type_name(rightType);
    error("operator '" + std::string(Fern::format(opToken)) +
          "' cannot be applied to values of type '" + leftName + "' and '" + rightName + "'", syntax->span);
    return fhir.error_expr(syntax);
}

FhirExpr* Binder::bind_binary_op(BinaryOp op, FhirExpr* lhs, FhirExpr* rhs, BaseExprSyntax* syntax)
{
    bool lhsError = lhs && lhs->is_error();
    bool rhsError = rhs && rhs->is_error();
    if (lhsError || rhsError) return fhir.error_expr(syntax);

    TypeSymbol* leftType = lhs ? lhs->type : nullptr;
    TypeSymbol* rightType = rhs ? rhs->type : nullptr;

    TokenKind opToken = binary_op_to_token(op);

    auto* namedType = leftType ? leftType->as<NamedTypeSymbol>() : nullptr;
    if (!namedType)
    {
        return fhir.intrinsic(syntax, leftType, to_intrinsic_op(op), {lhs, rhs});
    }

    auto result = namedType->find_binary_operator(opToken, leftType, rightType);
    if (result.ambiguous)
    {
        std::string msg = "operator '" + std::string(Fern::format(opToken)) +
              "' is ambiguous for values of type '" + format_type_name(leftType) +
              "' and '" + format_type_name(rightType) + "':";
        for (auto* m : result.ambiguousCandidates)
            msg += "\n  op " + std::string(Fern::format(opToken)) + "(" + m->format_parameters() + ")";
        error(msg, syntax->span);
        return fhir.error_expr(syntax);
    }
    if (result.best.is_callable())
    {
        MethodSymbol* method = result.best.method;
        lhs = coerce_to_param(lhs, method->parameters[0]->type);
        rhs = coerce_to_param(rhs, method->parameters[1]->type);
        if (!namedType->is_builtin())
        {
            return fhir.call(syntax, method->get_return_type(), method, {lhs, rhs});
        }
        return fhir.intrinsic(syntax, method->get_return_type(), to_intrinsic_op(op), {lhs, rhs});
    }

    return try_synthesize_compound_comparison(op, opToken, namedType, leftType, rightType, lhs, rhs, syntax);
}

FhirExpr* Binder::bind_assignment(AssignmentExprSyntax* expr)
{
    if (auto* indexExpr = expr->target->as<IndexExprSyntax>())
    {
        FhirExpr* object = bind_value_expr(indexExpr->object);
        FhirExpr* index = bind_value_expr(indexExpr->index);
        FhirExpr* value = bind_value_expr(expr->value);

        if ((object && object->is_error()) || (index && index->is_error()) || (value && value->is_error()))
        {
            return fhir.error_expr(expr);
        }

        if (expr->op != AssignOp::Simple)
        {
            FhirExpr* readTarget = bind_index(indexExpr);
            BinaryOp binOp = BinaryOp::Add;
            switch (expr->op)
            {
                case AssignOp::Add: binOp = BinaryOp::Add; break;
                case AssignOp::Sub: binOp = BinaryOp::Sub; break;
                case AssignOp::Mul: binOp = BinaryOp::Mul; break;
                case AssignOp::Div: binOp = BinaryOp::Div; break;
                default: break;
            }
            value = bind_binary_op(binOp, readTarget, value, expr);
        }

        TypeSymbol* objectType = object ? object->type : nullptr;
        TypeSymbol* indexType = index ? index->type : nullptr;
        TypeSymbol* valueType = value ? value->type : nullptr;

        auto* namedType = objectType ? objectType->as<NamedTypeSymbol>() : nullptr;
        if (!namedType)
        {
            error("cannot index a value of type '" + format_type_name(objectType) + "'", expr->span);
            return fhir.error_expr(expr);
        }

        auto setterResult = namedType->find_index_setter(indexType, valueType);
        if (setterResult.ambiguous)
        {
            std::string msg = "index assignment is ambiguous between:";
            for (auto* m : setterResult.ambiguousCandidates)
                msg += "\n  " + format_type_name(namedType) + ".op []=(" + m->format_parameters() + ")";
            error(msg, expr->span);
            return fhir.error_expr(expr);
        }
        if (!setterResult.best.is_callable())
        {
            error("type '" + format_type_name(namedType) +
                  "' has no 'op []=' for index type '" + format_type_name(indexType) +
                  "' and value type '" + format_type_name(valueType) + "'", expr->span);
            return fhir.error_expr(expr);
        }

        MethodSymbol* method = setterResult.best.method;
        index = coerce_to_param(index, method->parameters[1]->type);
        value = coerce_to_param(value, method->parameters[2]->type);
        return fhir.call(expr, method->get_return_type(), method, {object, index, value});
    }

    FhirExpr* writeTarget = bind_value_expr(expr->target);
    TypeSymbol* targetType = writeTarget ? writeTarget->type : nullptr;
    FhirExpr* value = bind_value_expr(expr->value, targetType);

    // Compound assignments (+=, -=, etc.) desugar to target = target op value,
    // binding the target twice: once for reading the current value, once for the write
    if (expr->op != AssignOp::Simple)
    {
        FhirExpr* readTarget = bind_value_expr(expr->target);

        BinaryOp binOp = BinaryOp::Add;
        switch (expr->op)
        {
            case AssignOp::Add: binOp = BinaryOp::Add; break;
            case AssignOp::Sub: binOp = BinaryOp::Sub; break;
            case AssignOp::Mul: binOp = BinaryOp::Mul; break;
            case AssignOp::Div: binOp = BinaryOp::Div; break;
            default: break;
        }
        value = bind_binary_op(binOp, readTarget, value, expr);
    }

    return fhir.assign(expr, writeTarget, value);
}

FhirExpr* Binder::bind_member_access(MemberAccessExprSyntax* expr)
{
    std::string_view memberName = expr->right.lexeme;
    if (memberName.empty())
    {
        return fhir.error_expr(expr);
    }

    Symbol* leftSym = resolve_expr_symbol(expr->left);

    if (auto* ns = leftSym ? leftSym->as<NamespaceSymbol>() : nullptr)
    {
        Symbol* member = ns->find_member(memberName);
        if (member)
        {
            return nullptr;
        }

        error("namespace '" + ns->name + "' has no member '" +
              std::string(memberName) + "'", expr->span);
        return fhir.error_expr(expr);
    }

    if (auto* typeRef = leftSym ? leftSym->as<NamedTypeSymbol>() : nullptr)
    {
        if (auto* nested = typeRef->find_nested_type(memberName))
        {
            return nullptr;
        }

        if (auto* field = typeRef->find_field(memberName))
        {
            if (!has_modifier(field->modifiers, Modifier::Static))
            {
                error("cannot access instance field '" + std::string(memberName) +
                      "' on type '" + format_type_name(typeRef) + "'", expr->span);
                return fhir.error_expr(expr, field->type);
            }
            return fhir.field_access(expr, nullptr, field);
        }

        if (auto* method = typeRef->find_method(memberName))
        {
            if (!has_modifier(method->modifiers, Modifier::Static))
            {
                error("cannot access instance method '" + std::string(memberName) +
                      "' on type '" + format_type_name(typeRef) + "'", expr->span);
                return fhir.error_expr(expr);
            }
            return nullptr;
        }

        error("type '" + format_type_name(typeRef) + "' has no member '" +
              std::string(memberName) + "'", expr->span);
        return fhir.error_expr(expr);
    }

    FhirExpr* left = bind_expr(expr->left);
    if (left && left->is_error())
    {
        return fhir.error_expr(expr);
    }

    TypeSymbol* leftType = left ? left->type : nullptr;
    if (!leftType)
    {
        if (leftSym)
        {
            error("'" + leftSym->name + "' is not a namespace or type", expr->span);
        }
        return fhir.error_expr(expr);
    }

    auto* namedType = leftType->as<NamedTypeSymbol>();
    if (!namedType)
    {
        return fhir.error_expr(expr);
    }

    if (auto* field = namedType->find_field(memberName))
    {
        return fhir.field_access(expr, left, field);
    }

    if (auto* method = namedType->find_method(memberName))
    {
        return nullptr;
    }

    error("type '" + format_type_name(namedType) + "' has no member '" +
          std::string(memberName) + "'", expr->span);
    return fhir.error_expr(expr);
}

FhirExpr* Binder::bind_call(CallExprSyntax* expr)
{
    Symbol* calleeSym = resolve_expr_symbol(expr->callee);

    std::vector<FhirExpr*> argExprs;
    std::vector<TypeSymbol*> argTypes;
    bool hasErrorArg = false;
    for (auto* arg : expr->arguments)
    {
        FhirExpr* bound = bind_value_expr(arg);
        argExprs.push_back(bound);
        if (bound && bound->is_error())
        {
            hasErrorArg = true;
            argTypes.push_back(nullptr);
        }
        else
        {
            argTypes.push_back(bound ? bound->type : nullptr);
        }
    }

    if (calleeSym && calleeSym->kind == SymbolKind::Type)
    {
        auto* namedType = calleeSym->as<NamedTypeSymbol>();
        if (namedType)
        {
            auto result = namedType->find_constructor(argTypes);
            if (result.ambiguous && !hasErrorArg)
            {
                std::string msg = "call is ambiguous between constructors:";
                for (auto* m : result.ambiguousCandidates)
                    msg += "\n  " + format_type_name(namedType) + "(" + m->format_parameters() + ")";
                error(msg, expr->span);
                return fhir.error_expr(expr);
            }
            if (!result.best.method && !hasErrorArg)
            {
                error("'" + format_type_name(namedType) + "' does not contain a constructor that takes " +
                      std::to_string(argTypes.size()) + (argTypes.size() == 1 ? " argument" : " arguments"), expr->span);
            }
            if (result.best.method && !hasErrorArg)
            {
                for (size_t i = 0; i < expr->arguments.size(); ++i)
                {
                    argExprs[i] = bind_value_expr(expr->arguments[i], result.best.method->parameters[i]->type);
                }
            }

            return fhir.object_create(expr, namedType, result.best.method, std::move(argExprs));
        }
    }

    MethodSymbol* method = nullptr;
    NamedTypeSymbol* targetType = nullptr;
    std::string_view methodName;
    FhirExpr* receiver = nullptr;

    if (auto* idExpr = expr->callee->as<IdentifierExprSyntax>())
    {
        if (calleeSym && calleeSym->kind == SymbolKind::Method)
        {
            targetType = calleeSym->parent ? calleeSym->parent->as<NamedTypeSymbol>() : nullptr;
            methodName = idExpr->name.lexeme;
        }
        else if (calleeSym)
        {
            error("'" + std::string(idExpr->name.lexeme) + "' cannot be called as a function", idExpr->span);
            return fhir.error_expr(expr);
        }
    }
    else if (auto* memberExpr = expr->callee->as<MemberAccessExprSyntax>())
    {
        if (calleeSym && calleeSym->kind == SymbolKind::Method)
        {
            targetType = calleeSym->parent ? calleeSym->parent->as<NamedTypeSymbol>() : nullptr;
            methodName = memberExpr->right.lexeme;
            receiver = bind_expr(memberExpr->left);
        }
        else
        {
            receiver = bind_expr(memberExpr->left);
            if (receiver && receiver->is_error())
            {
                return fhir.error_expr(expr);
            }
            TypeSymbol* leftType = receiver ? receiver->type : nullptr;
            auto* namedLeft = leftType ? leftType->as<NamedTypeSymbol>() : nullptr;
            if (namedLeft)
            {
                targetType = namedLeft;
                methodName = memberExpr->right.lexeme;
            }
            else if (calleeSym)
            {
                error("'" + std::string(memberExpr->right.lexeme) + "' cannot be called as a function", memberExpr->span);
                return fhir.error_expr(expr);
            }
        }
    }

    if (targetType && !methodName.empty())
    {
        auto result = targetType->find_method(methodName, argTypes);
        if (result.ambiguous && !hasErrorArg)
        {
            std::string msg = "call to '" + std::string(methodName) + "' is ambiguous between:";
            for (auto* m : result.ambiguousCandidates)
                msg += "\n  " + format_type_name(targetType) + "." + std::string(methodName) + "(" + m->format_parameters() + ")";
            error(msg, expr->span);
            return fhir.error_expr(expr);
        }
        method = result.best.method;
        if (!method)
        {
            if (!hasErrorArg)
            {
                error("'" + format_type_name(targetType) + "' does not contain a method '" + std::string(methodName) +
                      "' that takes " + std::to_string(argTypes.size()) +
                      (argTypes.size() == 1 ? " argument" : " arguments"), expr->span);
            }
            return fhir.error_expr(expr);
        }
        if (!hasErrorArg)
        {
            for (size_t i = 0; i < expr->arguments.size(); ++i)
            {
                argExprs[i] = bind_value_expr(expr->arguments[i], method->parameters[i]->type);
            }
        }
    }

    if (!method)
    {
        if (!calleeSym)
        {
            FhirExpr* calleeExpr = bind_value_expr(expr->callee);
            if (calleeExpr && !calleeExpr->is_error())
            {
                error("expression cannot be called as a function", expr->callee->span);
            }
        }
        return fhir.error_expr(expr);
    }

    TypeSymbol* returnType = method->get_return_type();

    if (method->is_constructor())
    {
        return fhir.object_create(expr, returnType, method, std::move(argExprs));
    }

    if (receiver && !has_modifier(method->modifiers, Modifier::Static))
    {
        return fhir.method_call(expr, returnType, receiver, method, std::move(argExprs));
    }

    return fhir.call(expr, returnType, method, std::move(argExprs));
}

#pragma region Initializer Binding

FhirExpr* Binder::bind_initializer_target(InitializerExprSyntax* expr)
{
    if (!expr->target) return nullptr;

    if (auto* call = expr->target->as<CallExprSyntax>())
        return bind_call(call);

    Symbol* sym = resolve_expr_symbol(expr->target);
    auto* namedType = sym ? sym->as<NamedTypeSymbol>() : nullptr;
    if (namedType)
    {
        auto ctorResult = namedType->find_constructor({});
        if (ctorResult.best.method)
        {
            return fhir.object_create(expr->target, namedType, ctorResult.best.method, {});
        }
    }

    return bind_value_expr(expr->target);
}

FhirExpr* Binder::build_field_access_chain(FhirExpr* receiver, BaseExprSyntax* target)
{
    if (auto* id = target->as<IdentifierExprSyntax>())
    {
        TypeSymbol* recvType = receiver ? receiver->type : nullptr;
        auto* namedRecv = recvType ? recvType->as<NamedTypeSymbol>() : nullptr;
        auto* field = namedRecv ? namedRecv->find_field(id->name.lexeme) : nullptr;
        return fhir.field_access(id, receiver, field);
    }

    if (auto* member = target->as<MemberAccessExprSyntax>())
    {
        FhirExpr* left = build_field_access_chain(receiver, member->left);
        TypeSymbol* leftType = left ? left->type : nullptr;
        auto* namedLeft = leftType ? leftType->as<NamedTypeSymbol>() : nullptr;
        auto* field = namedLeft ? namedLeft->find_field(member->right.lexeme) : nullptr;
        return fhir.field_access(member, left, field);
    }

    return nullptr;
}

FhirExpr* Binder::bind_initializer(InitializerExprSyntax* expr)
{
    if (!expr->target)
    {
        error("cannot infer type for initializer list, use an explicit type name", expr->span);
        for (auto* member : expr->members)
        {
            if (auto* fieldInit = member->as<FieldInitSyntax>())
            {
                bind_value_expr(fieldInit->value);
            }
            else if (auto* childStmt = member->as<ExpressionStmtSyntax>())
            {
                bind_value_expr(childStmt->expression);
            }
        }
        return fhir.error_expr(expr);
    }

    NamedTypeSymbol* namedType = nullptr;

    if (auto* idExpr = expr->target->as<IdentifierExprSyntax>())
    {
        Symbol* sym = resolve_name(idExpr->name.lexeme);
        if (!sym)
        {
            error("undefined name '" + std::string(idExpr->name.lexeme) + "'", idExpr->span);
            return fhir.error_expr(expr);
        }

        namedType = sym->as<NamedTypeSymbol>();
        if (!namedType)
        {
            error("initializer lists can only be applied to a type or constructor call", idExpr->span);
            return fhir.error_expr(expr);
        }

        if (!namedType->find_constructor({}).best.method)
        {
            error("type '" + format_type_name(namedType) + "' has no default constructor", idExpr->span);
        }
    }
    else if (auto* callExpr = expr->target->as<CallExprSyntax>())
    {
        FhirExpr* callResult = bind_call(callExpr);
        if (callResult && callResult->is_error())
        {
            return fhir.error_expr(expr);
        }
        TypeSymbol* callType = callResult ? callResult->type : nullptr;
        namedType = callType ? callType->as<NamedTypeSymbol>() : nullptr;

        if (namedType)
        {
            Symbol* calleeSym = resolve_expr_symbol(callExpr->callee);
            if (calleeSym)
            {
                auto* method = calleeSym->as<MethodSymbol>();
                if (method && !method->is_constructor())
                {
                    error("initializer lists can only be applied to a type or constructor call", callExpr->span);
                    namedType = nullptr;
                }
            }
        }
    }
    else if (auto* memberExpr = expr->target->as<MemberAccessExprSyntax>())
    {
        Symbol* sym = resolve_expr_symbol(memberExpr);
        if (!sym)
        {
            error("undefined name '" + std::string(memberExpr->right.lexeme) + "'", memberExpr->span);
            return fhir.error_expr(expr);
        }

        namedType = sym->as<NamedTypeSymbol>();
        if (!namedType)
        {
            error("initializer lists can only be applied to a type or constructor call", memberExpr->span);
            return fhir.error_expr(expr);
        }

        if (!namedType->find_constructor({}).best.method)
        {
            error("type '" + format_type_name(namedType) + "' has no default constructor", memberExpr->span);
        }
    }
    else if (auto* genericExpr = expr->target->as<GenericTypeExprSyntax>())
    {
        TypeSymbol* type = resolve_generic_type(genericExpr);
        namedType = type ? type->as<NamedTypeSymbol>() : nullptr;
        if (!namedType)
        {
            return fhir.error_expr(expr);
        }

        if (!namedType->find_constructor({}).best.method)
        {
            error("type '" + format_type_name(namedType) + "' has no default constructor", genericExpr->span);
        }
    }
    else
    {
        error("initializer lists can only be applied to a type or constructor call", expr->target->span);
        return fhir.error_expr(expr);
    }

    if (!namedType)
    {
        for (auto* member : expr->members)
        {
            if (auto* fieldInit = member->as<FieldInitSyntax>())
            {
                bind_value_expr(fieldInit->value);
            }
            else if (auto* childStmt = member->as<ExpressionStmtSyntax>())
            {
                bind_value_expr(childStmt->expression);
            }
        }
        return fhir.error_expr(expr);
    }

    if (expr->members.empty())
    {
        return bind_initializer_target(expr);
    }

    // Initializer lists with field assignments are lowered to:
    //   var __init_N = Constructor(...)   // injected into the enclosing block
    //   __init_N.field1 = value1          // injected into the enclosing block
    //   __init_N.field2 = value2          // injected into the enclosing block
    //   ... expression result is __init_N
    if (!pendingStmts)
    {
        error("initializer lists are not yet supported outside of method bodies", expr->span);
        return fhir.error_expr(expr);
    }

    auto tempPtr = std::make_unique<LocalSymbol>();
    tempPtr->name = "__init_" + std::to_string(tempCounter++);
    tempPtr->type = namedType;
    auto* tempLocal = context.symbols.own(std::move(tempPtr));

    pendingStmts->push_back(fhir.var_decl(expr, tempLocal, bind_initializer_target(expr)));

    bind_initializer_fields(expr, namedType, *pendingStmts, fhir.local_ref(expr, tempLocal));

    return fhir.local_ref(expr, tempLocal);
}

void Binder::bind_initializer_fields(InitializerExprSyntax* expr, NamedTypeSymbol* namedType, std::vector<FhirStmt*>& out, FhirExpr* receiver)
{
    std::vector<std::string> boundFieldPaths;
    for (auto* member : expr->members)
    {
        auto* fieldInit = member->as<FieldInitSyntax>();
        if (!fieldInit)
        {
            if (auto* childStmt = member->as<ExpressionStmtSyntax>())
            {
                bind_value_expr(childStmt->expression);
            }
            continue;
        }

        TypeSymbol* fieldType = nullptr;
        if (fieldInit->target &&
            (fieldInit->target->is<IdentifierExprSyntax>() ||
             fieldInit->target->is<MemberAccessExprSyntax>()))
        {
            fieldType = bind_field_init_target(fieldInit->target, namedType);
        }

        if (fieldInit->value)
        {
            if (receiver)
            {
                FhirExpr* fieldTarget = build_field_access_chain(receiver, fieldInit->target);
                out.push_back(fhir.expr_stmt(fieldInit, fhir.assign(fieldInit, fieldTarget, bind_value_expr(fieldInit->value, fieldType))));
            }
            else
            {
                bind_value_expr(fieldInit->value, fieldType);
            }
        }
    }

    for (auto* member : expr->members)
    {
        auto* fieldInit = member->as<FieldInitSyntax>();
        if (!fieldInit) continue;

        std::string path = format_field_path(fieldInit->target);
        if (path.empty()) continue;

        auto it = std::find(boundFieldPaths.begin(), boundFieldPaths.end(), path);
        if (it != boundFieldPaths.end())
        {
            error("duplicate field '" + path + "' in initializer", fieldInit->target->span);
        }
        else
        {
            boundFieldPaths.push_back(std::move(path));
        }
    }
}

TypeSymbol* Binder::bind_field_init_target(BaseExprSyntax* target, NamedTypeSymbol* type)
{
    if (auto* id = target->as<IdentifierExprSyntax>())
    {
        FieldSymbol* field = type->find_field(id->name.lexeme);
        if (!field)
        {
            error("type '" + format_type_name(type) + "' has no field named '" +
                  std::string(id->name.lexeme) + "'", target->span);
            return nullptr;
        }
        return field->type;
    }

    if (auto* member = target->as<MemberAccessExprSyntax>())
    {
        TypeSymbol* leftType = bind_field_init_target(member->left, type);
        if (!leftType)
        {
            return nullptr;
        }

        auto* nestedType = leftType->as<NamedTypeSymbol>();
        if (!nestedType)
        {
            error("cannot access member on non-struct type", member->span);
            return nullptr;
        }

        FieldSymbol* field = nestedType->find_field(member->right.lexeme);
        if (!field)
        {
            error("type '" + format_type_name(nestedType) + "' has no field named '" +
                  std::string(member->right.lexeme) + "'", member->span);
            return nullptr;
        }
        return field->type;
    }

    error("initializer target must be a field name or member access", target->span);
    return nullptr;
}

FhirExpr* Binder::bind_array_literal(ArrayLiteralExprSyntax* expr, TypeSymbol* expected)
{
    if (expr->elements.empty())
    {
        auto* expectedNamed = expected ? expected->as<NamedTypeSymbol>() : nullptr;
        if (expectedNamed && expectedNamed->genericOrigin &&
            expectedNamed->genericOrigin->name == "Array" &&
            !expectedNamed->typeArguments.empty())
        {
            TypeSymbol* elementType = expectedNamed->typeArguments[0];
            TypeSymbol* i32Type = context.resolve_type_name("i32");

            auto* countLit = fhir.literal(expr, i32Type);
            countLit->value = ConstantValue::make_int(0);

            std::vector<TypeSymbol*> ctorArgTypes = {i32Type};
            auto ctorResult = expectedNamed->find_constructor(ctorArgTypes);
            if (!ctorResult.best.method)
            {
                error("Core.Array has no constructor taking i32", expr->span);
                return fhir.error_expr(expr);
            }

            return fhir.object_create(expr, expectedNamed, ctorResult.best.method, {countLit});
        }

        return nullptr;
    }

    if (!pendingStmts)
    {
        error("array literals are not supported outside of method bodies", expr->span);
        return fhir.error_expr(expr);
    }

    TypeSymbol* expectedElementType = nullptr;
    auto* expectedNamed = expected ? expected->as<NamedTypeSymbol>() : nullptr;
    if (expectedNamed && expectedNamed->genericOrigin &&
        expectedNamed->genericOrigin->name == "Array" &&
        !expectedNamed->typeArguments.empty())
    {
        expectedElementType = expectedNamed->typeArguments[0];
    }

    std::vector<FhirExpr*> elements;
    for (auto* elem : expr->elements)
    {
        elements.push_back(bind_value_expr(elem, expectedElementType));
    }

    bool hasErrorElement = std::any_of(elements.begin(), elements.end(),
        [](auto* e) { return e && e->is_error(); });

    TypeSymbol* elementType = expectedElementType;
    if (!elementType)
    {
        for (auto* elem : elements)
        {
            if (!elem || !elem->type) continue;
            if (elem->is_error()) continue;
            if (!elementType)
            {
                elementType = elem->type;
            }
            else if (elem->type != elementType)
            {
                error("array literal has mixed element types: '" +
                      format_type_name(elementType) + "' and '" +
                      format_type_name(elem->type) + "'", expr->span);
            }
        }
    }

    if (!elementType)
    {
        if (!hasErrorElement)
        {
            error("cannot infer element type for array literal", expr->span);
        }
        return fhir.error_expr(expr);
    }

    auto* coreNs = context.symbols.globalNamespace->find_namespace("Core");
    auto* arrayTemplate = coreNs ? coreNs->find_type("Array", 1) : nullptr;
    if (!arrayTemplate)
    {
        error("Core.Array type not found", expr->span);
        return fhir.error_expr(expr);
    }

    auto* arrayType = context.symbols.get_or_create_instantiation(arrayTemplate, {elementType});
    context.symbols.ensure_members_populated(arrayType);

    TypeSymbol* i32Type = context.resolve_type_name("i32");
    int count = static_cast<int>(elements.size());

    auto* countLit = fhir.literal(expr, i32Type);
    countLit->value = ConstantValue::make_int(count);

    std::vector<TypeSymbol*> ctorArgTypes = {i32Type};
    auto ctorResult = arrayType->find_constructor(ctorArgTypes);
    if (!ctorResult.best.method)
    {
        error("Core.Array has no constructor taking i32", expr->span);
        return fhir.error_expr(expr);
    }

    auto* createExpr = fhir.object_create(expr, arrayType, ctorResult.best.method, {countLit});

    auto tempPtr = std::make_unique<LocalSymbol>();
    tempPtr->name = "__arr_" + std::to_string(tempCounter++);
    tempPtr->type = arrayType;
    auto* tempLocal = context.symbols.own(std::move(tempPtr));

    pendingStmts->push_back(fhir.var_decl(expr, tempLocal, createExpr));

    auto setterResult = arrayType->find_index_setter(i32Type, elementType);
    if (!setterResult.best.is_callable())
    {
        error("Core.Array has no 'op []=' for element type '" +
              format_type_name(elementType) + "'", expr->span);
        return fhir.error_expr(expr);
    }
    MethodSymbol* setter = setterResult.best.method;

    for (int i = 0; i < count; ++i)
    {
        auto* indexLit = fhir.literal(expr, i32Type);
        indexLit->value = ConstantValue::make_int(i);

        auto* setCall = fhir.call(expr, setter->get_return_type(), setter,
                                  {fhir.local_ref(expr, tempLocal), indexLit, elements[i]});
        pendingStmts->push_back(fhir.expr_stmt(expr, setCall));
    }

    return fhir.local_ref(expr, tempLocal);
}

FhirExpr* Binder::bind_index(IndexExprSyntax* expr)
{
    FhirExpr* object = bind_value_expr(expr->object);
    FhirExpr* index = bind_value_expr(expr->index);

    if ((object && object->is_error()) || (index && index->is_error()))
    {
        return fhir.error_expr(expr);
    }

    TypeSymbol* objectType = object ? object->type : nullptr;
    TypeSymbol* indexType = index ? index->type : nullptr;

    auto* namedType = objectType ? objectType->as<NamedTypeSymbol>() : nullptr;
    if (!namedType)
    {
        error("cannot index a value of type '" + format_type_name(objectType) + "'", expr->span);
        return fhir.error_expr(expr);
    }

    auto getterResult = namedType->find_index_getter(indexType);
    if (getterResult.ambiguous)
    {
        std::string msg = "index access is ambiguous between:";
        for (auto* m : getterResult.ambiguousCandidates)
            msg += "\n  " + format_type_name(namedType) + ".op [](" + m->format_parameters() + ")";
        error(msg, expr->span);
        return fhir.error_expr(expr);
    }
    if (!getterResult.best.is_callable())
    {
        error("type '" + format_type_name(namedType) +
              "' has no 'op []' for index type '" + format_type_name(indexType) + "'", expr->span);
        return fhir.error_expr(expr);
    }

    MethodSymbol* method = getterResult.best.method;
    index = coerce_to_param(index, method->parameters[1]->type);
    return fhir.call(expr, method->get_return_type(), method, {object, index});
}

}
