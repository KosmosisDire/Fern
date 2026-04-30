#include "binder.hpp"

#include <algorithm>
#include <format>

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

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
                    diag.error(std::format("unknown escape sequence '\\{}'", raw[i + 1]), span);
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
    auto& map = context.symbols.literalSuffixMap;
    auto it = map.find(std::string(suffixName));
    if (it == map.end() || it->second.empty())
    {
        diag.error(std::format("unknown literal suffix '{}'", suffixName), span);
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
        diag.error(std::format("no literal '{}' accepting '{}'",
              suffixName,
              argType ? format_type_name(argType) : "?"), span);
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

    std::string msg = std::format("ambiguous literal suffix '{}', candidates:", suffixName);
    for (auto* method : candidates)
    {
        auto* parent = method->parent ? method->parent->as<NamedTypeSymbol>() : nullptr;
        msg += std::format("\n  {}.{}", parent ? format_type_name(parent) : "?", method->name);
    }
    diag.error(msg, span);
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
            diag.error(constVal->format_range_message(returnType), expr->span);

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
                diag.error("character literal must contain exactly one character", expr->span);
                node->value = ConstantValue::make_int(0);
            }
        }
    }
    catch (const std::out_of_range&)
    {
        diag.error(std::format("literal '{}' is out of range", expr->token.lexeme), expr->span);
    }

    return node;
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
                diag.error("Core.Array has no constructor taking i32", expr->span);
                return fhir.error_expr(expr);
            }

            auto* synthTypeRef = fhir.type_ref(expr, expectedNamed);
            return fhir.construction(expr, expectedNamed, synthTypeRef, ctorResult.best.method, {countLit});
        }

        return nullptr;
    }

    auto* pending = pending_statements();
    int* counter = temp_counter();
    if (!pending || !counter)
    {
        diag.error("array literals are not supported outside of method bodies", expr->span);
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
                diag.error(std::format("array literal has mixed element types: '{}' and '{}'",
                      format_type_name(elementType),
                      format_type_name(elem->type)), expr->span);
            }
        }
    }

    if (!elementType)
    {
        if (!hasErrorElement)
        {
            diag.error("cannot infer element type for array literal", expr->span);
        }
        return fhir.error_expr(expr);
    }

    auto* arrayType = context.symbols.get_or_declare_array_type(elementType);
    if (!arrayType)
    {
        diag.error("Array type not found", expr->span);
        return fhir.error_expr(expr);
    }
    context.symbols.ensure_members_populated(arrayType);

    TypeSymbol* i32Type = context.resolve_type_name("i32");
    int count = static_cast<int>(elements.size());

    auto* countLit = fhir.literal(expr, i32Type);
    countLit->value = ConstantValue::make_int(count);

    std::vector<TypeSymbol*> ctorArgTypes = {i32Type};
    auto ctorResult = arrayType->find_constructor(ctorArgTypes);
    if (!ctorResult.best.method)
    {
        diag.error("Core.Array has no constructor taking i32", expr->span);
        return fhir.error_expr(expr);
    }

    auto* synthTypeRef = fhir.type_ref(expr, arrayType);
    auto* createExpr = fhir.construction(expr, arrayType, synthTypeRef, ctorResult.best.method, {countLit});

    auto tempPtr = std::make_unique<LocalSymbol>();
    tempPtr->name = std::format("__arr_{}", (*counter)++);
    tempPtr->type = arrayType;
    auto* tempLocal = context.symbols.own(std::move(tempPtr));

    pending->push_back(fhir.var_decl(expr, tempLocal, createExpr));

    auto setterResult = arrayType->find_index_setter(i32Type, elementType);
    if (!setterResult.best.is_callable())
    {
        diag.error(std::format("Core.Array has no 'op []=' for element type '{}'",
              format_type_name(elementType)), expr->span);
        return fhir.error_expr(expr);
    }
    MethodSymbol* setter = setterResult.best.method;

    for (int i = 0; i < count; ++i)
    {
        auto* indexLit = fhir.literal(expr, i32Type);
        indexLit->value = ConstantValue::make_int(i);

        auto* setCall = fhir.call(expr, setter->get_return_type(), setter,
                                  {fhir.local_ref(expr, tempLocal), indexLit, elements[i]});
        pending->push_back(fhir.expr_stmt(expr, setCall));
    }

    return fhir.local_ref(expr, tempLocal);
}

}
