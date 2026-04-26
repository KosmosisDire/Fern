#include "binder.hpp"

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>

namespace Fern
{

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
    else if (auto* castExpr = expr->as<CastExprSyntax>())
        result = bind_cast(castExpr);
    else if (auto* genName = expr->as<GenericNameExprSyntax>())
        result = bind_generic_name_expr(genName);
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
                diag.error(constant->format_range_message(expected), expr->span);
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

                diag.error(constant->format_range_message(expected), expr->span);
                return fhir.error_expr(expr, expected, result);
            }
        }

        diag.error("expected '" + format_type_name(expected) +
              "', got '" + format_type_name(result->type) + "'", expr->span);
        return fhir.error_expr(expr, expected, result);
    }

    return result;
}

FhirCastExpr* Binder::try_implicit_cast(FhirExpr* expr, TypeSymbol* targetType, const Span& span)
{
    if (!expr || !expr->type || !targetType) return nullptr;

    auto conv = NamedTypeSymbol::get_conversion(expr->type, targetType);
    if (conv.level == Convertibility::Implicit)
        return fhir.cast(expr->syntax, targetType, expr, true, conv.method);

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
    if (!result) return nullptr;

    if (auto* tref = result->as<FhirTypeRef>())
    {
        diag.error("'" + format_type_name(tref->referenced) + "' is a type, not a value", expr->span);
        return fhir.error_expr(expr, nullptr, tref);
    }
    if (auto* nref = result->as<FhirNamespaceRefExpr>())
    {
        std::string name = nref->namespaceSymbol ? nref->namespaceSymbol->name : "?";
        diag.error("'" + name + "' is a namespace, not a value", expr->span);
        return fhir.error_expr(expr, nullptr, nref);
    }
    if (auto* mg = result->as<FhirMethodGroupRefExpr>())
    {
        diag.error("'" + std::string(mg->name) + "' is a method, not a value", expr->span);
        return fhir.error_expr(expr, nullptr, mg);
    }

    return result;
}

FhirExpr* Binder::bind_identifier(IdentifierExprSyntax* expr)
{
    Symbol* symbol = lookup(expr->name.lexeme);
    if (!symbol)
    {
        diag.error("undefined name '" + std::string(expr->name.lexeme) + "'", expr->span);
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
            return fhir.field_ref(expr, fhir.this_expr(expr, thisType), fieldSym);
        }
        case SymbolKind::NamedType:
        case SymbolKind::TypeParam:
        {
            auto* typeSym = symbol->as<TypeSymbol>();
            return fhir.type_ref(expr, typeSym);
        }
        case SymbolKind::Namespace:
        {
            auto* nsSym = symbol->as<NamespaceSymbol>();
            return fhir.namespace_ref(expr, nsSym);
        }
        case SymbolKind::Method:
        {
            auto* methodSym = symbol->as<MethodSymbol>();
            FhirExpr* thisRef = nullptr;
            if (auto* enclosingMethod = containing_method();
                enclosingMethod && !has_modifier(enclosingMethod->modifiers, Modifier::Static))
            {
                if (auto* enclosingType = containing_type())
                    thisRef = fhir.this_expr(expr, enclosingType);
            }
            return fhir.method_group_ref(expr, methodSym->parent, methodSym->name, thisRef);
        }
        default:
            return fhir.error_expr(expr);
    }
}

FhirExpr* Binder::bind_this(ThisExprSyntax* expr)
{
    auto* type = containing_type();
    if (!type)
    {
        diag.error("'this' can only be used inside a type", expr->span);
        return fhir.error_expr(expr);
    }

    return fhir.this_expr(expr, type);
}

FhirExpr* Binder::bind_paren(ParenExprSyntax* expr, TypeSymbol* expected)
{
    return bind_value_expr(expr->expression, expected);
}

FhirExpr* Binder::bind_cast(CastExprSyntax* expr)
{
    FhirTypeRef* typeRef = bind_type_ref(expr->type);
    TypeSymbol* targetType = typeRef ? typeRef->referenced : nullptr;
    if (!targetType)
        return fhir.error_expr(expr);

    FhirExpr* operand = bind_value_expr(expr->operand);
    if (!operand || operand->is_error())
        return fhir.error_expr(expr);

    if (operand->type == targetType)
        return operand;

    auto conv = NamedTypeSymbol::get_conversion(operand->type, targetType);
    if (conv.level == Convertibility::Implicit || conv.level == Convertibility::Explicit)
    {
        return fhir.cast(expr, targetType, operand, false, conv.method, typeRef);
    }

    diag.error("cannot cast from '" + format_type_name(operand->type) +
          "' to '" + format_type_name(targetType) + "'", expr->span);
    return fhir.error_expr(expr);
}

FhirExpr* Binder::bind_generic_name_expr(GenericNameExprSyntax* expr)
{
    TypeSymbol* type = resolve_generic_name(expr);
    if (!type) return fhir.error_expr(expr);
    return fhir.type_ref(expr, type);
}

FhirExpr* Binder::bind_member_access(MemberAccessExprSyntax* expr)
{
    if (!expr->right) return fhir.error_expr(expr);
    std::string_view memberName = expr->right->name.lexeme;

    FhirExpr* left = bind_expr(expr->left);
    if (left && left->is_error()) return fhir.error_expr(expr);

    // Left is a TYPE: looking up a member on a type (nested type, static field, static method)
    if (auto* leftTypeRef = left ? left->as<FhirTypeRef>() : nullptr)
    {
        auto* namedLeft = leftTypeRef->referenced ? leftTypeRef->referenced->as<NamedTypeSymbol>() : nullptr;
        if (!namedLeft)
        {
            diag.error("type '" + format_type_name(leftTypeRef->referenced) + "' has no members", expr->span);
            return fhir.error_expr(expr, nullptr, left);
        }

        // X<T> member: only nested generic types make sense here. Methods don't carry type args in Fern.
        if (auto* genRight = expr->right->as<GenericNameExprSyntax>())
        {
            TypeSymbol* type = resolve_generic_name(genRight, namedLeft);
            if (!type) return fhir.error_expr(expr);
            return fhir.type_ref(expr, type);
        }

        Symbol* member = namedLeft->find_member(memberName);
        if (!member)
        {
            diag.error("type '" + format_type_name(namedLeft) + "' has no member '" +
                  std::string(memberName) + "'", expr->span);
            return fhir.error_expr(expr);
        }

        if (auto* nested = member->as<NamedTypeSymbol>())
            return fhir.type_ref(expr, nested);

        if (auto* field = member->as<FieldSymbol>())
        {
            if (!has_modifier(field->modifiers, Modifier::Static))
            {
                diag.error("cannot access instance field '" + std::string(memberName) +
                      "' on type '" + format_type_name(namedLeft) + "'", expr->span);
                return fhir.error_expr(expr, field->type);
            }
            return fhir.field_ref(expr, nullptr, field);
        }

        if (member->as<MethodSymbol>())
        {
            // Static-vs-instance check moves to bind_call (where we know the resolved overload).
            return fhir.method_group_ref(expr, namedLeft, memberName, /*thisRef=*/nullptr);
        }

        return fhir.error_expr(expr);
    }

    // Left is a NAMESPACE: dot into the namespace's members
    if (auto* leftNs = left ? left->as<FhirNamespaceRefExpr>() : nullptr)
    {
        auto* ns = leftNs->namespaceSymbol;

        if (auto* genRight = expr->right->as<GenericNameExprSyntax>())
        {
            TypeSymbol* type = resolve_generic_name(genRight, ns);
            if (!type) return fhir.error_expr(expr);
            return fhir.type_ref(expr, type);
        }

        Symbol* member = ns->find_member(memberName);
        if (!member)
        {
            diag.error("namespace '" + ns->name + "' has no member '" +
                  std::string(memberName) + "'", expr->span);
            return fhir.error_expr(expr);
        }

        if (auto* nestedNs = member->as<NamespaceSymbol>())
            return fhir.namespace_ref(expr, nestedNs);
        if (auto* nestedType = member->as<NamedTypeSymbol>())
            return fhir.type_ref(expr, nestedType);
        return fhir.error_expr(expr);
    }

    // Anything else: left must be a value with a NamedType; look up an instance field or method.
    TypeSymbol* leftType = left ? left->type : nullptr;
    if (!leftType) return fhir.error_expr(expr);

    auto* namedType = leftType->as<NamedTypeSymbol>();
    if (!namedType) return fhir.error_expr(expr);

    if (expr->right->as<GenericNameExprSyntax>())
    {
        diag.error("type arguments are not supported on instance members", expr->right->span);
        return fhir.error_expr(expr);
    }

    if (auto* field = namedType->find_field(memberName))
    {
        return fhir.field_ref(expr, left, field);
    }

    if (namedType->find_method(memberName))
    {
        return fhir.method_group_ref(expr, namedType, memberName, left);
    }

    diag.error("type '" + format_type_name(namedType) + "' has no member '" +
          std::string(memberName) + "'", expr->span);
    return fhir.error_expr(expr);
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
            diag.error(msg, expr->span);
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

        diag.error("operator '" + std::string(Fern::format(opToken)) +
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

        diag.error(msg, syntax->span);
        return fhir.error_expr(syntax);
    }

    std::string leftName = format_type_name(leftType);
    std::string rightName = format_type_name(rightType);
    diag.error("operator '" + std::string(Fern::format(opToken)) +
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
        diag.error(msg, syntax->span);
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
            diag.error("cannot index a value of type '" + format_type_name(objectType) + "'", expr->span);
            return fhir.error_expr(expr);
        }

        auto setterResult = namedType->find_index_setter(indexType, valueType);
        if (setterResult.ambiguous)
        {
            std::string msg = "index assignment is ambiguous between:";
            for (auto* m : setterResult.ambiguousCandidates)
                msg += "\n  " + format_type_name(namedType) + ".op []=(" + m->format_parameters() + ")";
            diag.error(msg, expr->span);
            return fhir.error_expr(expr);
        }
        if (!setterResult.best.is_callable())
        {
            diag.error("type '" + format_type_name(namedType) +
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
        diag.error("cannot index a value of type '" + format_type_name(objectType) + "'", expr->span);
        return fhir.error_expr(expr);
    }

    auto getterResult = namedType->find_index_getter(indexType);
    if (getterResult.ambiguous)
    {
        std::string msg = "index access is ambiguous between:";
        for (auto* m : getterResult.ambiguousCandidates)
            msg += "\n  " + format_type_name(namedType) + ".op [](" + m->format_parameters() + ")";
        diag.error(msg, expr->span);
        return fhir.error_expr(expr);
    }
    if (!getterResult.best.is_callable())
    {
        diag.error("type '" + format_type_name(namedType) +
              "' has no 'op []' for index type '" + format_type_name(indexType) + "'", expr->span);
        return fhir.error_expr(expr);
    }

    MethodSymbol* method = getterResult.best.method;
    index = coerce_to_param(index, method->parameters[1]->type);
    return fhir.call(expr, method->get_return_type(), method, {object, index});
}

}
