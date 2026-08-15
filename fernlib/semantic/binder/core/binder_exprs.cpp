#include "binder.hpp"

#include <charconv>
#include <cstdint>
#include <format>
#include <system_error>

#include <ast/ast.hpp>
#include <semantic/context.hpp>
#include <semantic/fhir/fhir.hpp>
#include <semantic/symbol/fmt.hpp>

namespace Fern
{

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
        result = bind_binary(bin, expected);
    else if (auto* assign = expr->as<AssignmentExprSyntax>())
        result = bind_assignment(assign);
    else if (auto* call = expr->as<CallExprSyntax>())
        result = bind_call(call);
    else if (auto* member = expr->as<MemberAccessExprSyntax>())
        result = bind_member_access(member);
    else if (auto* objectBuilder = expr->as<ObjectBuilderExprSyntax>())
        result = bind_object_builder(objectBuilder);
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
    else if (expr->is<ErrorExprSyntax>())
        result = fhir.error_expr(expr);

    if (result && result->is_error()) return result;

    if (expected && result && result->type)
    {
        if (result->type == expected)
        {
            const auto& constant = result->get_constant();
            if (constant && !constant->range_fits(expected))
            {
                diag.report(DiagnosticCode::Err_ConstantOutOfRange, expr->span, constant->intValue, format_type(expected));
                return fhir.error_expr(expr, expected, result);
            }
            return result;
        }

        // return as an implicit cast if possible
        if (auto* castResult = try_implicit_cast(result, expected, expr->span))
        {
            return castResult;
        }

        const auto& constant = result->get_constant();
        report_conversion_failure(result->type, expected, constant ? &*constant : nullptr, expr->span);
        return fhir.error_expr(expr, expected, result);
    }

    return result;
}

// reports the specific out of range error for integer constants, otherwise the generic conversion error
void Binder::report_conversion_failure(TypeSymbol* from, TypeSymbol* to, const ConstantValue* constant, const Span& span, std::string prefix)
{
    auto* fromNamed = from ? from->as<NamedTypeSymbol>() : nullptr;
    auto* toNamed = to ? to->as<NamedTypeSymbol>() : nullptr;
    if (fromNamed && fromNamed->is_integer() && toNamed && toNamed->is_integer()
        && constant && constant->kind == ConstantValue::Kind::Int)
    {
        diag.report(DiagnosticCode::Err_ConstantOutOfRange, span, constant->intValue, format_type(to));
        return;
    }

    DiagnosticCode code = (NamedTypeSymbol::get_conversion(from, to).level == Convertibility::Explicit)
        ? DiagnosticCode::Err_NoImplicitConv
        : DiagnosticCode::Err_TypeMismatch;
    diag.report(code, span, prefix, format_type(from), format_type(to));
}

FhirCastExpr* Binder::try_implicit_cast(FhirExpr* expr, TypeSymbol* targetType, const Span& span)
{
    if (!expr || !expr->type || !targetType) return nullptr;

    auto conv = NamedTypeSymbol::get_conversion(OverloadArg(expr), targetType);
    if (conv.level == Convertibility::Implicit)
        return fhir.cast(expr->syntax, targetType, expr, conv.method);

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
        diag.report(DiagnosticCode::Err_BadSymbolKind, expr->span, format_type(tref->referenced), "type", "value");
        return fhir.error_expr(expr, nullptr, tref);
    }
    if (auto* nref = result->as<FhirNamespaceRefExpr>())
    {
        std::string name = nref->namespaceSymbol ? nref->namespaceSymbol->name : "?";
        diag.report(DiagnosticCode::Err_BadSymbolKind, expr->span, name, "namespace", "value");
        return fhir.error_expr(expr, nullptr, nref);
    }
    if (auto* mg = result->as<FhirMethodGroupRefExpr>())
    {
        diag.report(DiagnosticCode::Err_BadSymbolKind, expr->span, mg->name, "method", "value");
        return fhir.error_expr(expr, nullptr, mg);
    }

    return result;
}

FhirExpr* Binder::bind_identifier(IdentifierExprSyntax* expr)
{
    LookupResult result = lookup(expr->name.lexeme);
    if (result.empty())
    {
        diag.report(DiagnosticCode::Err_UndefinedName, expr->span, expr->name.lexeme);
        return fhir.error_expr(expr);
    }

    if (result.is_method_group())
    {
        auto* first = result.symbols.front()->as<MethodSymbol>();
        FhirExpr* thisRef = nullptr;
        if (auto* enclosingMethod = containing_method();
            enclosingMethod && !has_modifier(enclosingMethod->modifiers, Modifier::Static))
        {
            if (auto* enclosingType = containing_type())
                thisRef = fhir.this_expr(expr, enclosingType);
        }
        return fhir.method_group_ref(expr, first->parent, first->name, thisRef);
    }

    Symbol* symbol = result.single();
    if (!symbol) return fhir.error_expr(expr);

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

            if (has_modifier(fieldSym->modifiers, Modifier::Static))
                return fhir.field_ref(expr, nullptr, fieldSym);

            if (auto* method = containing_method();
                method && has_modifier(method->modifiers, Modifier::Static))
            {
                diag.report(DiagnosticCode::Err_InstanceFieldInStatic, expr->span, fieldSym->name);
                return fhir.error_expr(expr, fieldSym->type);
            }

            auto* thisType = symbol->parent ? symbol->parent->as<TypeSymbol>() : nullptr;
            return fhir.field_ref(expr, fhir.this_expr(expr, thisType), fieldSym);
        }
        case SymbolKind::NamedType:
        case SymbolKind::TypeParam:
        {
            auto* typeSym = symbol->as<TypeSymbol>();
            return build_type_ref_tree(expr, typeSym);
        }
        case SymbolKind::Namespace:
        {
            auto* nsSym = symbol->as<NamespaceSymbol>();
            return fhir.namespace_ref(expr, nsSym);
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
        diag.report(DiagnosticCode::Err_ThisOutsideType, expr->span);
        return fhir.error_expr(expr);
    }

    if (auto* method = containing_method();
        method && has_modifier(method->modifiers, Modifier::Static))
    {
        diag.report(DiagnosticCode::Err_ThisInStaticFunction, expr->span);
        return fhir.error_expr(expr, type);
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

    if (!expr->operand)
    {
        // A cast with no operand is a type used as a value
        diag.report(DiagnosticCode::Err_BadSymbolKind, expr->type->span, format_type(targetType), "type", "value");
        return fhir.error_expr(expr);
    }

    FhirExpr* operand = bind_value_expr(expr->operand);
    if (!operand || operand->is_error())
        return fhir.error_expr(expr);

    if (operand->type == targetType)
        return operand;

    auto conv = NamedTypeSymbol::get_conversion(operand->type, targetType);
    if (conv.level == Convertibility::Implicit || conv.level == Convertibility::Explicit)
    {
        return fhir.cast(expr, targetType, operand, conv.method, typeRef);
    }

    diag.report(DiagnosticCode::Err_BadCast, expr->span, std::string{}, format_type(operand->type), format_type(targetType));
    return fhir.error_expr(expr);
}

FhirExpr* Binder::bind_generic_name_expr(GenericNameExprSyntax* expr)
{
    TypeSymbol* type = resolve_generic_name(expr);
    if (!type) return fhir.error_expr(expr);
    return build_type_ref_tree(expr, type);
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
            diag.report(DiagnosticCode::Err_TypeHasNoMembers, expr->span, format_type(leftTypeRef->referenced));
            return fhir.error_expr(expr, nullptr, left);
        }

        // X<T> member: only nested generic types make sense here. Methods don't carry type args in Fern.
        if (auto* genRight = expr->right->as<GenericNameExprSyntax>())
        {
            TypeSymbol* type = resolve_generic_name(genRight, namedLeft);
            if (!type) return fhir.error_expr(expr);
            return build_type_ref_tree(expr, type);
        }

        if (!namedLeft->collect_methods(memberName).empty())
        {
            // Static-vs-instance check moves to bind_call (where we know the resolved overload).
            return fhir.method_group_ref(expr, namedLeft, memberName, /*thisRef=*/nullptr);
        }

        Symbol* member = namedLeft->find_non_method_member(memberName);
        if (!member)
        {
            diag.report(DiagnosticCode::Err_NoSuchMember, expr->span, format_type(namedLeft), memberName);
            return fhir.error_expr(expr);
        }

        if (auto* nested = member->as<NamedTypeSymbol>())
            return build_type_ref_tree(expr, nested);

        if (auto* field = member->as<FieldSymbol>())
        {
            if (!has_modifier(field->modifiers, Modifier::Static))
            {
                diag.report(DiagnosticCode::Err_InstanceFieldOnType, expr->span, memberName, format_type(namedLeft));
                return fhir.error_expr(expr, field->type);
            }
            return fhir.field_ref(expr, nullptr, field);
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
            return build_type_ref_tree(expr, type);
        }

        Symbol* member = ns->find_member(memberName);
        if (!member)
        {
            diag.report(DiagnosticCode::Err_NoSuchMember, expr->span, ns->name, memberName);
            return fhir.error_expr(expr);
        }

        if (auto* nestedNs = member->as<NamespaceSymbol>())
            return fhir.namespace_ref(expr, nestedNs);
        if (auto* nestedType = member->as<NamedTypeSymbol>())
            return build_type_ref_tree(expr, nestedType);
        return fhir.error_expr(expr);
    }

    // Anything else: left must be a value with a NamedType; look up an instance field or method.
    TypeSymbol* leftType = left ? left->type : nullptr;
    if (!leftType) return fhir.error_expr(expr);

    auto* namedType = leftType->as<NamedTypeSymbol>();
    if (!namedType) return fhir.error_expr(expr);

    if (expr->right->as<GenericNameExprSyntax>())
    {
        diag.report(DiagnosticCode::Err_TypeArgsOnInstanceMember, expr->right->span);
        return fhir.error_expr(expr);
    }

    if (auto* field = namedType->find_field(memberName))
    {
        if (has_modifier(field->modifiers, Modifier::Static))
        {
            diag.report(DiagnosticCode::Err_StaticMemberOnInstance, expr->span, memberName);
            return fhir.error_expr(expr, field->type);
        }
        return fhir.field_ref(expr, left, field);
    }

    if (!namedType->collect_methods(memberName).empty())
    {
        return fhir.method_group_ref(expr, namedType, memberName, left, /*explicitReceiver=*/true);
    }

    diag.report(DiagnosticCode::Err_NoSuchMember, expr->span, format_type(namedType), memberName);
    return fhir.error_expr(expr);
}

// Constant math that does not fit the operation's own type is an error, so compile time folding
// and runtime wrapping can never disagree about the value
FhirExpr* Binder::check_constant_overflow(FhirOpExpr* node)
{
    if (!node->constant_overflows()) return node;

    diag.report(DiagnosticCode::Err_ConstantOverflow, node->span, format_type(node->type));
    return fhir.error_expr(node->syntax, node->type, node);
}

FhirExpr* Binder::bind_unary(UnaryExprSyntax* expr)
{
    // A minus on an integer literal is typed as one number so the minimum of each type can be written directly
    auto* literal = expr->operand ? expr->operand->as<LiteralExprSyntax>() : nullptr;
    if (expr->op == UnaryOp::Negative && literal && literal->token.kind == TokenKind::LiteralInt)
    {
        uint64_t magnitude = 0;
        const char* first = literal->token.lexeme.data();
        const char* last = first + literal->token.lexeme.size();
        auto [ptr, ec] = std::from_chars(first, last, magnitude);
        if (ec != std::errc{} || magnitude > (uint64_t{1} << 63))
        {
            diag.report(DiagnosticCode::Err_LiteralOutOfRange, expr->span, std::format("-{}", literal->token.lexeme));
            return fhir.error_expr(expr);
        }

        int64_t value = static_cast<int64_t>(0ull - magnitude);
        auto* node = fhir.literal(expr, type_integer_literal(value));
        node->value = ConstantValue::make_int(value);
        return node;
    }

    FhirExpr* operand = bind_value_expr(expr->operand);
    if (!operand || operand->is_error()) return fhir.error_expr(expr);

    TypeSymbol* operandType = operand ? operand->type : nullptr;

    auto* namedType = operandType ? operandType->as<NamedTypeSymbol>() : nullptr;
    if (namedType)
    {
        TokenKind opToken = unary_op_to_token(expr->op);
        auto result = namedType->find_unary_operator(opToken);
        if (result.ambiguous)
        {
            std::string candidates;
            for (auto* m : result.ambiguousCandidates)
                candidates += std::format("\n  {}", format_method(m, SymbolFormat::signature()));
            diag.report(DiagnosticCode::Err_AmbiguousUnaryOp, expr->span,
                  Fern::format(opToken), format_type(namedType), candidates);
            return fhir.error_expr(expr);
        }
        if (result.best.is_callable())
        {
            MethodSymbol* method = result.best.method;
            operand = coerce_to_param(operand, method->parameters[0]->type);
            return check_constant_overflow(fhir.op(expr, method->get_return_type(), method->intrinsic(), {operand}, method));
        }

        diag.report(DiagnosticCode::Err_BadUnaryOp, expr->span, Fern::format(opToken), format_type(namedType));
        return fhir.error_expr(expr);
    }

    return fhir.op(expr, operandType, IntrinsicKind::None, {operand});
}

FhirExpr* Binder::bind_binary(BinaryExprSyntax* expr, TypeSymbol* expected)
{
    FhirExpr* lhs = bind_value_expr(expr->left);
    FhirExpr* rhs = bind_value_expr(expr->right);
    return bind_binary_op(expr->op, lhs, rhs, expr, expected);
}

// Breaks an ambiguous binary operator: prefer a candidate whose result matches the
// expected type, then prefer the operator owned by the left operand's type.
MethodSymbol* Binder::break_operator_tie(const std::vector<MethodSymbol*>& candidates, TypeSymbol* expected, TypeSymbol* leftType)
{
    std::vector<MethodSymbol*> pool = candidates;

    if (expected)
    {
        std::vector<MethodSymbol*> byReturn;
        for (auto* method : pool)
        {
            auto level = NamedTypeSymbol::get_conversion(method->get_return_type(), expected).level;
            if (level == Convertibility::Exact || level == Convertibility::Implicit)
                byReturn.push_back(method);
        }
        if (byReturn.size() == 1) return byReturn.front();
        if (!byReturn.empty()) pool = byReturn;
    }

    MethodSymbol* leftOwned = nullptr;
    int leftCount = 0;
    for (auto* method : pool)
    {
        if (method->parent == leftType)
        {
            leftOwned = method;
            ++leftCount;
        }
    }
    if (leftCount == 1) return leftOwned;

    return nullptr;
}

FhirExpr* Binder::bind_binary_op(BinaryOp op, FhirExpr* lhs, FhirExpr* rhs, BaseExprSyntax* syntax, TypeSymbol* expected)
{
    bool lhsError = !lhs || lhs->is_error();
    bool rhsError = !rhs || rhs->is_error();
    if (lhsError || rhsError) return fhir.error_expr(syntax);

    TypeSymbol* leftType = lhs ? lhs->type : nullptr;
    TypeSymbol* rightType = rhs ? rhs->type : nullptr;

    TokenKind opToken = binary_op_to_token(op);

    // TODO: this should be an error, this is only reached when the left type is null
    auto* namedType = leftType ? leftType->as<NamedTypeSymbol>() : nullptr;
    if (!namedType)
    {
        return fhir.op(syntax, leftType, IntrinsicKind::None, {lhs, rhs});
    }

    auto* rightNamed = rightType ? rightType->as<NamedTypeSymbol>() : nullptr;
    auto result = namedType->find_binary_operator(opToken, OverloadArg(lhs), OverloadArg(rhs), rightNamed);

    MethodSymbol* method = nullptr;
    if (result.ambiguous)
        method = break_operator_tie(result.ambiguousCandidates, expected, leftType);
    else if (result.best.is_callable())
        method = result.best.method;

    if (method)
    {
        lhs = coerce_to_param(lhs, method->parameters[0]->type);
        rhs = coerce_to_param(rhs, method->parameters[1]->type);
        return check_constant_overflow(fhir.op(syntax, method->get_return_type(), method->intrinsic(), {lhs, rhs}, method));
    }

    if (result.ambiguous)
    {
        std::string candidates;
        for (auto* m : result.ambiguousCandidates)
            candidates += std::format("\n  {}", format_method(m, SymbolFormat::signature()));
        diag.report(DiagnosticCode::Err_AmbiguousBinaryOp, syntax->span,
              Fern::format(opToken), format_type(leftType), format_type(rightType), candidates);
        return fhir.error_expr(syntax);
    }

    diag.report(DiagnosticCode::Err_BadBinaryOp, syntax->span,
          Fern::format(opToken), format_type(leftType), format_type(rightType), "");
    return fhir.error_expr(syntax);
}

FhirExpr* Binder::bind_assignment(AssignmentExprSyntax* expr)
{
    FhirExpr* writeTarget;
    if (auto* idxSyntax = expr->target->as<IndexExprSyntax>())
        writeTarget = bind_index(idxSyntax, IndexContext::Write);
    else
        writeTarget = bind_value_expr(expr->target);

    TypeSymbol* targetType = writeTarget ? writeTarget->type : nullptr;
    FhirExpr* value = bind_value_expr(expr->value, targetType);

    if (!writeTarget || writeTarget->is_error() || !value || value->is_error())
        return fhir.error_expr(expr);

    auto* idx = writeTarget->as<FhirIndexExpr>();

    if (idx && !idx->setter)
    {
        TypeSymbol* objType = idx->object ? idx->object->type : nullptr;
        TypeSymbol* idxType = idx->index ? idx->index->type : nullptr;
        diag.report(DiagnosticCode::Err_NoIndexSetter, expr->target->span,
              format_type(objType), format_type(idxType));
        return fhir.error_expr(expr);
    }

    if (expr->op != AssignOp::Simple)
    {
        // Compound assignment reads the target before writing, so an index
        // target needs both a getter and a setter.
        if (idx && !idx->getter)
        {
            TypeSymbol* objType = idx->object ? idx->object->type : nullptr;
            TypeSymbol* idxType = idx->index ? idx->index->type : nullptr;
            diag.report(DiagnosticCode::Err_NoIndexGetter, expr->target->span,
                  format_type(objType), format_type(idxType));
            return fhir.error_expr(expr);
        }

        BinaryOp binOp = assign_op_to_binary_op(expr->op);
        FhirExpr* opResult = bind_binary_op(binOp, writeTarget, value, expr);
        if (!opResult || opResult->is_error()) return fhir.error_expr(expr);
        auto* binFhir = opResult->as<FhirOpExpr>();
        if (!binFhir) return fhir.error_expr(expr);
        return fhir.compound_assign(expr, binFhir);
    }

    return fhir.assign(expr, writeTarget, value);
}

FhirExpr* Binder::bind_index(IndexExprSyntax* expr, IndexContext ctx)
{
    FhirExpr* object = bind_value_expr(expr->object);
    FhirExpr* index = bind_value_expr(expr->index);

    if (!object || !index || object->is_error() || index->is_error())
    {
        return fhir.error_expr(expr);
    }

    TypeSymbol* objectType = object->type;
    TypeSymbol* indexType = index->type;

    auto* namedType = objectType ? objectType->as<NamedTypeSymbol>() : nullptr;
    if (!namedType)
    {
        diag.report(DiagnosticCode::Err_CannotIndex, expr->span, format_type(objectType));
        return fhir.error_expr(expr);
    }

    auto getterResult = namedType->find_index_getter(OverloadArg(index));
    auto setterResult = namedType->find_index_setter(OverloadArg(index));

    auto report_ambiguous = [&](const OverloadResult& r)
    {
        std::string candidates;
        for (auto* m : r.ambiguousCandidates)
            candidates += std::format("\n  {}", format_method(m, SymbolFormat::signature()));
        diag.report(DiagnosticCode::Err_AmbiguousCall, expr->span, candidates);
    };

    if (getterResult.ambiguous) { report_ambiguous(getterResult); return fhir.error_expr(expr); }
    if (setterResult.ambiguous) { report_ambiguous(setterResult); return fhir.error_expr(expr); }

    MethodSymbol* getter = getterResult.best.is_callable() ? getterResult.best.method : nullptr;
    MethodSymbol* setter = setterResult.best.is_callable() ? setterResult.best.method : nullptr;

    // No indexer methods declared on this type at all.
    if (getterResult.candidates.empty() && setterResult.candidates.empty())
    {
        diag.report(DiagnosticCode::Err_CannotIndex, expr->span, format_type(namedType));
        return fhir.error_expr(expr);
    }

    if (ctx == IndexContext::Read && !getter)
    {
        diag.report(DiagnosticCode::Err_NoIndexGetter, expr->index->span, format_type(namedType), format_type(indexType));
        return fhir.error_expr(expr);
    }

    if (ctx == IndexContext::Write && !getter && !setter)
    {
        diag.report(DiagnosticCode::Err_NoIndexSetter, expr->index->span, format_type(namedType), format_type(indexType));
        return fhir.error_expr(expr);
    }

    MethodSymbol* primary = getter ? getter : setter;
    TypeSymbol* exprType = getter ? getter->get_return_type() : setter->parameters[2]->type;
    index = coerce_to_param(index, primary->parameters[1]->type);

    return fhir.index_expr(expr, exprType, object, index, getter, setter);
}

}
