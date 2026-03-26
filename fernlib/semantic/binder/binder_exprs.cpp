#include "binder.hpp"

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

static std::string format_arg_types(const std::vector<TypeSymbol*>& argTypes)
{
    std::string result = "(";
    for (size_t i = 0; i < argTypes.size(); ++i)
    {
        if (i > 0)
        {
            result += ", ";
        }
        result += format_type_name(argTypes[i]);
    }
    result += ")";
    return result;
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

FhirExpr* Binder::bind_expr(BaseExprSyntax* expr)
{
    if (!expr) return nullptr;

    if (auto* lit = expr->as<LiteralExprSyntax>())
        return bind_literal(lit);
    if (auto* id = expr->as<IdentifierExprSyntax>())
        return bind_identifier(id);
    if (auto* thisExpr = expr->as<ThisExprSyntax>())
        return bind_this(thisExpr);
    if (auto* unary = expr->as<UnaryExprSyntax>())
        return bind_unary(unary);
    if (auto* bin = expr->as<BinaryExprSyntax>())
        return bind_binary(bin);
    if (auto* assign = expr->as<AssignmentExprSyntax>())
        return bind_assignment(assign);
    if (auto* call = expr->as<CallExprSyntax>())
        return bind_call(call);
    if (auto* member = expr->as<MemberAccessExprSyntax>())
        return bind_member_access(member);
    if (auto* initializer = expr->as<InitializerExprSyntax>())
        return bind_initializer(initializer);
    if (auto* paren = expr->as<ParenExprSyntax>())
        return bind_paren(paren);
    if (auto* generic = expr->as<GenericTypeExprSyntax>())
        return bind_generic_type_expr(generic);
    if (auto* indexExpr = expr->as<IndexExprSyntax>())
        return bind_index(indexExpr);

    return nullptr;
}

FhirExpr* Binder::bind_literal(LiteralExprSyntax* expr)
{
    auto typeKeyword = literal_to_type_keyword(expr->token.kind);
    TypeSymbol* type = typeKeyword ? context.resolve_type_name(*typeKeyword) : nullptr;

    auto* node = fhir.literal(expr, type);

    try
    {
        if (expr->token.kind == TokenKind::LiteralI32)
            node->value = LiteralValue::make_int(std::stoi(std::string(expr->token.lexeme)));
        else if (expr->token.kind == TokenKind::LiteralF32)
            node->value = LiteralValue::make_float(std::stof(std::string(expr->token.lexeme)));
        else if (expr->token.kind == TokenKind::LiteralBool)
            node->value = LiteralValue::make_bool(expr->token.lexeme == "true");
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
        return nullptr;
    }

    switch (symbol->kind)
    {
        case SymbolKind::Local:
            return fhir.local_ref(expr, symbol->as<LocalSymbol>());
        case SymbolKind::Parameter:
            return fhir.param_ref(expr, symbol->as<ParameterSymbol>());
        case SymbolKind::Field:
        {
            auto* fieldSym = symbol->as<FieldSymbol>();
            auto* thisType = symbol->parent ? symbol->parent->as<TypeSymbol>() : nullptr;
            return fhir.field_access(expr, fhir.this_expr(expr, thisType), fieldSym);
        }
        case SymbolKind::Type:
            error("'" + std::string(expr->name.lexeme) + "' is a type, not a value", expr->span);
            return nullptr;
        case SymbolKind::Namespace:
            error("'" + std::string(expr->name.lexeme) + "' is a namespace, not a value", expr->span);
            return nullptr;
        case SymbolKind::Method:
            error("'" + std::string(expr->name.lexeme) + "' is a method, not a value", expr->span);
            return nullptr;
        default:
            return nullptr;
    }
}

FhirExpr* Binder::bind_this(ThisExprSyntax* expr)
{
    if (!currentType)
    {
        error("'this' can only be used inside a type", expr->span);
        return nullptr;
    }

    return fhir.this_expr(expr, currentType);
}

FhirExpr* Binder::bind_paren(ParenExprSyntax* expr)
{
    return bind_expr(expr->expression);
}

FhirExpr* Binder::bind_generic_type_expr(GenericTypeExprSyntax* expr)
{
    resolve_generic_type(expr);
    return nullptr;
}

FhirExpr* Binder::bind_unary(UnaryExprSyntax* expr)
{
    FhirExpr* operand = bind_expr(expr->operand);
    TypeSymbol* operandType = operand ? operand->type : nullptr;

    auto* namedType = operandType ? operandType->as<NamedTypeSymbol>() : nullptr;
    if (namedType)
    {
        TokenKind opToken = unary_op_to_token(expr->op);
        if (auto* method = namedType->find_unary_operator(opToken, operandType))
        {
            if (!namedType->is_builtin())
            {
                return fhir.call(expr, method->get_return_type(), method, {operand});
            }
            return fhir.intrinsic(expr, method->get_return_type(), to_intrinsic_op(expr->op), {operand});
        }

        error("operator '" + std::string(Fern::format(opToken)) +
              "' cannot be applied to value of type '" + format_type_name(namedType) + "'", expr->span);
    }

    return fhir.intrinsic(expr, operandType, to_intrinsic_op(expr->op), {operand});
}

FhirExpr* Binder::bind_binary(BinaryExprSyntax* expr)
{
    FhirExpr* lhs = bind_expr(expr->left);
    FhirExpr* rhs = bind_expr(expr->right);
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
        bool hasBase = namedType->find_binary_operator(baseToken, leftType, rightType) != nullptr;
        bool hasEqual = (baseToken == TokenKind::Equal) || namedType->find_binary_operator(TokenKind::Equal, leftType, rightType) != nullptr;

        if (hasBase && hasEqual)
        {
            TypeSymbol* boolType = context.resolve_type_name(TokenKind::BoolKeyword);
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
        return nullptr;
    }

    std::string leftName = format_type_name(leftType);
    std::string rightName = format_type_name(rightType);
    error("operator '" + std::string(Fern::format(opToken)) +
          "' cannot be applied to values of type '" + leftName + "' and '" + rightName + "'", syntax->span);
    return nullptr;
}

FhirExpr* Binder::bind_binary_op(BinaryOp op, FhirExpr* lhs, FhirExpr* rhs, BaseExprSyntax* syntax)
{
    TypeSymbol* leftType = lhs ? lhs->type : nullptr;
    TypeSymbol* rightType = rhs ? rhs->type : nullptr;

    TokenKind opToken = binary_op_to_token(op);

    auto* namedType = leftType ? leftType->as<NamedTypeSymbol>() : nullptr;
    if (!namedType)
    {
        return fhir.intrinsic(syntax, leftType, to_intrinsic_op(op), {lhs, rhs});
    }

    if (auto* method = namedType->find_binary_operator(opToken, leftType, rightType))
    {
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
        FhirExpr* object = bind_expr(indexExpr->object);
        FhirExpr* index = bind_expr(indexExpr->index);
        FhirExpr* value = bind_expr(expr->value);

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
            return nullptr;
        }

        auto* method = namedType->find_index_setter(indexType, valueType);
        if (!method)
        {
            error("type '" + format_type_name(namedType) +
                  "' has no 'op []=' for index type '" + format_type_name(indexType) +
                  "' and value type '" + format_type_name(valueType) + "'", expr->span);
            return nullptr;
        }

        return fhir.call(expr, method->get_return_type(), method, {object, index, value});
    }

    FhirExpr* value = bind_expr(expr->value);
    FhirExpr* writeTarget = bind_expr(expr->target);

    // Compound assignments (+=, -=, etc.) desugar to target = target op value,
    // binding the target twice: once for reading the current value, once for the write
    if (expr->op != AssignOp::Simple)
    {
        FhirExpr* readTarget = bind_expr(expr->target);

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
    FhirExpr* left = bind_expr(expr->left);
    TypeSymbol* leftType = left ? left->type : nullptr;

    std::string_view memberName = expr->right.lexeme;
    if (memberName.empty())
    {
        return nullptr;
    }

    if (!leftType)
    {
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
            return nullptr;
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
                    return nullptr;
                }
                return fhir.field_access(expr, nullptr, field);
            }

            if (auto* method = typeRef->find_method(memberName))
            {
                if (!has_modifier(method->modifiers, Modifier::Static))
                {
                    error("cannot access instance method '" + std::string(memberName) +
                          "' on type '" + format_type_name(typeRef) + "'", expr->span);
                    return nullptr;
                }
                return nullptr;
            }

            error("type '" + format_type_name(typeRef) + "' has no member '" +
                  std::string(memberName) + "'", expr->span);
            return nullptr;
        }

        if (leftSym)
        {
            error("'" + leftSym->name + "' is not a namespace or type", expr->span);
        }
        return nullptr;
    }

    auto* namedType = leftType->as<NamedTypeSymbol>();
    if (!namedType)
    {
        return nullptr;
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
    return nullptr;
}

FhirExpr* Binder::bind_call(CallExprSyntax* expr)
{
    std::vector<FhirExpr*> argExprs;
    std::vector<TypeSymbol*> argTypes;
    for (auto* arg : expr->arguments)
    {
        FhirExpr* bound = bind_expr(arg);
        argExprs.push_back(bound);
        argTypes.push_back(bound ? bound->type : nullptr);
    }

    Symbol* calleeSym = resolve_expr_symbol(expr->callee);

    if (calleeSym && calleeSym->kind == SymbolKind::Type)
    {
        auto* namedType = calleeSym->as<NamedTypeSymbol>();
        if (namedType)
        {
            MethodSymbol* ctor = namedType->resolve_constructor(argTypes);
            if (!ctor)
            {
                if (namedType->has_constructor_with_count(argTypes.size()))
                {
                    error("no constructor for '" + format_type_name(namedType) +
                          "' matches argument types " + format_arg_types(argTypes), expr->span);
                }
                else
                {
                    error("'" + format_type_name(namedType) + "' does not contain a constructor that takes " +
                          std::to_string(argTypes.size()) + (argTypes.size() == 1 ? " argument" : " arguments"), expr->span);
                }
            }

            return fhir.object_create(expr, namedType, ctor, std::move(argExprs));
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
            }
        }
    }

    if (targetType && !methodName.empty())
    {
        method = targetType->resolve_method(methodName, argTypes);
        if (!method)
        {
            if (targetType->has_method_with_count(methodName, argTypes.size()))
            {
                error("no overload of '" + std::string(methodName) + "' on '" + format_type_name(targetType) +
                      "' matches argument types " + format_arg_types(argTypes), expr->span);
            }
            else
            {
                error("'" + format_type_name(targetType) + "' does not contain a method '" + std::string(methodName) +
                      "' that takes " + std::to_string(argTypes.size()) +
                      (argTypes.size() == 1 ? " argument" : " arguments"), expr->span);
            }
        }
    }

    TypeSymbol* returnType = method ? method->get_return_type() : nullptr;

    if (method && method->is_constructor())
    {
        return fhir.object_create(expr, returnType, method, std::move(argExprs));
    }

    if (receiver && method && !has_modifier(method->modifiers, Modifier::Static))
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
        MethodSymbol* ctor = namedType->resolve_constructor({});
        if (ctor)
        {
            return fhir.object_create(expr->target, namedType, ctor, {});
        }
    }

    return bind_expr(expr->target);
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
                bind_expr(fieldInit->value);
            }
            else if (auto* childStmt = member->as<ExpressionStmtSyntax>())
            {
                bind_expr(childStmt->expression);
            }
        }
        return nullptr;
    }

    NamedTypeSymbol* namedType = nullptr;

    if (auto* idExpr = expr->target->as<IdentifierExprSyntax>())
    {
        Symbol* sym = resolve_name(idExpr->name.lexeme);
        if (!sym)
        {
            error("undefined name '" + std::string(idExpr->name.lexeme) + "'", idExpr->span);
            return nullptr;
        }

        namedType = sym->as<NamedTypeSymbol>();
        if (!namedType)
        {
            error("initializer lists can only be applied to a type or constructor call", idExpr->span);
            return nullptr;
        }

        if (!namedType->resolve_constructor({}))
        {
            error("type '" + format_type_name(namedType) + "' has no default constructor", idExpr->span);
        }
    }
    else if (auto* callExpr = expr->target->as<CallExprSyntax>())
    {
        FhirExpr* callResult = bind_call(callExpr);
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
    else if (auto* genericExpr = expr->target->as<GenericTypeExprSyntax>())
    {
        TypeSymbol* type = resolve_generic_type(genericExpr);
        namedType = type ? type->as<NamedTypeSymbol>() : nullptr;
        if (!namedType)
        {
            return nullptr;
        }

        if (!namedType->resolve_constructor({}))
        {
            error("type '" + format_type_name(namedType) + "' has no default constructor", genericExpr->span);
        }
    }
    else
    {
        error("initializer lists can only be applied to a type or constructor call", expr->target->span);
        return nullptr;
    }

    if (!namedType)
    {
        for (auto* member : expr->members)
        {
            if (auto* fieldInit = member->as<FieldInitSyntax>())
            {
                bind_expr(fieldInit->value);
            }
            else if (auto* childStmt = member->as<ExpressionStmtSyntax>())
            {
                bind_expr(childStmt->expression);
            }
        }
        return nullptr;
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
        return nullptr;
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
                bind_expr(childStmt->expression);
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
                out.push_back(fhir.expr_stmt(fieldInit, fhir.assign(fieldInit, fieldTarget, bind_expr(fieldInit->value))));
            }
            else
            {
                bind_expr(fieldInit->value);
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

FhirExpr* Binder::bind_index(IndexExprSyntax* expr)
{
    FhirExpr* object = bind_expr(expr->object);
    FhirExpr* index = bind_expr(expr->index);

    TypeSymbol* objectType = object ? object->type : nullptr;
    TypeSymbol* indexType = index ? index->type : nullptr;

    auto* namedType = objectType ? objectType->as<NamedTypeSymbol>() : nullptr;
    if (!namedType)
    {
        error("cannot index a value of type '" + format_type_name(objectType) + "'", expr->span);
        return nullptr;
    }

    auto* method = namedType->find_index_getter(indexType);
    if (!method)
    {
        error("type '" + format_type_name(namedType) +
              "' has no 'op []' for index type '" + format_type_name(indexType) + "'", expr->span);
        return nullptr;
    }

    return fhir.call(expr, method->get_return_type(), method, {object, index});
}

}
