#include "semantic/type_resolver.hpp"
#include <sstream>

namespace Fern
{
    #pragma region Main Resolution Entry Point

    bool TypeResolver::resolve(BoundCompilationUnit *unit)
    {
        clear_diagnostics();
        substitution.clear();
        pendingConstraints.clear();
        constraintNodes.clear();

        // Multiple passes for type inference
        for (int pass = 0; pass < MAX_PASSES; ++pass)
        {
            // Clear errors at start of each pass (we'll re-encounter them if they persist)
            clear_diagnostics();

            size_t constraintsBefore = pendingConstraints.size();

            // Visit the entire tree
            unit->accept(this);

            // Check if we made progress
            bool madeProgress = (pendingConstraints.size() < constraintsBefore);

            if (!madeProgress || pendingConstraints.empty())
            {
                break;
            }
        }

        // Report any remaining unresolved types
        report_final_errors();

        return !has_errors();
    }

    #pragma region Core Type Resolution

    TypePtr TypeResolver::apply_substitution(TypePtr type)
    {
        if (!type)
            return nullptr;

        auto it = substitution.find(type);
        if (it == substitution.end())
        {
            return type; // Already canonical
        }

        // Path compression
        TypePtr root = apply_substitution(it->second);
        substitution[type] = root;
        return root;
    }

    void TypeResolver::unify(TypePtr t1, TypePtr t2, BoundNode *error_node, const std::string &context)
    {
        if (!t1 || !t2)
            return;

        TypePtr root1 = apply_substitution(t1);
        TypePtr root2 = apply_substitution(t2);

        if (root1 == root2)
            return; // Already unified

        bool root1_is_var = root1->is<UnresolvedType>();
        bool root2_is_var = root2->is<UnresolvedType>();

        if (root1_is_var)
        {
            substitution[root1] = root2;
            pendingConstraints.erase(root1);
            constraintNodes.erase(root1);
        }
        else if (root2_is_var)
        {
            substitution[root2] = root1;
            pendingConstraints.erase(root2);
            constraintNodes.erase(root2);
        }
        else if (root1->get_name() != root2->get_name())
        {
            report_error(error_node, context + ": Type mismatch - '" + root1->get_name() +
                                         "' and '" + root2->get_name() + "' are incompatible");
        }
    }

    void TypeResolver::annotate_expression(BoundExpression *expr, TypePtr type, Symbol *symbol)
    {
        if (!expr || !type)
            return;

        TypePtr canonical = apply_substitution(type);
        expr->type = canonical;

        // Update value category
        expr->valueCategory = compute_value_category(expr, symbol);

        // Track unresolved types
        if (canonical->is<UnresolvedType>())
        {
            pendingConstraints.insert(canonical);
            constraintNodes[canonical] = expr;  // Track which node has this unresolved type
        }
    }

    #pragma region Symbol Resolution

    Symbol *TypeResolver::resolve_qualified_name(const std::vector<std::string> &parts)
    {
        if (parts.empty())
            return nullptr;

        // TODO: Add this method to SymbolTable
        // For now, we'll handle single names and report an error for qualified names
        if (parts.size() == 1)
        {
            return symbolTable.resolve(parts[0]);
        }

        // For qualified names, we need to navigate the symbol hierarchy
        // This should be added to SymbolTable as resolve_qualified()
        Symbol *current = symbolTable.get_global_namespace();

        for (size_t i = 0; i < parts.size(); ++i)
        {
            auto container = current->as<ContainerSymbol>();
            if (!container)
                return nullptr;

            auto members = container->get_member(parts[i]);
            if (members.empty())
                return nullptr;

            current = members[0]; // TODO: Handle ambiguity
        }

        return current;
    }

    FunctionSymbol *TypeResolver::resolve_overload(const std::vector<FunctionSymbol *> &overloads,
                                                        const std::vector<TypePtr> &argTypes)
    {
        if (overloads.empty())
            return nullptr;

        // Single overload - check compatibility
        if (overloads.size() == 1)
        {
            FunctionSymbol *func = overloads[0];
            if (func->parameters.size() != argTypes.size())
                return nullptr;

            // Check if all arguments can convert
            for (size_t i = 0; i < argTypes.size(); ++i)
            {
                TypePtr paramType = apply_substitution(func->parameters[i]->type);

                // Handle unresolved parameter types
                if (paramType->is<UnresolvedType>())
                {
                    unify(paramType, argTypes[i], nullptr, "parameter inference");
                    continue;
                }

                ConversionKind conv = check_conversion(argTypes[i], paramType);
                if (!Conversions::is_implicit_conversion(conv))
                    return nullptr;
            }
            return func;
        }

        // Multiple overloads - find best match
        FunctionSymbol *bestMatch = nullptr;
        int bestScore = -1;

        for (auto func : overloads)
        {
            if (func->parameters.size() != argTypes.size())
                continue;

            int score = 0;
            bool viable = true;

            for (size_t i = 0; i < argTypes.size(); ++i)
            {
                TypePtr paramType = apply_substitution(func->parameters[i]->type);
                ConversionKind conv = check_conversion(argTypes[i], paramType);

                if (!Conversions::is_implicit_conversion(conv))
                {
                    viable = false;
                    break;
                }

                // Prefer identity conversions
                if (conv == ConversionKind::Identity)
                    score += 2;
                else if (conv == ConversionKind::ImplicitNumeric)
                    score += 1;
            }

            if (viable && score > bestScore)
            {
                bestMatch = func;
                bestScore = score;
            }
        }

        return bestMatch;
    }

    #pragma region Type Utilities

    TypePtr TypeResolver::resolve_type_expression(BoundExpression *typeExpr)
    {
        if (!typeExpr)
            return typeSystem.get_unresolved();

        // Handle BoundTypeExpression
        if (auto boundType = typeExpr->as<BoundTypeExpression>())
        {
            // Check for pointer types (marked with "*")
            if (boundType->parts.size() == 1 && boundType->parts[0] == "*")
            {
                if (!boundType->typeArguments.empty())
                {
                    TypePtr pointeeType = resolve_type_expression(boundType->typeArguments[0]);
                    if (pointeeType)
                    {
                        return typeSystem.get_pointer(pointeeType);
                    }
                }
                return typeSystem.get_unresolved();
            }

            // Check for array types (marked with "[]")
            if (boundType->parts.size() == 1 && boundType->parts[0] == "[]")
            {
                if (!boundType->typeArguments.empty())
                {
                    TypePtr elementType = resolve_type_expression(boundType->typeArguments[0]);
                    if (elementType)
                    {
                        // Extract array size if present
                        int32_t arraySize = -1; // Default to unsized/dynamic
                        if (boundType->arraySize)
                        {
                            // If it's a literal, extract the constant value
                            if (auto literal = boundType->arraySize->as<BoundLiteralExpression>())
                            {
                                if (std::holds_alternative<int64_t>(literal->constantValue))
                                {
                                    arraySize = static_cast<int32_t>(std::get<int64_t>(literal->constantValue));
                                }
                            }
                        }
                        return typeSystem.get_array(elementType, arraySize);
                    }
                }
                return typeSystem.get_unresolved();
            }

            // Look up the type symbol
            Symbol *symbol = resolve_qualified_name(boundType->parts);


            if (symbol)
            {
                if (auto typeSymbol = symbol->as<TypeSymbol>())
                {
                    return typeSymbol->type;
                }
            }

            // Check for primitive types
            if (boundType->parts.size() == 1)
            {
                TypePtr primitive = typeSystem.get_primitive(boundType->parts[0]);
                if (primitive)
                {
                    return primitive;
                }
            }
        }

        return typeSystem.get_unresolved();
    }

    TypePtr TypeResolver::infer_return_type(BoundStatement *body)
    {
        if (!body)
            return typeSystem.get_void();

        // Find all return statements in the body
        class ReturnTypeFinder : public DefaultBoundVisitor
        {
        public:
            TypeResolver *resolver;
            TypePtr commonType = nullptr;
            bool hasVoidReturn = false;

            ReturnTypeFinder(TypeResolver *r) : resolver(r) {}

            void visit(BoundReturnStatement *node) override
            {
                if (node->value)
                {
                    TypePtr valueType = resolver->apply_substitution(node->value->type);

                    if (valueType && !valueType->is<UnresolvedType>() && valueType->is_void())
                    {
                        resolver->report_error(node, "Cannot return void expression");
                        return;
                    }

                    if (!commonType)
                    {
                        commonType = valueType;
                    }
                    else
                    {
                        resolver->unify(commonType, valueType, node, "return type inference");
                        commonType = resolver->apply_substitution(commonType);
                    }
                }
                else
                {
                    hasVoidReturn = true;
                }
            }

            void visit(BoundFunctionDeclaration *node) override
            {
                // Don't visit nested functions
            }
        };

        ReturnTypeFinder finder(this);
        body->accept(&finder);

        if (finder.commonType && !finder.hasVoidReturn)
            return finder.commonType;
        else if (finder.hasVoidReturn && !finder.commonType)
            return typeSystem.get_void();
        else if (finder.commonType && finder.hasVoidReturn)
        {
            report_error(body, "Inconsistent return types: both void and non-void returns");
            return finder.commonType;
        }
        else
            return typeSystem.get_void();
    }

    ValueCategory TypeResolver::compute_value_category(BoundExpression *expr, Symbol *symbol)
    {
        if (!expr)
            return ValueCategory::RValue;

        // Indexing is always lvalue
        if (expr->is<BoundIndexExpression>())
            return ValueCategory::LValue;

        // 'this' is always lvalue
        if (expr->is<BoundThisExpression>())
            return ValueCategory::LValue;

        // Dereference produces lvalue
        if (auto unary = expr->as<BoundUnaryExpression>())
        {
            if (unary->operatorKind == UnaryOperatorKind::Dereference)
                return ValueCategory::LValue;
        }

        // Check symbol type
        if (symbol)
        {
            if (symbol->is<VariableSymbol>())
                return ValueCategory::LValue;

            if (auto prop = symbol->as<PropertySymbol>())
            {
                if (prop->has_setter)
                    return ValueCategory::LValue;
            }
        }

        return ValueCategory::RValue;
    }

    #pragma region Conversion Checking

    ConversionKind TypeResolver::check_conversion(TypePtr from, TypePtr to)
    {
        if (!from || !to)
            return ConversionKind::NoConversion;

        TypePtr canonicalFrom = apply_substitution(from);
        TypePtr canonicalTo = apply_substitution(to);

        // Can't determine conversion for unresolved types
        if (canonicalFrom->is<UnresolvedType>() || canonicalTo->is<UnresolvedType>())
            return ConversionKind::NoConversion;

        return Conversions::classify_conversion(canonicalFrom, canonicalTo);
    }

    bool TypeResolver::check_implicit_conversion(TypePtr from, TypePtr to, BoundNode *node, const std::string &context)
    {
        TypePtr canonicalFrom = apply_substitution(from);
        TypePtr canonicalTo = apply_substitution(to);

        // Check for null types (can happen with external functions that haven't been fully resolved)
        if (!canonicalFrom || !canonicalTo)
        {
            return false; // Cannot determine conversion with null types
        }

        // Don't report errors if types are still being inferred
        if (canonicalFrom->is<UnresolvedType>() || canonicalTo->is<UnresolvedType>())
        {
            return false; // Return false to indicate conversion not yet determined
        }

        ConversionKind kind = check_conversion(canonicalFrom, canonicalTo);

        if (Conversions::is_implicit_conversion(kind))
        {
            return true;
        }
        else if (Conversions::is_explicit_conversion(kind))
        {
            report_error(node, context + ": Implicit conversion from '" + canonicalFrom->get_name() +
                                   "' to '" + canonicalTo->get_name() + "' not allowed - explicit cast required");
            return false;
        }
        else
        {
            // Special error message for null-to-value-type
            if (auto* fromPrim = canonicalFrom->as<PrimitiveType>()) {
                if (fromPrim->kind == LiteralKind::Null) {
                    if (auto* toNamed = canonicalTo->as<NamedType>()) {
                        if (toNamed->symbol && !toNamed->symbol->is_ref()) {
                            report_error(node, "Cannot assign 'null' to value type '" +
                                               canonicalTo->get_name() + "'");
                            return false;
                        }
                    }
                }
            }

            report_error(node, context + ": Cannot convert '" + canonicalFrom->get_name() +
                                   "' to '" + canonicalTo->get_name() + "'");
            return false;
        }
    }

    bool TypeResolver::check_explicit_conversion(TypePtr from, TypePtr to, BoundNode *node, const std::string &context)
    {
        TypePtr canonicalFrom = apply_substitution(from);
        TypePtr canonicalTo = apply_substitution(to);

        if (canonicalFrom->is<UnresolvedType>() || canonicalTo->is<UnresolvedType>())
        {
            return false;
        }

        auto is_integer = [](TypePtr t) -> bool {
            if (auto* prim = t->as<PrimitiveType>()) {
                switch (prim->kind) {
                    case LiteralKind::I32:
                    case LiteralKind::Char:
                        return true;
                    default:
                        return false;
                }
            }
            return false;
        };

        if (canonicalFrom->is<PointerType>() && is_integer(canonicalTo))
        {
            return true;
        }

        if (is_integer(canonicalFrom) && canonicalTo->is<PointerType>())
        {
            return true;
        }

        ConversionKind kind = check_conversion(canonicalFrom, canonicalTo);

        if (Conversions::is_conversion_possible(kind))
        {
            return true;
        }
        else
        {
            report_error(node, context + ": Cannot convert '" + canonicalFrom->get_name() +
                                   "' to '" + canonicalTo->get_name() + "' even with explicit cast");
            return false;
        }
    }

    #pragma region Error Reporting

    void TypeResolver::report_error(BoundNode *node, const std::string &message)
    {
        SourceRange loc = node ? node->location : SourceRange();
        error(message, loc);
    }

    void TypeResolver::report_final_errors()
    {
        for (auto constraint : pendingConstraints)
        {
            // Find the node that has this unresolved type
            auto nodeIt = constraintNodes.find(constraint);
            if (nodeIt != constraintNodes.end() && nodeIt->second)
            {
                BoundNode* node = nodeIt->second;
                std::string message = "Could not infer type";

                // Try to add more context about what variable/expression this is
                if (auto varDecl = node->as<BoundVariableDeclaration>())
                {
                    message += " for variable '" + varDecl->name + "'";
                }
                else if (auto nameExpr = node->as<BoundNameExpression>())
                {
                    if (!nameExpr->parts.empty())
                    {
                        message += " for name '" + nameExpr->parts.back() + "'";
                    }
                }
                else if (auto memberExpr = node->as<BoundMemberAccessExpression>())
                {
                    message += " for member '" + memberExpr->memberName + "'";
                }
                else
                {
                    // For other expressions, just mention it's an expression
                    message += " for expression";
                }

                error(message, node->location);
            }
            else
            {
                // Fallback to old error message if we can't find the node
                error("Could not infer type: " + constraint->get_name(), SourceRange());
            }
        }
    }

    #pragma region Expression Visitors

    void TypeResolver::visit(BoundLiteralExpression *node)
    {
        // Determine type from literal kind
        TypePtr type = nullptr;
        switch (node->literalKind)
        {
        case LiteralKind::I32:
            type = typeSystem.get_i32();
            break;
        case LiteralKind::F32:
            type = typeSystem.get_f32();
            break;
        case LiteralKind::Bool:
            type = typeSystem.get_bool();
            break;
        case LiteralKind::Char:
            type = typeSystem.get_primitive("char");
            break;
        case LiteralKind::String:
            // String literals are char* (pointer to null-terminated char array)
            type = typeSystem.get_primitive("string");
            break;
        case LiteralKind::Null:
            // Null literal has its own type, convertible to any pointer/reference type
            type = typeSystem.get_null();
            break;
        default:
            type = typeSystem.get_unresolved();
            break;
        }

        annotate_expression(node, type);
    }

    void TypeResolver::visit(BoundNameExpression *node)
    {
        // Resolve symbol if not already resolved
        if (!node->symbol)
        {
            node->symbol = resolve_qualified_name(node->parts);
            if (!node->symbol)
            {
                report_error(node, "Undefined symbol: " +
                                       (node->parts.empty() ? "<empty>" : node->parts.back()));
                annotate_expression(node, typeSystem.get_unresolved());
                return;
            }
        }

        // Extract type from symbol
        if (auto typed = node->symbol->as<VariableSymbol>())
        {
            annotate_expression(node, typed->type, node->symbol);
        }
        else if (auto function = node->symbol->as<FunctionSymbol>())
        {
            // Create a proper function type for this function reference
            std::vector<TypePtr> paramTypes;
            for (auto param : function->parameters)
            {
                if (param && param->type)
                {
                    paramTypes.push_back(apply_substitution(param->type));
                }
            }
            TypePtr funcType = typeSystem.get_function(
                apply_substitution(function->return_type), 
                paramTypes
            );
            annotate_expression(node, funcType, node->symbol);
        }
        else
        {
            annotate_expression(node, typeSystem.get_unresolved());
        }
    }

    void TypeResolver::visit(BoundBinaryExpression *node)
    {
        // Visit operands
        if (node->left)
            node->left->accept(this);
        if (node->right)
            node->right->accept(this);

        TypePtr leftType = node->left ? apply_substitution(node->left->type) : nullptr;
        TypePtr rightType = node->right ? apply_substitution(node->right->type) : nullptr;

        if (!leftType || !rightType)
        {
            annotate_expression(node, typeSystem.get_unresolved());
            return;
        }

        // Handle comparison operators
        switch (node->operatorKind)
        {
        case BinaryOperatorKind::Equals:
        case BinaryOperatorKind::NotEquals:
        case BinaryOperatorKind::LessThan:
        case BinaryOperatorKind::LessThanOrEqual:
        case BinaryOperatorKind::GreaterThan:
        case BinaryOperatorKind::GreaterThanOrEqual:
        {
            // Check if types are comparable
            if (!leftType->is<UnresolvedType>() && !rightType->is<UnresolvedType>())
            {
                ConversionKind ltr = check_conversion(leftType, rightType);
                ConversionKind rtl = check_conversion(rightType, leftType);

                if (!Conversions::is_implicit_conversion(ltr) &&
                    !Conversions::is_implicit_conversion(rtl))
                {
                    // Check for null comparison with value type and give helpful message
                    auto* leftPrim = leftType->as<PrimitiveType>();
                    auto* rightPrim = rightType->as<PrimitiveType>();
                    auto* leftNamed = leftType->as<NamedType>();
                    auto* rightNamed = rightType->as<NamedType>();

                    bool leftIsNull = leftPrim && leftPrim->kind == LiteralKind::Null;
                    bool rightIsNull = rightPrim && rightPrim->kind == LiteralKind::Null;

                    if (leftIsNull && rightNamed && rightNamed->symbol && !rightNamed->symbol->is_ref()) {
                        report_error(node, "Cannot compare 'null' with value type '" + rightType->get_name() + "'");
                    } else if (rightIsNull && leftNamed && leftNamed->symbol && !leftNamed->symbol->is_ref()) {
                        report_error(node, "Cannot compare value type '" + leftType->get_name() + "' with 'null'");
                    } else {
                        report_error(node, "Cannot compare '" + leftType->get_name() +
                                               "' and '" + rightType->get_name() + "'");
                    }
                }
            }
            else
            {
                unify(leftType, rightType, node, "comparison");
            }
            annotate_expression(node, typeSystem.get_bool());
            break;
        }

        case BinaryOperatorKind::Add:
        case BinaryOperatorKind::Subtract:
        {
            // Handle pointer arithmetic: pointer +/- integer
            TypePtr resultType = nullptr;

            if (!leftType->is<UnresolvedType>() && !rightType->is<UnresolvedType>())
            {
                // Helper lambda to check if a type is an integer
                auto is_integer = [](TypePtr t) -> bool {
                    if (auto* prim = t->as<PrimitiveType>()) {
                        switch (prim->kind) {
                            case LiteralKind::I32:
                            case LiteralKind::Char:
                                return true;
                            default:
                                return false;
                        }
                    }
                    return false;
                };

                // Check for pointer + integer or integer + pointer
                if (leftType->is<PointerType>() && is_integer(rightType))
                {
                    // pointer + integer = pointer
                    resultType = leftType;
                }
                else if (node->operatorKind == BinaryOperatorKind::Add &&
                         rightType->is<PointerType>() && is_integer(leftType))
                {
                    // integer + pointer = pointer (only for addition)
                    resultType = rightType;
                }
                else if (leftType->is<PointerType>() && rightType->is<PointerType>())
                {
                    // pointer - pointer = integer (ptrdiff_t, represented as i32)
                    if (node->operatorKind == BinaryOperatorKind::Subtract)
                    {
                        // TODO: Verify pointers are compatible types
                        resultType = typeSystem.get_i32();
                    }
                    else
                    {
                        report_error(node, "Cannot add two pointers");
                        resultType = typeSystem.get_unresolved();
                    }
                }
                else
                {
                    // Regular arithmetic between non-pointer types
                    ConversionKind ltr = check_conversion(leftType, rightType);
                    ConversionKind rtl = check_conversion(rightType, leftType);

                    if (Conversions::is_implicit_conversion(rtl))
                        resultType = leftType; // Left is wider
                    else if (Conversions::is_implicit_conversion(ltr))
                        resultType = rightType; // Right is wider
                    else if (ltr == ConversionKind::Identity)
                        resultType = leftType; // Same type
                    else
                    {
                        report_error(node, "Incompatible types for binary operator: '" +
                                               leftType->get_name() + "' and '" + rightType->get_name() + "'");
                        resultType = typeSystem.get_unresolved();
                    }
                }
            }
            else
            {
                unify(leftType, rightType, node, "binary operation");
                resultType = apply_substitution(leftType);
            }

            annotate_expression(node, resultType);
            break;
        }

        default: // Other arithmetic operators (multiply, divide, modulo, bitwise, etc.)
        {
            // Determine result type
            TypePtr resultType = nullptr;

            if (!leftType->is<UnresolvedType>() && !rightType->is<UnresolvedType>())
            {
                ConversionKind ltr = check_conversion(leftType, rightType);
                ConversionKind rtl = check_conversion(rightType, leftType);

                if (Conversions::is_implicit_conversion(rtl))
                    resultType = leftType; // Left is wider
                else if (Conversions::is_implicit_conversion(ltr))
                    resultType = rightType; // Right is wider
                else if (ltr == ConversionKind::Identity)
                    resultType = leftType; // Same type
                else
                {
                    report_error(node, "Incompatible types for binary operator: '" +
                                           leftType->get_name() + "' and '" + rightType->get_name() + "'");
                    resultType = typeSystem.get_unresolved();
                }
            }
            else
            {
                unify(leftType, rightType, node, "binary operation");
                resultType = apply_substitution(leftType);
            }

            annotate_expression(node, resultType);
            break;
        }
        }
    }

    void TypeResolver::visit(BoundUnaryExpression *node)
    {
        if (node->operand)
            node->operand->accept(this);

        TypePtr operandType = node->operand ? apply_substitution(node->operand->type) : nullptr;
        if (!operandType)
        {
            annotate_expression(node, typeSystem.get_unresolved());
            return;
        }

        TypePtr resultType = nullptr;

        switch (node->operatorKind)
        {
        case UnaryOperatorKind::Plus:
        case UnaryOperatorKind::Minus:
        case UnaryOperatorKind::BitwiseNot:
            resultType = operandType;
            break;

        case UnaryOperatorKind::Not:
            unify(operandType, typeSystem.get_bool(), node, "logical not");
            resultType = typeSystem.get_bool();
            break;

        case UnaryOperatorKind::AddressOf:
            resultType = typeSystem.get_pointer(operandType);
            break;

        case UnaryOperatorKind::Dereference:
            if (operandType->is<PointerType>())
            {
                auto ptrType = operandType->as<PointerType>();
                resultType = ptrType->pointee;
            }
            else
            {
                report_error(node, "Cannot dereference non-pointer type '" +
                                       operandType->get_name() + "'");
                resultType = typeSystem.get_unresolved();
            }
            break;

        default:
            report_error(node, "Unknown unary operator");
            resultType = typeSystem.get_unresolved();
            break;
        }

        annotate_expression(node, resultType);
    }

    void TypeResolver::visit(BoundAssignmentExpression *node)
    {
        if (node->target)
            node->target->accept(this);
        if (node->value)
            node->value->accept(this);

        TypePtr targetType = node->target ? apply_substitution(node->target->type) : nullptr;
        TypePtr valueType = node->value ? apply_substitution(node->value->type) : nullptr;

        if (!targetType || !valueType)
        {
            annotate_expression(node, typeSystem.get_unresolved());
            return;
        }

        // Check lvalue
        if (node->target && node->target->valueCategory != ValueCategory::LValue)
        {
            report_error(node, "Cannot assign to rvalue");
        }

        // Check type compatibility
        if (!targetType->is<UnresolvedType>() && !valueType->is<UnresolvedType>())
        {
            check_implicit_conversion(valueType, targetType, node, "assignment");
        }
        else
        {
            unify(targetType, valueType, node, "assignment");
        }

        annotate_expression(node, targetType);
    }

    void TypeResolver::visit(BoundCallExpression *node)
    {
        // Visit callee and arguments
        if (node->callee)
            node->callee->accept(this);
        for (auto arg : node->arguments)
        {
            if (arg)
                arg->accept(this);
        }

        // Collect argument types
        std::vector<TypePtr> argTypes;
        for (auto arg : node->arguments)
        {
            if (arg)
                argTypes.push_back(apply_substitution(arg->type));
        }

        // Resolve function based on callee type
        if (!node->callee)
        {
            annotate_expression(node, typeSystem.get_unresolved());
            return;
        }

        // Handle direct function calls
        if (auto nameExpr = node->callee->as<BoundNameExpression>())
        {
            if (auto func = nameExpr->symbol->as<FunctionSymbol>())
            {
                node->method = func;

                // Check parameter count
                if (func->parameters.size() != argTypes.size())
                {
                    report_error(node, "Argument count mismatch: expected " +
                                           std::to_string(func->parameters.size()) + ", got " +
                                           std::to_string(argTypes.size()));
                }
                else
                {
                    // Check argument types
                    for (size_t i = 0; i < argTypes.size(); ++i)
                    {
                        TypePtr paramType = apply_substitution(func->parameters[i]->type);
                        check_implicit_conversion(argTypes[i], paramType, node,
                                                  "argument " + std::to_string(i + 1));
                    }
                }

                annotate_expression(node, apply_substitution(func->return_type));
            }
            else
            {
                // Check if it's a container with function overloads
                if (auto container = nameExpr->symbol->as<ContainerSymbol>())
                {
                    auto overloads = container->get_functions(nameExpr->parts.back());
                    node->method = resolve_overload(overloads, argTypes);

                    if (!node->method)
                    {
                        std::stringstream ss;
                        ss << "No matching overload for '" << nameExpr->parts.back() << "' with argument types (";
                        for (size_t i = 0; i < argTypes.size(); ++i)
                        {
                            if (i > 0)
                                ss << ", ";
                            ss << argTypes[i]->get_name();
                        }
                        ss << ")";
                        report_error(node, ss.str());
                        annotate_expression(node, typeSystem.get_unresolved());
                        return;
                    }

                    annotate_expression(node, apply_substitution(node->method->return_type));
                }
                else
                {
                    report_error(node, "Expression is not callable");
                    annotate_expression(node, typeSystem.get_unresolved());
                }
            }
        }
        else if (auto memberExpr = node->callee->as<BoundMemberAccessExpression>())
        {
            // Handle method calls (object.method(...))
            if (memberExpr->member)
            {
                if (auto method = memberExpr->member->as<FunctionSymbol>())
                {
                    node->method = method;
                    
                    // Check parameter count
                    if (method->parameters.size() != argTypes.size())
                    {
                        report_error(node, "Argument count mismatch: expected " +
                                           std::to_string(method->parameters.size()) + ", got " +
                                           std::to_string(argTypes.size()));
                    }
                    else
                    {
                        // Check argument types
                        for (size_t i = 0; i < argTypes.size(); ++i)
                        {
                            TypePtr paramType = apply_substitution(method->parameters[i]->type);
                            check_implicit_conversion(argTypes[i], paramType, node,
                                                      "argument " + std::to_string(i + 1));
                        }
                    }
                    
                    annotate_expression(node, apply_substitution(method->return_type));
                }
                else if (auto container = memberExpr->member->as<ContainerSymbol>())
                {
                    // Handle overloaded methods
                    auto overloads = container->get_functions(memberExpr->memberName);
                    node->method = resolve_overload(overloads, argTypes);
                    
                    if (!node->method)
                    {
                        std::stringstream ss;
                        ss << "No matching overload for '" << memberExpr->memberName << "' with argument types (";
                        for (size_t i = 0; i < argTypes.size(); ++i)
                        {
                            if (i > 0)
                                ss << ", ";
                            ss << argTypes[i]->get_name();
                        }
                        ss << ")";
                        report_error(node, ss.str());
                        annotate_expression(node, typeSystem.get_unresolved());
                        return;
                    }
                    
                    annotate_expression(node, apply_substitution(node->method->return_type));
                }
                else
                {
                    report_error(node, "Member '" + memberExpr->memberName + "' is not callable");
                    annotate_expression(node, typeSystem.get_unresolved());
                }
            }
            else
            {
                report_error(node, "Cannot resolve member for method call");
                annotate_expression(node, typeSystem.get_unresolved());
            }
        }
        else
        {
            report_error(node, "Unsupported callee expression");
            annotate_expression(node, typeSystem.get_unresolved());
        }
    }

    void TypeResolver::visit(BoundMemberAccessExpression *node)
    {
        if (node->object)
            node->object->accept(this);

        TypePtr objectType = node->object ? apply_substitution(node->object->type) : nullptr;
        if (!objectType)
        {
            annotate_expression(node, typeSystem.get_unresolved());
            return;
        }

        // Get the type's symbol
        TypeSymbol *typeSymbol = nullptr;
        if (objectType->is<NamedType>())
        {
            auto namedType = objectType->as<NamedType>();
            typeSymbol = namedType->symbol;
        }

        if (!typeSymbol)
        {
            report_error(node, "Cannot access members of type '" + objectType->get_name() + "'");
            annotate_expression(node, typeSystem.get_unresolved());
            return;
        }

        // Look up member
        auto members = typeSymbol->get_member(node->memberName);
        if (members.empty())
        {
            report_error(node, "Member '" + node->memberName + "' not found in type '" +
                                   typeSymbol->name + "'");
            annotate_expression(node, typeSystem.get_unresolved());
            return;
        }

        node->member = members[0]; // TODO: Handle ambiguity

        // Check actual symbol types to get the type
        if (auto varSym = node->member->as<VariableSymbol>())
        {
            annotate_expression(node, varSym->type, node->member);
        }
        else if (auto funcSym = node->member->as<FunctionSymbol>())
        {
            // Create a proper function type for method reference
            std::vector<TypePtr> paramTypes;
            for (auto param : funcSym->parameters)
            {
                if (param && param->type)
                {
                    paramTypes.push_back(apply_substitution(param->type));
                }
            }
            TypePtr funcType = typeSystem.get_function(
                apply_substitution(funcSym->return_type),
                paramTypes
            );
            annotate_expression(node, funcType, node->member);
        }
        else
        {
            annotate_expression(node, typeSystem.get_unresolved());
        }
    }

    void TypeResolver::visit(BoundIndexExpression *node)
    {
        if (node->object)
            node->object->accept(this);
        if (node->index)
            node->index->accept(this);

        TypePtr objectType = node->object ? apply_substitution(node->object->type) : nullptr;
        TypePtr indexType = node->index ? apply_substitution(node->index->type) : nullptr;

        if (!objectType || !indexType)
        {
            annotate_expression(node, typeSystem.get_unresolved());
            return;
        }

        // Check for array type
        if (objectType->is<ArrayType>())
        {
            auto arrayType = objectType->as<ArrayType>();
            unify(indexType, typeSystem.get_i32(), node, "array index");
            annotate_expression(node, arrayType->element);
        }
        // Check for pointer type (pointer arithmetic)
        else if (objectType->is<PointerType>())
        {
            auto ptrType = objectType->as<PointerType>();
            unify(indexType, typeSystem.get_i32(), node, "pointer index");
            annotate_expression(node, ptrType->pointee);
        }
        else
        {
            report_error(node, "Cannot index type '" + objectType->get_name() + "'");
            annotate_expression(node, typeSystem.get_unresolved());
        }
    }

    void TypeResolver::visit(BoundNewExpression *node)
    {
        if (node->typeExpression)
            node->typeExpression->accept(this);
        for (auto arg : node->arguments)
        {
            if (arg)
                arg->accept(this);
        }

        TypePtr type = resolve_type_expression(node->typeExpression);
        annotate_expression(node, type);

        // Resolve constructor (must happen here after argument types are known)
        if (!node->arguments.empty() || !node->constructor)
        {
            // Build argument type list
            std::vector<TypePtr> argTypes;
            for (auto arg : node->arguments)
            {
                if (arg && arg->type)
                {
                    argTypes.push_back(apply_substitution(arg->type));
                }
            }

            // Find the type symbol to get constructors
            if (auto named_type = type->as<NamedType>())
            {
                if (auto type_symbol = named_type->symbol)
                {
                    // Get all constructors (they're named "New")
                    auto constructors = type_symbol->get_functions("New");

                    // Filter to only constructors
                    std::vector<FunctionSymbol*> ctors;
                    for (auto* func : constructors)
                    {
                        if (func->is_constructor)
                        {
                            ctors.push_back(func);
                        }
                    }

                    // Resolve to best matching constructor
                    if (!ctors.empty())
                    {
                        node->constructor = resolve_overload(ctors, argTypes);
                    }

                    // Error if we need a constructor but couldn't find one
                    if (!node->constructor && !argTypes.empty())
                    {
                        std::string typeName = type->get_name();
                        std::stringstream ss;
                        ss << "No matching constructor found for type '" << typeName << "' with arguments (";
                        for (size_t i = 0; i < argTypes.size(); ++i)
                        {
                            if (i > 0) ss << ", ";
                            ss << argTypes[i]->get_name();
                        }
                        ss << ")";

                        // Look for a constructor with matching argument count to suggest correct types
                        FunctionSymbol* suggestion = nullptr;
                        for (auto* ctor : ctors)
                        {
                            if (ctor->parameters.size() == argTypes.size())
                            {
                                suggestion = ctor;
                                break;
                            }
                        }

                        if (suggestion)
                        {
                            ss << ". Did you mean (";
                            for (size_t i = 0; i < suggestion->parameters.size(); ++i)
                            {
                                if (i > 0) ss << ", ";
                                TypePtr paramType = apply_substitution(suggestion->parameters[i]->type);
                                ss << paramType->get_name();
                            }
                            ss << ")?";
                        }

                        report_error(node, ss.str());
                    }

                    // Type inference for constructor parameters
                    if (node->constructor)
                    {
                        for (size_t i = 0; i < argTypes.size() && i < node->constructor->parameters.size(); ++i)
                        {
                            TypePtr paramType = apply_substitution(node->constructor->parameters[i]->type);
                            if (paramType->is<UnresolvedType>())
                            {
                                unify(paramType, argTypes[i], nullptr, "constructor parameter inference");
                            }
                        }
                    }
                }
            }
        }
    }

    void TypeResolver::visit(BoundArrayCreationExpression *node)
    {
        if (node->elementTypeExpression)
            node->elementTypeExpression->accept(this);
        if (node->size)
            node->size->accept(this);
        for (auto init : node->initializers)
        {
            if (init)
                init->accept(this);
        }

        TypePtr elementType = resolve_type_expression(node->elementTypeExpression);

        // If no explicit element type, infer from initializers
        if (elementType->is<UnresolvedType>() && !node->initializers.empty())
        {
            // Try to infer from first non-null initializer
            for (auto init : node->initializers)
            {
                if (init)
                {
                    TypePtr initType = apply_substitution(init->type);
                    if (initType && !initType->is<UnresolvedType>())
                    {
                        elementType = initType;
                        break;
                    }
                }
            }
        }

        // Check size is integer
        if (node->size)
        {
            TypePtr sizeType = apply_substitution(node->size->type);
            unify(sizeType, typeSystem.get_i32(), node, "array size");
        }

        // Check initializers match element type
        for (auto init : node->initializers)
        {
            if (init)
            {
                TypePtr initType = apply_substitution(init->type);
                if (!elementType->is<UnresolvedType>())
                {
                    check_implicit_conversion(initType, elementType, node, "array initializer");
                }
                else
                {
                    // Unify all initializers with the element type for inference
                    unify(initType, elementType, node, "array element type inference");
                }
            }
        }

        int arraySize = node->size ? -1 : node->initializers.size(); // TODO: Evaluate constant size
        TypePtr arrayType = typeSystem.get_array(elementType, arraySize);
        annotate_expression(node, arrayType);
    }

    void TypeResolver::visit(BoundCastExpression *node)
    {
        if (node->expression)
            node->expression->accept(this);
        if (node->targetTypeExpression)
            node->targetTypeExpression->accept(this);

        TypePtr sourceType = node->expression ? apply_substitution(node->expression->type) : nullptr;
        TypePtr targetType = resolve_type_expression(node->targetTypeExpression);

        if (sourceType && targetType)
        {
            node->conversionKind = check_conversion(sourceType, targetType);
            if (!sourceType->is<UnresolvedType>() && !targetType->is<UnresolvedType>())
            {
                check_explicit_conversion(sourceType, targetType, node, "cast");
            }
        }

        annotate_expression(node, targetType);
    }

    void TypeResolver::visit(BoundThisExpression *node)
    {
        if (!currentType)
        {
            report_error(node, "'this' used outside of type context");
            annotate_expression(node, typeSystem.get_unresolved());
            return;
        }

        node->containingType = currentType;
        annotate_expression(node, currentType->type);
    }

    void TypeResolver::visit(BoundConversionExpression *node)
    {
        if (node->expression)
            node->expression->accept(this);

        // Type should already be set by whoever created this node
        if (!node->type)
        {
            annotate_expression(node, typeSystem.get_unresolved());
        }
    }

    void TypeResolver::visit(BoundTypeExpression *node)
    {
        // Type expressions resolve to themselves
        node->resolvedTypeReference = resolve_type_expression(node);
        // The type of a type expression is Type<T>, not T itself
        node->type = typeSystem.get_type_type(node->resolvedTypeReference);
    }

    #pragma region Statement Visitors

    void TypeResolver::visit(BoundBlockStatement *node)
    {
        // Push block scope if present
        Symbol* prevScope = nullptr;
        if (node->symbol)
        {
            prevScope = symbolTable.get_current_scope();
            symbolTable.push_scope(node->symbol);
        }

        for (auto stmt : node->statements)
        {
            if (stmt)
                stmt->accept(this);
        }

        // Restore previous scope
        if (node->symbol && prevScope)
        {
            symbolTable.pop_scope();
        }
    }

    void TypeResolver::visit(BoundExpressionStatement *node)
    {
        if (node->expression)
            node->expression->accept(this);
    }

    void TypeResolver::visit(BoundIfStatement *node)
    {
        if (node->condition)
            node->condition->accept(this);

        TypePtr condType = node->condition ? apply_substitution(node->condition->type) : nullptr;
        if (condType)
        {
            unify(condType, typeSystem.get_bool(), node, "if condition");
        }

        if (node->thenStatement)
            node->thenStatement->accept(this);
        if (node->elseStatement)
            node->elseStatement->accept(this);
    }

    void TypeResolver::visit(BoundWhileStatement *node)
    {
        if (node->condition)
            node->condition->accept(this);

        TypePtr condType = node->condition ? apply_substitution(node->condition->type) : nullptr;
        if (condType)
        {
            unify(condType, typeSystem.get_bool(), node, "while condition");
        }

        if (node->body)
            node->body->accept(this);
    }

    void TypeResolver::visit(BoundForStatement *node)
    {
        if (node->initializer)
            node->initializer->accept(this);

        if (node->condition)
        {
            node->condition->accept(this);
            TypePtr condType = apply_substitution(node->condition->type);
            if (condType)
            {
                unify(condType, typeSystem.get_bool(), node, "for condition");
            }
        }

        for (auto inc : node->incrementors)
        {
            if (inc)
                inc->accept(this);
        }

        if (node->body)
            node->body->accept(this);
    }

    void TypeResolver::visit(BoundBreakStatement *node)
    {
        // Nothing to resolve
    }

    void TypeResolver::visit(BoundContinueStatement *node)
    {
        // Nothing to resolve
    }

    void TypeResolver::visit(BoundReturnStatement *node)
    {
        if (node->value)
            node->value->accept(this);

        if (!currentFunction)
        {
            report_error(node, "Return statement outside of function");
            return;
        }

        TypePtr expectedType = apply_substitution(currentFunction->return_type);

        if (node->value)
        {
            TypePtr valueType = apply_substitution(node->value->type);

            // Check for void return
            if (valueType && !valueType->is<UnresolvedType>() && valueType->is_void())
            {
                report_error(node, "Cannot return void expression");
                return;
            }

            if (expectedType && !expectedType->is<UnresolvedType>())
            {
                check_implicit_conversion(valueType, expectedType, node, "return value");
            }
            else
            {
                unify(expectedType, valueType, node, "return type");
            }
        }
        else
        {
            // Void return
            if (expectedType && !expectedType->is<UnresolvedType>() && !expectedType->is_void())
            {
                report_error(node, "Cannot return without value from non-void function");
            }
            else
            {
                unify(expectedType, typeSystem.get_void(), node, "void return");
            }
        }
    }

    void TypeResolver::visit(BoundUsingStatement *node)
    {
        // TODO: Implement when adding using support
    }

    #pragma region Declaration Visitors

    void TypeResolver::visit(BoundVariableDeclaration *node)
    {
        // Resolve type if specified
        if (node->typeExpression)
        {
            node->typeExpression->accept(this);

            if (node->symbol)
            {
                if (auto varSym = node->symbol->as<VariableSymbol>())
                {
                    varSym->type = resolve_type_expression(node->typeExpression);
                }
            }
        }

        // Process initializer
        if (node->initializer)
        {
            node->initializer->accept(this);

            if (node->symbol && node->symbol->as<VariableSymbol>())
            {
                auto varSym = node->symbol->as<VariableSymbol>();

                if (!node->typeExpression || varSym->type->is<UnresolvedType>())
                {
                    // Type inference from initializer
                    varSym->type = apply_substitution(node->initializer->type);
                }
                else
                {
                    // Check type compatibility
                    check_implicit_conversion(node->initializer->type, varSym->type, node, "variable initialization");
                }

                // Track if the variable still has an unresolved type
                if (varSym->type->is<UnresolvedType>())
                {
                    pendingConstraints.insert(varSym->type);
                    constraintNodes[varSym->type] = node;
                }
            }
        }
    }

    void TypeResolver::visit(BoundFunctionDeclaration *node)
    {
        // Save context
        auto prevFunction = currentFunction;
        auto prevScope = symbolTable.get_current_scope();

        currentFunction = node->symbol ? node->symbol->as<FunctionSymbol>() : nullptr;

        if (currentFunction)
        {
            // Set function as current scope
            symbolTable.push_scope(currentFunction);

            // Resolve return type
            if (node->returnTypeExpression)
            {
                node->returnTypeExpression->accept(this);
                currentFunction->return_type = resolve_type_expression(node->returnTypeExpression);
            }

            // Visit parameters
            for (auto param : node->parameters)
            {
                if (param)
                    param->accept(this);
            }

            // Visit body
            if (node->body)
            {
                node->body->accept(this);

                // Infer return type if needed
                if (currentFunction->return_type->is<UnresolvedType>())
                {
                    currentFunction->return_type = infer_return_type(node->body);
                }
            }

            // Restore scope
            symbolTable.pop_scope();
        }

        // Restore context
        currentFunction = prevFunction;
    }

    void TypeResolver::visit(BoundPropertyDeclaration *node)
    {
        if (node->typeExpression)
        {
            node->typeExpression->accept(this);

            if (node->symbol)
            {
                if (auto propSym = node->symbol->as<PropertySymbol>())
                {
                    propSym->type = resolve_type_expression(node->typeExpression);

                    // Also update the getter function's return type if it exists
                    if (node->getter && node->getter->function_symbol)
                    {
                        node->getter->function_symbol->return_type = propSym->type;
                    }
                }
            }
        }

        // Process getter first to infer type if needed
        if (node->getter)
        {
            if (node->getter->expression)
            {
                node->getter->expression->accept(this);
                
                // Infer property type from getter expression if no explicit type
                if (node->symbol && (!node->typeExpression || node->symbol->as<PropertySymbol>()->type->is<UnresolvedType>()))
                {
                    if (auto propSym = node->symbol->as<PropertySymbol>())
                    {
                        TypePtr exprType = apply_substitution(node->getter->expression->type);
                        if (exprType && !exprType->is<UnresolvedType>())
                        {
                            propSym->type = exprType;
                            
                            // Also update the getter function's return type
                            // TODO: Maybe we sould actually be creating function symbols out of properties to begin with?
                            if (node->getter && node->getter->function_symbol)
                            {
                                node->getter->function_symbol->return_type = exprType;
                            }
                        }
                    }
                }
            }
            if (node->getter->body)
                node->getter->body->accept(this);
        }
        
        if (node->setter)
        {
            if (node->setter->expression)
                node->setter->expression->accept(this);
            if (node->setter->body)
                node->setter->body->accept(this);
        }
    }

    void TypeResolver::visit(BoundTypeDeclaration *node)
    {
        // Save context
        auto prevType = currentType;
        auto prevScope = symbolTable.get_current_scope();

        currentType = node->symbol ? node->symbol->as<TypeSymbol>() : nullptr;

        if (currentType)
        {
            // This creates the self-referential structure where a type knows itself
            if (currentType->type && currentType->type->is<UnresolvedType>())
            {
                // Save the old unresolved type before we replace it
                TypePtr oldUnresolved = currentType->type;
                
                // Replace the unresolved type with a proper NamedType pointing to this symbol
                TypePtr namedType = typeSystem.get_named(currentType);
                
                // Update the symbol's type
                currentType->type = namedType;
                
                // Also update the substitution map so all references get resolved
                substitution[oldUnresolved] = namedType;
            }

            // Set type as current scope
            symbolTable.push_scope(currentType);

            // Visit members
            for (auto member : node->members)
            {
                if (member)
                    member->accept(this);
            }

            // Restore scope
            symbolTable.pop_scope();
        }

        // Restore context
        currentType = prevType;
    }

    void TypeResolver::visit(BoundNamespaceDeclaration *node)
    {
        // Save current scope
        auto prevScope = symbolTable.get_current_scope();

        if (node->symbol)
        {
            // Set namespace as current scope
            symbolTable.push_scope(node->symbol);

            // Visit members
            for (auto member : node->members)
            {
                if (member)
                    member->accept(this);
            }

            // Restore scope
            symbolTable.pop_scope();
        }
    }

    void TypeResolver::visit(BoundCompilationUnit *node)
    {
        // Process all top-level statements
        for (auto stmt : node->statements)
        {
            if (stmt)
                stmt->accept(this);
        }
    }

} // namespace Fern
