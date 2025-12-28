#include "conversion_inserter.hpp"

namespace Fern
{
    BoundExpression* ConversionInserter::wrap_if_needed(BoundExpression* expr, TypePtr targetType)
    {
        if (!expr || !targetType || !expr->type)
            return expr;

        // No conversion needed if types match
        if (expr->type == targetType || expr->type->get_name() == targetType->get_name())
            return expr;

        // Check if implicit conversion is valid
        ConversionKind kind = Conversions::classify_conversion(expr->type, targetType);
        if (!Conversions::is_implicit_conversion(kind) || kind == ConversionKind::Identity)
            return expr;

        // Create conversion node
        auto* conv = arena.make<BoundConversionExpression>();
        conv->expression = expr;
        conv->conversionKind = kind;
        conv->type = targetType;
        conv->location = expr->location;
        conv->valueCategory = ValueCategory::RValue;
        return conv;
    }

    void ConversionInserter::transform(BoundCompilationUnit* unit)
    {
        if (unit)
            unit->accept(this);
    }

    #pragma region Expression Visitors

    void ConversionInserter::visit(BoundBinaryExpression* node)
    {
        if (node->left)
            node->left->accept(this);
        if (node->right)
            node->right->accept(this);

        // For arithmetic operations, convert operands to result type
        if (node->type && !node->type->is<UnresolvedType>())
        {
            node->left = wrap_if_needed(node->left, node->type);
            node->right = wrap_if_needed(node->right, node->type);
        }
    }

    void ConversionInserter::visit(BoundUnaryExpression* node)
    {
        if (node->operand)
            node->operand->accept(this);
    }

    void ConversionInserter::visit(BoundAssignmentExpression* node)
    {
        if (node->target)
            node->target->accept(this);
        if (node->value)
            node->value->accept(this);

        // Convert value to target type
        if (node->target && node->target->type)
        {
            node->value = wrap_if_needed(node->value, node->target->type);
        }
    }

    void ConversionInserter::visit(BoundCallExpression* node)
    {
        if (node->callee)
            node->callee->accept(this);
        for (auto arg : node->arguments)
        {
            if (arg)
                arg->accept(this);
        }

        // Convert arguments to parameter types
        if (node->method)
        {
            for (size_t i = 0; i < node->arguments.size() && i < node->method->parameters.size(); ++i)
            {
                TypePtr paramType = node->method->parameters[i]->type;
                node->arguments[i] = wrap_if_needed(node->arguments[i], paramType);
            }
        }
    }

    void ConversionInserter::visit(BoundMemberAccessExpression* node)
    {
        if (node->object)
            node->object->accept(this);
    }

    void ConversionInserter::visit(BoundIndexExpression* node)
    {
        if (node->object)
            node->object->accept(this);
        if (node->index)
            node->index->accept(this);
    }

    void ConversionInserter::visit(BoundNewExpression* node)
    {
        if (node->typeExpression)
            node->typeExpression->accept(this);
        for (auto arg : node->arguments)
        {
            if (arg)
                arg->accept(this);
        }

        // Convert constructor arguments to parameter types
        if (node->constructor)
        {
            for (size_t i = 0; i < node->arguments.size() && i < node->constructor->parameters.size(); ++i)
            {
                TypePtr paramType = node->constructor->parameters[i]->type;
                node->arguments[i] = wrap_if_needed(node->arguments[i], paramType);
            }
        }
    }

    void ConversionInserter::visit(BoundArrayCreationExpression* node)
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

        // Convert initializers to element type
        if (auto arrayType = node->type ? node->type->as<ArrayType>() : nullptr)
        {
            for (size_t i = 0; i < node->initializers.size(); ++i)
            {
                node->initializers[i] = wrap_if_needed(node->initializers[i], arrayType->element);
            }
        }
    }

    void ConversionInserter::visit(BoundCastExpression* node)
    {
        if (node->expression)
            node->expression->accept(this);
    }

    void ConversionInserter::visit(BoundConversionExpression* node)
    {
        if (node->expression)
            node->expression->accept(this);
    }

    #pragma region Statement Visitors

    void ConversionInserter::visit(BoundReturnStatement* node)
    {
        if (node->value)
            node->value->accept(this);

        // Note: Would need access to current function's return type
        // For now, this is handled by the fact that TypeResolver already checked compatibility
        // The conversion will be inserted when we have context tracking
    }

    void ConversionInserter::visit(BoundBlockStatement* node)
    {
        for (auto stmt : node->statements)
        {
            if (stmt)
                stmt->accept(this);
        }
    }

    void ConversionInserter::visit(BoundExpressionStatement* node)
    {
        if (node->expression)
            node->expression->accept(this);
    }

    void ConversionInserter::visit(BoundIfStatement* node)
    {
        if (node->condition)
            node->condition->accept(this);
        if (node->thenStatement)
            node->thenStatement->accept(this);
        if (node->elseStatement)
            node->elseStatement->accept(this);
    }

    void ConversionInserter::visit(BoundWhileStatement* node)
    {
        if (node->condition)
            node->condition->accept(this);
        if (node->body)
            node->body->accept(this);
    }

    void ConversionInserter::visit(BoundForStatement* node)
    {
        if (node->initializer)
            node->initializer->accept(this);
        if (node->condition)
            node->condition->accept(this);
        for (auto inc : node->incrementors)
        {
            if (inc)
                inc->accept(this);
        }
        if (node->body)
            node->body->accept(this);
    }

    #pragma region Declaration Visitors

    void ConversionInserter::visit(BoundVariableDeclaration* node)
    {
        if (node->typeExpression)
            node->typeExpression->accept(this);
        if (node->initializer)
            node->initializer->accept(this);

        // Convert initializer to variable type
        if (node->symbol)
        {
            if (auto varSym = node->symbol->as<VariableSymbol>())
            {
                if (varSym->type && !varSym->type->is<UnresolvedType>())
                {
                    node->initializer = wrap_if_needed(node->initializer, varSym->type);
                }
            }
        }
    }

    void ConversionInserter::visit(BoundFunctionDeclaration* node)
    {
        for (auto param : node->parameters)
        {
            if (param)
                param->accept(this);
        }
        if (node->body)
            node->body->accept(this);
    }

    void ConversionInserter::visit(BoundPropertyDeclaration* node)
    {
        if (node->typeExpression)
            node->typeExpression->accept(this);
        if (node->initializer)
            node->initializer->accept(this);
        if (node->getter)
        {
            if (node->getter->expression)
                node->getter->expression->accept(this);
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

    void ConversionInserter::visit(BoundTypeDeclaration* node)
    {
        for (auto member : node->members)
        {
            if (member)
                member->accept(this);
        }
    }

    void ConversionInserter::visit(BoundNamespaceDeclaration* node)
    {
        for (auto member : node->members)
        {
            if (member)
                member->accept(this);
        }
    }

    void ConversionInserter::visit(BoundCompilationUnit* node)
    {
        for (auto stmt : node->statements)
        {
            if (stmt)
                stmt->accept(this);
        }
    }

} // namespace Fern
