#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <memory>
#include <variant>
#include <optional>
#include "common/source_location.hpp"
#include "common/token_kind.hpp"
#include "semantic/symbol.hpp"
#include "conversions.hpp"
#include "semantic/type.hpp"

namespace Fern
{
    // Forward declarations
    class BoundVisitor;
    struct BoundNode;
    struct BoundExpression;
    struct BoundStatement;
    struct BoundDeclaration;
    struct BoundCompilationUnit;
    struct BoundLiteralExpression;
    struct BoundNameExpression;
    struct BoundBinaryExpression;
    struct BoundUnaryExpression;
    struct BoundAssignmentExpression;
    struct BoundCallExpression;
    struct BoundMemberAccessExpression;
    struct BoundIndexExpression;
    struct BoundNewExpression;
    struct BoundArrayCreationExpression;
    struct BoundCastExpression;
    struct BoundThisExpression;
    struct BoundConversionExpression;
    struct BoundTypeExpression;
    struct BoundBlockStatement;
    struct BoundExpressionStatement;
    struct BoundIfStatement;
    struct BoundWhileStatement;
    struct BoundForStatement;
    struct BoundBreakStatement;
    struct BoundContinueStatement;
    struct BoundReturnStatement;
    struct BoundUsingStatement;
    struct BoundVariableDeclaration;
    struct BoundFunctionDeclaration;
    struct BoundPropertyDeclaration;
    struct BoundTypeDeclaration;
    struct BoundNamespaceDeclaration;

    class BoundVisitor
    {
    public:
        virtual ~BoundVisitor() = default;

        // Expressions
        virtual void visit(BoundLiteralExpression *node) = 0;
        virtual void visit(BoundNameExpression *node) = 0;
        virtual void visit(BoundBinaryExpression *node) = 0;
        virtual void visit(BoundUnaryExpression *node) = 0;
        virtual void visit(BoundAssignmentExpression *node) = 0;
        virtual void visit(BoundCallExpression *node) = 0;
        virtual void visit(BoundMemberAccessExpression *node) = 0;
        virtual void visit(BoundIndexExpression *node) = 0;
        virtual void visit(BoundNewExpression *node) = 0;
        virtual void visit(BoundArrayCreationExpression *node) = 0;
        virtual void visit(BoundCastExpression *node) = 0;
        virtual void visit(BoundThisExpression *node) = 0;
        virtual void visit(BoundConversionExpression *node) = 0;
        virtual void visit(BoundTypeExpression *node) = 0;

        // Statements
        virtual void visit(BoundBlockStatement *node) = 0;
        virtual void visit(BoundExpressionStatement *node) = 0;
        virtual void visit(BoundIfStatement *node) = 0;
        virtual void visit(BoundWhileStatement *node) = 0;
        virtual void visit(BoundForStatement *node) = 0;
        virtual void visit(BoundBreakStatement *node) = 0;
        virtual void visit(BoundContinueStatement *node) = 0;
        virtual void visit(BoundReturnStatement *node) = 0;
        virtual void visit(BoundUsingStatement *node) = 0;

        // Declarations
        virtual void visit(BoundVariableDeclaration *node) = 0;
        virtual void visit(BoundFunctionDeclaration *node) = 0;
        virtual void visit(BoundPropertyDeclaration *node) = 0;
        virtual void visit(BoundTypeDeclaration *node) = 0;
        virtual void visit(BoundNamespaceDeclaration *node) = 0;

        // Top-level
        virtual void visit(BoundCompilationUnit *node) = 0;
    };

    // Value categories
    enum class ValueCategory
    {
        RValue,
        LValue
    };

    // Constant value for compile-time constants
    using ConstantValue = std::variant<
        std::monostate, // not constant
        int64_t,
        uint64_t,
        double,
        bool,
        std::string>;

// Macro for accept implementation
#define BOUND_ACCEPT_VISITOR \
    inline void accept(BoundVisitor *visitor) override { visitor->visit(this); }
    // === Base Nodes ===

    struct BoundNode
    {
        SourceRange location;

        virtual ~BoundNode() = default;
        virtual void accept(BoundVisitor *visitor) = 0;

        template <typename T>
        T *as() { return dynamic_cast<T *>(this); }

        template <typename T>
        const T *as() const { return dynamic_cast<const T *>(this); }

        template <typename T>
        bool is() const { return dynamic_cast<const T *>(this) != nullptr; }
    };

    struct BoundExpression : BoundNode
    {
        TypePtr type = nullptr; // Resolved in semantic pass
        ValueCategory valueCategory = ValueCategory::RValue;
        ConstantValue constantValue;

        bool is_constant() const
        {
            return !std::holds_alternative<std::monostate>(constantValue);
        }
    };

    struct BoundStatement : BoundNode
    {
    };

    // Base for all declarations (functions, types, variables, etc.)
    struct BoundDeclaration : BoundStatement
    {
        std::string name;
        Symbol *symbol = nullptr; // Resolved in semantic pass
        ModifierKindFlags modifiers = ModifierKindFlags::None;
    };

    // === Expressions ===

    struct BoundLiteralExpression : BoundExpression
    {
        LiteralKind literalKind;
        // constantValue is stored in base class
        BOUND_ACCEPT_VISITOR
    };

    struct BoundNameExpression : BoundExpression
    {
        std::vector<std::string> parts; // e.g. ["System", "Console", "WriteLine"]
        Symbol *symbol = nullptr;       // Resolved in semantic pass
        BOUND_ACCEPT_VISITOR
    };

    struct BoundBinaryExpression : BoundExpression
    {
        BoundExpression *left = nullptr;
        BoundExpression *right = nullptr;
        BinaryOperatorKind operatorKind;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundUnaryExpression : BoundExpression
    {
        BoundExpression *operand = nullptr;
        UnaryOperatorKind operatorKind;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundAssignmentExpression : BoundExpression
    {
        BoundExpression *target = nullptr;
        BoundExpression *value = nullptr;
        AssignmentOperatorKind operatorKind;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundCallExpression : BoundExpression
    {
        BoundExpression *callee = nullptr; // Can be name, member access, etc.
        std::vector<BoundExpression *> arguments;
        FunctionSymbol *method = nullptr; // Resolved in semantic pass
        BOUND_ACCEPT_VISITOR
    };

    struct BoundMemberAccessExpression : BoundExpression
    {
        BoundExpression *object = nullptr;
        std::string memberName;
        Symbol *member = nullptr; // Could be field, property, method
        BOUND_ACCEPT_VISITOR
    };

    struct BoundIndexExpression : BoundExpression
    {
        BoundExpression *object = nullptr;
        BoundExpression *index = nullptr;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundNewExpression : BoundExpression
    {
        BoundExpression *typeExpression = nullptr; // The type to instantiate
        std::vector<BoundExpression *> arguments;
        FunctionSymbol *constructor = nullptr; // Resolved in semantic pass
        BOUND_ACCEPT_VISITOR
    };

    struct BoundArrayCreationExpression : BoundExpression
    {
        BoundExpression *elementTypeExpression = nullptr;
        BoundExpression *size = nullptr; // Can be null for initializer syntax
        std::vector<BoundExpression *> initializers;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundCastExpression : BoundExpression
    {
        BoundExpression *expression = nullptr;
        BoundExpression *targetTypeExpression = nullptr;
        ConversionKind conversionKind = ConversionKind::NoConversion; // Set in semantic pass
        BOUND_ACCEPT_VISITOR
    };

    struct BoundThisExpression : BoundExpression
    {
        TypeSymbol *containingType = nullptr; // Resolved in semantic pass
        BOUND_ACCEPT_VISITOR
    };

    struct BoundConversionExpression : BoundExpression
    {
        BoundExpression *expression = nullptr;
        ConversionKind conversionKind;
        // type is stored in base class
        BOUND_ACCEPT_VISITOR
    };

    // === Statements ===

    struct BoundBlockStatement : BoundStatement
    {
        std::vector<BoundStatement *> statements;
        Symbol *symbol = nullptr;  // The $block namespace symbol
        BOUND_ACCEPT_VISITOR
    };

    struct BoundExpressionStatement : BoundStatement
    {
        BoundExpression *expression = nullptr;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundIfStatement : BoundStatement
    {
        BoundExpression *condition = nullptr;
        BoundStatement *thenStatement = nullptr;
        BoundStatement *elseStatement = nullptr;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundWhileStatement : BoundStatement
    {
        BoundExpression *condition = nullptr;
        BoundStatement *body = nullptr;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundForStatement : BoundStatement
    {
        BoundStatement *initializer = nullptr;
        BoundExpression *condition = nullptr;
        std::vector<BoundExpression *> incrementors;
        BoundStatement *body = nullptr;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundBreakStatement : BoundStatement
    {
        // Target loop will be resolved in semantic pass
        BOUND_ACCEPT_VISITOR
    };

    struct BoundContinueStatement : BoundStatement
    {
        // Target loop will be resolved in semantic pass
        BOUND_ACCEPT_VISITOR
    };

    struct BoundReturnStatement : BoundStatement
    {
        BoundExpression *value = nullptr;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundUsingStatement : BoundStatement
    {
        std::vector<std::string> namespaceParts;
        NamespaceSymbol *targetNamespace = nullptr; // Resolved in semantic pass
        BOUND_ACCEPT_VISITOR
    };

    // === Declarations ===

    struct BoundVariableDeclaration : BoundDeclaration
    {
        BoundExpression *typeExpression = nullptr; // Can be null for var
        BoundExpression *initializer = nullptr;
        bool isParameter = false;
        bool isLocal = false;
        bool isField = false;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundFunctionDeclaration : BoundDeclaration
    {
        BoundExpression *returnTypeExpression = nullptr; // Can be null for constructors
        std::vector<BoundVariableDeclaration *> parameters;
        BoundStatement *body = nullptr;
        bool isConstructor = false;
        BOUND_ACCEPT_VISITOR
    };

    // Property accessor (getter or setter)
    // TODO: Maybe we sould actually be creating function symbols out of properties to begin with?
    struct BoundPropertyAccessor
    {
        enum class Kind { Get, Set };
        Kind kind;
        
        // For arrow properties: just an expression
        // For block properties: a statement (block)
        BoundExpression *expression = nullptr;  // For arrow syntax: => expr
        BoundStatement *body = nullptr;         // For block syntax: { ... }
        
        // Function symbol for the generated getter/setter function
        FunctionSymbol *function_symbol = nullptr;
    };

    struct BoundPropertyDeclaration : BoundDeclaration
    {
        BoundExpression *typeExpression = nullptr;
        BoundPropertyAccessor *getter = nullptr;
        BoundPropertyAccessor *setter = nullptr;
        BoundExpression *initializer = nullptr;  // For auto-properties with initial value
        BOUND_ACCEPT_VISITOR
    };

    struct BoundTypeDeclaration : BoundDeclaration
    {
        std::vector<BoundStatement *> members;
        BOUND_ACCEPT_VISITOR
    };

    struct BoundNamespaceDeclaration : BoundDeclaration
    {
        std::vector<BoundStatement *> members;
        BOUND_ACCEPT_VISITOR
    };

    // === Type Expression ===

    struct BoundTypeExpression : BoundExpression
    {
        std::vector<std::string> parts;                   // ["List"], or ["System", "Collections", "Generic", "List"]
        std::vector<BoundTypeExpression *> typeArguments; // For generics (future)
        TypePtr resolvedTypeReference = nullptr;          // Resolved in semantic pass
        BoundExpression* arraySize = nullptr;             // For array types like char[12]
        BOUND_ACCEPT_VISITOR
    };

    // === Compilation Unit ===

    struct BoundCompilationUnit : BoundNode
    {
        std::vector<BoundStatement *> statements; // Top-level statements/declarations
        BOUND_ACCEPT_VISITOR
    };

    class DefaultBoundVisitor : public BoundVisitor
    {
    public:
        // Expressions
        void visit(BoundLiteralExpression *node) override
        {
            // No children to visit
        }

        void visit(BoundNameExpression *node) override
        {
            // No children to visit
        }

        void visit(BoundBinaryExpression *node) override
        {
            if (node->left)
                node->left->accept(this);
            if (node->right)
                node->right->accept(this);
        }

        void visit(BoundUnaryExpression *node) override
        {
            if (node->operand)
                node->operand->accept(this);
        }

        void visit(BoundAssignmentExpression *node) override
        {
            if (node->target)
                node->target->accept(this);
            if (node->value)
                node->value->accept(this);
        }

        void visit(BoundCallExpression *node) override
        {
            if (node->callee)
                node->callee->accept(this);
            for (auto *arg : node->arguments)
            {
                if (arg)
                    arg->accept(this);
            }
        }

        void visit(BoundMemberAccessExpression *node) override
        {
            if (node->object)
                node->object->accept(this);
        }

        void visit(BoundIndexExpression *node) override
        {
            if (node->object)
                node->object->accept(this);
            if (node->index)
                node->index->accept(this);
        }

        void visit(BoundNewExpression *node) override
        {
            if (node->typeExpression)
                node->typeExpression->accept(this);
            for (auto *arg : node->arguments)
            {
                if (arg)
                    arg->accept(this);
            }
        }

        void visit(BoundArrayCreationExpression *node) override
        {
            if (node->elementTypeExpression)
                node->elementTypeExpression->accept(this);
            if (node->size)
                node->size->accept(this);
            for (auto *init : node->initializers)
            {
                if (init)
                    init->accept(this);
            }
        }

        void visit(BoundCastExpression *node) override
        {
            if (node->expression)
                node->expression->accept(this);
            if (node->targetTypeExpression)
                node->targetTypeExpression->accept(this);
        }

        void visit(BoundThisExpression *node) override
        {
            // No children to visit
        }

        void visit(BoundConversionExpression *node) override
        {
            if (node->expression)
                node->expression->accept(this);
        }

        void visit(BoundTypeExpression *node) override
        {
            for (auto *typeArg : node->typeArguments)
            {
                if (typeArg)
                    typeArg->accept(this);
            }
        }

        // Statements
        void visit(BoundBlockStatement *node) override
        {
            for (auto *stmt : node->statements)
            {
                if (stmt)
                    stmt->accept(this);
            }
        }

        void visit(BoundExpressionStatement *node) override
        {
            if (node->expression)
                node->expression->accept(this);
        }

        void visit(BoundIfStatement *node) override
        {
            if (node->condition)
                node->condition->accept(this);
            if (node->thenStatement)
                node->thenStatement->accept(this);
            if (node->elseStatement)
                node->elseStatement->accept(this);
        }

        void visit(BoundWhileStatement *node) override
        {
            if (node->condition)
                node->condition->accept(this);
            if (node->body)
                node->body->accept(this);
        }

        void visit(BoundForStatement *node) override
        {
            if (node->initializer)
                node->initializer->accept(this);
            if (node->condition)
                node->condition->accept(this);
            for (auto *inc : node->incrementors)
            {
                if (inc)
                    inc->accept(this);
            }
            if (node->body)
                node->body->accept(this);
        }

        void visit(BoundBreakStatement *node) override
        {
            // No children to visit
        }

        void visit(BoundContinueStatement *node) override
        {
            // No children to visit
        }

        void visit(BoundReturnStatement *node) override
        {
            if (node->value)
                node->value->accept(this);
        }

        void visit(BoundUsingStatement *node) override
        {
            // No children to visit
        }

        // Declarations
        void visit(BoundVariableDeclaration *node) override
        {
            if (node->typeExpression)
                node->typeExpression->accept(this);
            if (node->initializer)
                node->initializer->accept(this);
        }

        void visit(BoundFunctionDeclaration *node) override
        {
            if (node->returnTypeExpression)
                node->returnTypeExpression->accept(this);
            for (auto *param : node->parameters)
            {
                if (param)
                    param->accept(this);
            }
            if (node->body)
                node->body->accept(this);
        }

        void visit(BoundPropertyDeclaration *node) override
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

        void visit(BoundTypeDeclaration *node) override
        {
            for (auto *member : node->members)
            {
                if (member)
                    member->accept(this);
            }
        }

        void visit(BoundNamespaceDeclaration *node) override
        {
            for (auto *member : node->members)
            {
                if (member)
                    member->accept(this);
            }
        }

        // Top-level
        void visit(BoundCompilationUnit *node) override
        {
            for (auto *stmt : node->statements)
            {
                if (stmt)
                    stmt->accept(this);
            }
        }
    };

} // namespace Fern