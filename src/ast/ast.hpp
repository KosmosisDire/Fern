#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <variant>
#include <span>
#include <optional>
#include <vector>
#include "common/source_location.hpp"
#include "common/token.hpp"
#include "semantic/type.hpp"

namespace Fern
{
#pragma region Forward Declarations
    class Visitor;

    // Root
    struct BaseSyntax;
    // Expressions
    struct BaseExprSyntax;
    struct MissingExprSyntax;

    // Names (used in type/namespace contexts)
    struct BaseNameExprSyntax;
    struct SimpleNameSyntax;
    struct QualifiedNameSyntax;
    struct GenericNameSyntax;

    // Literals
    struct LiteralExprSyntax;
    struct ArrayLiteralExprSyntax;

    // Primary expressions
    struct ThisExprSyntax;
    struct ParenthesizedExprSyntax;

    // Operators
    struct UnaryExprSyntax;
    struct BinaryExprSyntax;
    struct AssignmentExprSyntax;

    // Member/element access
    struct MemberAccessExprSyntax;
    struct IndexerExprSyntax;

    // Invocations
    struct CallExprSyntax;
    struct NewExprSyntax;
    struct CastExprSyntax;

    // Special expressions
    struct LambdaExprSyntax;

    // Type expressions
    struct ArrayTypeSyntax;
    struct PointerTypeSyntax;

    // Statements
    struct BaseStmtSyntax;
    struct MissingStmtSyntax;

    // Control flow
    struct BlockSyntax;
    struct IfStmtSyntax;
    struct WhileStmtSyntax;
    struct ForStmtSyntax;
    struct ReturnStmtSyntax;
    struct BreakStmtSyntax;
    struct ContinueStmtSyntax;

    // Simple statements
    struct ExpressionStmtSyntax;
    struct UsingDirectiveSyntax;

    // Declarations (inherit from statement)
    struct BaseDeclSyntax;
    struct VariableDeclSyntax;
    struct PropertyDeclSyntax;
    struct ParameterDeclSyntax;
    struct FunctionDeclSyntax;
    struct ConstructorDeclSyntax;
    struct EnumCaseDeclSyntax;
    struct TypeDeclSyntax;
    struct TypeParameterDeclSyntax;
    struct NamespaceDeclSyntax;

    // Supporting nodes (not in main hierarchy)
    struct TypedIdentifier;
    struct PropertyAccessorSyntax;
    struct CompilationUnitSyntax;

    // Non-owning collection type (arena allocated)
    template <typename T>
    using List = std::span<T>;
#pragma endregion

// Macro to create an accept function implementation
#define ACCEPT_VISITOR \
    void accept(Visitor *visitor) override { visitor->visit(this); }

#pragma region Visitor Pattern
    class Visitor
    {
    public:
        virtual ~Visitor() = default;

        // Base type visits - can be overridden for uniform handling
        virtual void visit(BaseSyntax *node);
        virtual void visit(BaseExprSyntax *node);
        virtual void visit(BaseStmtSyntax *node);
        virtual void visit(BaseDeclSyntax *node);

        // Names
        virtual void visit(BaseNameExprSyntax *node) = 0;
        virtual void visit(SimpleNameSyntax *node) = 0;
        virtual void visit(QualifiedNameSyntax *node) = 0;
        virtual void visit(GenericNameSyntax *node) = 0;

        // Expressions
        virtual void visit(MissingExprSyntax *node) = 0;
        virtual void visit(LiteralExprSyntax *node) = 0;
        virtual void visit(ArrayLiteralExprSyntax *node) = 0;
        virtual void visit(ThisExprSyntax *node) = 0;
        virtual void visit(ParenthesizedExprSyntax *node) = 0;
        virtual void visit(UnaryExprSyntax *node) = 0;
        virtual void visit(BinaryExprSyntax *node) = 0;
        virtual void visit(AssignmentExprSyntax *node) = 0;
        virtual void visit(MemberAccessExprSyntax *node) = 0;
        virtual void visit(IndexerExprSyntax *node) = 0;
        virtual void visit(CallExprSyntax *node) = 0;
        virtual void visit(NewExprSyntax *node) = 0;
        virtual void visit(CastExprSyntax *node) = 0;
        virtual void visit(LambdaExprSyntax *node) = 0;

        // Type expressions
        virtual void visit(ArrayTypeSyntax *node) = 0;
        virtual void visit(PointerTypeSyntax *node) = 0;

        // Statements
        virtual void visit(MissingStmtSyntax *node) = 0;
        virtual void visit(BlockSyntax *node) = 0;
        virtual void visit(IfStmtSyntax *node) = 0;
        virtual void visit(WhileStmtSyntax *node) = 0;
        virtual void visit(ForStmtSyntax *node) = 0;
        virtual void visit(ReturnStmtSyntax *node) = 0;
        virtual void visit(BreakStmtSyntax *node) = 0;
        virtual void visit(ContinueStmtSyntax *node) = 0;
        virtual void visit(ExpressionStmtSyntax *node) = 0;
        virtual void visit(UsingDirectiveSyntax *node) = 0;

        // Declarations
        virtual void visit(VariableDeclSyntax *node) = 0;
        virtual void visit(PropertyDeclSyntax *node) = 0;
        virtual void visit(ParameterDeclSyntax *node) = 0;
        virtual void visit(FunctionDeclSyntax *node) = 0;
        virtual void visit(ConstructorDeclSyntax *node) = 0;
        virtual void visit(EnumCaseDeclSyntax *node) = 0;
        virtual void visit(TypeDeclSyntax *node) = 0;
        virtual void visit(TypeParameterDeclSyntax *node) = 0;
        virtual void visit(NamespaceDeclSyntax *node) = 0;

        // Supporting nodes
        virtual void visit(TypedIdentifier *node) = 0;
        virtual void visit(PropertyAccessorSyntax *node) = 0;
        virtual void visit(CompilationUnitSyntax *node) = 0;
    };
#pragma endregion

#pragma region Core Node Hierarchy
    struct BaseSyntax
    {
        SourceRange location;

        virtual ~BaseSyntax() = default;
        virtual void accept(Visitor *visitor);

        // Clean API using dynamic_cast
        template <typename T>
        bool is() const
        {
            return dynamic_cast<const T *>(this) != nullptr;
        }

        template <typename T>
        T *as()
        {
            return dynamic_cast<T *>(this);
        }

        template <typename T>
        const T *as() const
        {
            return dynamic_cast<const T *>(this);
        }
    };

    struct BaseExprSyntax : BaseSyntax
    {
        ACCEPT_VISITOR
    };

    struct BaseStmtSyntax : BaseSyntax
    {
        ACCEPT_VISITOR
    };

    struct BaseDeclSyntax : BaseStmtSyntax
    {
        ModifierKindFlags modifiers;
        ACCEPT_VISITOR
    };
#pragma endregion

#pragma region Names
    
    struct BaseNameExprSyntax : BaseExprSyntax
    {
        ACCEPT_VISITOR

        std::string get_name() const;
        std::vector<std::string> get_parts() const;
    };

    struct SimpleNameSyntax : BaseNameExprSyntax
    {
        Token identifier;
        ACCEPT_VISITOR
    };

    struct QualifiedNameSyntax : BaseNameExprSyntax
    {
        BaseExprSyntax *left;      // The qualifier (can be any expression to allow complex types)
        BaseNameExprSyntax *right; // The member name
        ACCEPT_VISITOR
    };

    struct GenericNameSyntax : BaseNameExprSyntax
    {
        BaseNameExprSyntax *identifier; // Just the name token
        List<BaseExprSyntax *> typeArguments;
        ACCEPT_VISITOR
    };

    inline std::vector<std::string> BaseNameExprSyntax::get_parts() const
    {
        std::vector<std::string> parts;

        if (auto qualified = this->as<QualifiedNameSyntax>())
        {
            // For qualified names, get parts from left side (if it's a name)
            if (auto leftName = qualified->left->as<BaseNameExprSyntax>())
            {
                auto leftParts = leftName->get_parts();
                parts.insert(parts.end(), leftParts.begin(), leftParts.end());
            }
            
            // Add parts from right side
            auto rightParts = qualified->right->get_parts();
            parts.insert(parts.end(), rightParts.begin(), rightParts.end());
        }
        else if (auto simple = this->as<SimpleNameSyntax>())
        {
            // Simple name - single part
            parts.push_back(simple->identifier.text);
        }
        else if (auto generic = this->as<GenericNameSyntax>())
        {
            // Generic name - get parts from the identifier (no type arguments)
            auto identifierParts = generic->identifier->get_parts();
            parts.insert(parts.end(), identifierParts.begin(), identifierParts.end());
        }

        return parts;
    }

    inline std::string BaseNameExprSyntax::get_name() const
    {
        auto parts = get_parts();
        if (parts.empty())
        {
            return "";
        }

        std::string result = parts[0];
        for (size_t i = 1; i < parts.size(); ++i)
        {
            result += "." + parts[i];
        }
        return result;
    }



#pragma endregion

#pragma region Supporting Nodes

    struct TypedIdentifier : BaseSyntax
    {
        BaseNameExprSyntax *name;
        BaseExprSyntax *type; // null = inferred (var)
        ACCEPT_VISITOR
    };
    
#pragma endregion

#pragma region Expressions

    struct MissingExprSyntax : BaseExprSyntax
    {
        std::string message;
        List<BaseSyntax *> partialNodes;
        ACCEPT_VISITOR
    };

    struct LiteralExprSyntax : BaseExprSyntax
    {
        LiteralKind kind;
        std::string value; // Raw text from source
        ACCEPT_VISITOR
    };

    struct ArrayLiteralExprSyntax : BaseExprSyntax
    {
        List<BaseExprSyntax *> elements;
        ACCEPT_VISITOR
    };

    struct ThisExprSyntax : BaseExprSyntax
    {
        ACCEPT_VISITOR
    };

    struct ParenthesizedExprSyntax : BaseExprSyntax
    {
        BaseExprSyntax *expression; // The expression inside parentheses
        ACCEPT_VISITOR
    };

    struct UnaryExprSyntax : BaseExprSyntax
    {
        UnaryOperatorKind op;
        BaseExprSyntax *operand;
        bool isPostfix;
        ACCEPT_VISITOR
    };

    struct BinaryExprSyntax : BaseExprSyntax
    {
        BaseExprSyntax *left;
        BinaryOperatorKind op;
        BaseExprSyntax *right;
        ACCEPT_VISITOR
    };

    struct AssignmentExprSyntax : BaseExprSyntax
    {
        BaseExprSyntax *target;
        AssignmentOperatorKind op;
        BaseExprSyntax *value;
        ACCEPT_VISITOR
    };

    struct MemberAccessExprSyntax : BaseExprSyntax
    {
        BaseExprSyntax *object;       // The object/expression being accessed
        BaseNameExprSyntax *member; // The member name
        ACCEPT_VISITOR
    };

    struct IndexerExprSyntax : BaseExprSyntax
    {
        BaseExprSyntax *object;
        BaseExprSyntax *index;
        ACCEPT_VISITOR
    };

    struct CallExprSyntax : BaseExprSyntax
    {
        BaseExprSyntax *callee;
        List<BaseExprSyntax *> arguments;
        ACCEPT_VISITOR
    };

    struct NewExprSyntax : BaseExprSyntax
    {
        BaseExprSyntax *type;
        List<BaseExprSyntax *> arguments;
        ACCEPT_VISITOR
    };

    struct CastExprSyntax : BaseExprSyntax
    {
        BaseExprSyntax *targetType;
        BaseExprSyntax *expression;
        ACCEPT_VISITOR
    };

    struct LambdaExprSyntax : BaseExprSyntax
    {
        List<ParameterDeclSyntax *> parameters;
        BaseStmtSyntax *body; // BlockSyntax or ExpressionStmtSyntax
        ACCEPT_VISITOR
    };

#pragma endregion

#pragma region Type Expressions
    struct ArrayTypeSyntax : BaseExprSyntax
    {
        BaseExprSyntax *baseType;
        LiteralExprSyntax *size; // Can be null for unsized arrays
        ACCEPT_VISITOR
    };

    struct PointerTypeSyntax : BaseExprSyntax
    {
        BaseExprSyntax *baseType;
        ACCEPT_VISITOR
    };
#pragma endregion

#pragma region Statements

    struct MissingStmtSyntax : BaseStmtSyntax
    {
        std::string message;
        List<BaseSyntax *> partialNodes;
        ACCEPT_VISITOR
    };

    struct BlockSyntax : BaseStmtSyntax
    {
        List<BaseStmtSyntax *> statements;
        ACCEPT_VISITOR
    };

    struct IfStmtSyntax : BaseStmtSyntax
    {
        BaseExprSyntax *condition;
        BaseStmtSyntax *thenBranch;
        BaseStmtSyntax *elseBranch; // Can be null
        ACCEPT_VISITOR
    };

    struct WhileStmtSyntax : BaseStmtSyntax
    {
        BaseExprSyntax *condition;
        BaseStmtSyntax *body;
        ACCEPT_VISITOR
    };

    struct ForStmtSyntax : BaseStmtSyntax
    {
        BaseStmtSyntax *initializer; // Can be null
        BaseExprSyntax *condition;   // Can be null (infinite loop)
        List<BaseExprSyntax *> updates;
        BaseStmtSyntax *body;
        ACCEPT_VISITOR
    };

    struct ReturnStmtSyntax : BaseStmtSyntax
    {
        BaseExprSyntax *value; // Can be null (void return)
        ACCEPT_VISITOR
    };

    struct BreakStmtSyntax : BaseStmtSyntax
    {
        ACCEPT_VISITOR
    };

    struct ContinueStmtSyntax : BaseStmtSyntax
    {
        ACCEPT_VISITOR
    };

    struct ExpressionStmtSyntax : BaseStmtSyntax
    {
        BaseExprSyntax *expression;
        ACCEPT_VISITOR
    };

    struct UsingDirectiveSyntax : BaseStmtSyntax
    {
        BaseNameExprSyntax *target; // The imported namespace/type
        ACCEPT_VISITOR
    };

#pragma endregion

#pragma region Declarations
    struct VariableDeclSyntax : BaseDeclSyntax
    {
        TypedIdentifier *variable;
        BaseExprSyntax *initializer; // Can be null
        ACCEPT_VISITOR
    };

    struct PropertyDeclSyntax : BaseDeclSyntax
    {
        VariableDeclSyntax *variable;   // The underlying variable
        PropertyAccessorSyntax *getter; // null = auto-generated
        PropertyAccessorSyntax *setter; // null = no setter (read-only)
        ACCEPT_VISITOR
    };

    struct PropertyAccessorSyntax : BaseSyntax
    {
        PropertyKind kind;
        ModifierKindFlags modifiers;

        // Body representation
        std::variant<
            std::monostate,   // Default/auto-implemented
            BaseExprSyntax *, // Expression-bodied: => expr
            BlockSyntax *     // BlockSyntax-bodied: { ... }
            >
            body;

        ACCEPT_VISITOR
    };

    struct ParameterDeclSyntax : BaseDeclSyntax
    {
        TypedIdentifier *param;
        BaseExprSyntax *defaultValue; // Can be null
        ACCEPT_VISITOR
    };

    struct FunctionDeclSyntax : BaseDeclSyntax
    {
        BaseNameExprSyntax *name;
        List<TypeParameterDeclSyntax *> typeParameters;
        List<ParameterDeclSyntax *> parameters;
        BaseExprSyntax *returnType; // null = void
        BlockSyntax *body;          // Can be null (abstract)
        ACCEPT_VISITOR
    };

    struct ConstructorDeclSyntax : BaseDeclSyntax
    {
        // No name field - constructors are always "new"
        List<ParameterDeclSyntax *> parameters;
        BlockSyntax *body;
        ACCEPT_VISITOR
    };

    struct EnumCaseDeclSyntax : BaseDeclSyntax
    {
        BaseNameExprSyntax *name;
        List<ParameterDeclSyntax *> associatedData; // Can be empty
        ACCEPT_VISITOR
    };

    struct TypeDeclSyntax : BaseDeclSyntax
    {
        BaseNameExprSyntax *name;
        List<TypeParameterDeclSyntax *> typeParameters;
        List<BaseExprSyntax *> baseTypes;
        List<BaseDeclSyntax *> members;
        ACCEPT_VISITOR
    };

    struct TypeParameterDeclSyntax : BaseDeclSyntax
    {
        BaseNameExprSyntax *name; // The type parameter name (T, U, etc.)
        // Future: constraints can be added here
        ACCEPT_VISITOR
    };

    struct NamespaceDeclSyntax : BaseDeclSyntax
    {
        BaseNameExprSyntax *name; // The namespace name
        bool isFileScoped;
        std::optional<List<BaseStmtSyntax *>> body; // nullopt for file-scoped
        ACCEPT_VISITOR
    };
#pragma endregion

#pragma region Root Node
    struct CompilationUnitSyntax : BaseSyntax
    {
        List<BaseStmtSyntax *> topLevelStatements;
        ACCEPT_VISITOR
    };
#pragma endregion

#pragma region Default Visitor
    class DefaultVisitor : public Visitor
    {
    public:
        // Base type visit implementations
        void visit(BaseSyntax *node) override { /* default: do nothing */ }
        void visit(BaseExprSyntax *node) override { visit(static_cast<BaseSyntax *>(node)); }
        void visit(BaseStmtSyntax *node) override { visit(static_cast<BaseSyntax *>(node)); }
        void visit(BaseDeclSyntax *node) override { visit(static_cast<BaseStmtSyntax *>(node)); }

        // Names
        void visit(BaseNameExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
        }

        void visit(SimpleNameSyntax *node) override
        {
            visit(static_cast<BaseNameExprSyntax *>(node));
        }

        void visit(QualifiedNameSyntax *node) override
        {
            visit(static_cast<BaseNameExprSyntax *>(node));
            if (node->left)
                node->left->accept(this);
            if (node->right)
                node->right->accept(this);
        }

        void visit(GenericNameSyntax *node) override
        {
            visit(static_cast<BaseNameExprSyntax *>(node));
            if (node->identifier)
                node->identifier->accept(this);
            for (auto arg : node->typeArguments)
            {
                if (arg)
                    arg->accept(this);
            }
        }

        // Supporting nodes
        void visit(TypedIdentifier *node) override
        {
            visit(static_cast<BaseSyntax *>(node));
            if (node->name)
                node->name->accept(this);
            if (node->type)
                node->type->accept(this);
        }

        void visit(PropertyAccessorSyntax *node) override
        {
            visit(static_cast<BaseSyntax *>(node));
            if (auto expr = std::get_if<BaseExprSyntax *>(&node->body))
            {
                if (*expr)
                    (*expr)->accept(this);
            }
            else if (auto block = std::get_if<BlockSyntax *>(&node->body))
            {
                if (*block)
                    (*block)->accept(this);
            }
        }

        // Expressions
        void visit(MissingExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            for (auto partial : node->partialNodes)
            {
                if (partial)
                    partial->accept(this);
            }
        }

        void visit(LiteralExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
        }

        void visit(ArrayLiteralExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            for (auto elem : node->elements)
            {
                if (elem)
                    elem->accept(this);
            }
        }
        
        void visit(ThisExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
        }

        void visit(ParenthesizedExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            if (node->expression)
                node->expression->accept(this);
        }

        void visit(UnaryExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            if (node->operand)
                node->operand->accept(this);
        }

        void visit(BinaryExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            if (node->left)
                node->left->accept(this);
            if (node->right)
                node->right->accept(this);
        }

        void visit(AssignmentExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            if (node->target)
                node->target->accept(this);
            if (node->value)
                node->value->accept(this);
        }

        void visit(MemberAccessExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            if (node->object)
                node->object->accept(this);
            if (node->member)
                node->member->accept(this);
        }

        void visit(IndexerExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            if (node->object)
                node->object->accept(this);
            if (node->index)
                node->index->accept(this);
        }

        void visit(CallExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            if (node->callee)
                node->callee->accept(this);
            for (auto arg : node->arguments)
            {
                if (arg)
                    arg->accept(this);
            }
        }

        void visit(NewExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            if (node->type)
                node->type->accept(this);
            for (auto arg : node->arguments)
            {
                if (arg)
                    arg->accept(this);
            }
        }

        void visit(CastExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            if (node->targetType)
                node->targetType->accept(this);
            if (node->expression)
                node->expression->accept(this);
        }

        void visit(LambdaExprSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            for (auto param : node->parameters)
            {
                if (param)
                    param->accept(this);
            }
            if (node->body)
                node->body->accept(this);
        }

        // Type expressions
        void visit(ArrayTypeSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            if (node->baseType)
                node->baseType->accept(this);
            if (node->size)
                node->size->accept(this);
        }

        void visit(PointerTypeSyntax *node) override
        {
            visit(static_cast<BaseExprSyntax *>(node));
            if (node->baseType)
                node->baseType->accept(this);
        }

        // Statements
        void visit(MissingStmtSyntax *node) override
        {
            visit(static_cast<BaseStmtSyntax *>(node));
            for (auto partial : node->partialNodes)
            {
                if (partial)
                    partial->accept(this);
            }
        }

        void visit(BlockSyntax *node) override
        {
            visit(static_cast<BaseStmtSyntax *>(node));
            for (auto stmt : node->statements)
            {
                if (stmt)
                    stmt->accept(this);
            }
        }

        void visit(IfStmtSyntax *node) override
        {
            visit(static_cast<BaseStmtSyntax *>(node));
            if (node->condition)
                node->condition->accept(this);
            if (node->thenBranch)
                node->thenBranch->accept(this);
            if (node->elseBranch)
                node->elseBranch->accept(this);
        }

        void visit(WhileStmtSyntax *node) override
        {
            visit(static_cast<BaseStmtSyntax *>(node));
            if (node->condition)
                node->condition->accept(this);
            if (node->body)
                node->body->accept(this);
        }

        void visit(ForStmtSyntax *node) override
        {
            visit(static_cast<BaseStmtSyntax *>(node));
            if (node->initializer)
                node->initializer->accept(this);
            if (node->condition)
                node->condition->accept(this);
            for (auto update : node->updates)
            {
                if (update)
                    update->accept(this);
            }
            if (node->body)
                node->body->accept(this);
        }

        void visit(ReturnStmtSyntax *node) override
        {
            visit(static_cast<BaseStmtSyntax *>(node));
            if (node->value)
                node->value->accept(this);
        }

        void visit(BreakStmtSyntax *node) override
        {
            visit(static_cast<BaseStmtSyntax *>(node));
        }

        void visit(ContinueStmtSyntax *node) override
        {
            visit(static_cast<BaseStmtSyntax *>(node));
        }

        void visit(ExpressionStmtSyntax *node) override
        {
            visit(static_cast<BaseStmtSyntax *>(node));
            if (node->expression)
                node->expression->accept(this);
        }

        void visit(UsingDirectiveSyntax *node) override
        {
            visit(static_cast<BaseStmtSyntax *>(node));
            if (node->target)
                node->target->accept(this);
        }

        // Declarations
        void visit(VariableDeclSyntax *node) override
        {
            visit(static_cast<BaseDeclSyntax *>(node));
            if (node->variable)
                node->variable->accept(this);
            if (node->initializer)
                node->initializer->accept(this);
        }

        void visit(PropertyDeclSyntax *node) override
        {
            visit(static_cast<BaseDeclSyntax *>(node));
            if (node->variable)
                node->variable->accept(this);
            if (node->getter)
                node->getter->accept(this);
            if (node->setter)
                node->setter->accept(this);
        }

        void visit(ParameterDeclSyntax *node) override
        {
            visit(static_cast<BaseDeclSyntax *>(node));
            if (node->param)
                node->param->accept(this);
            if (node->defaultValue)
                node->defaultValue->accept(this);
        }

        void visit(FunctionDeclSyntax *node) override
        {
            visit(static_cast<BaseDeclSyntax *>(node));
            if (node->name)
                node->name->accept(this);
            for (auto typeParam : node->typeParameters)
            {
                if (typeParam)
                    typeParam->accept(this);
            }
            for (auto param : node->parameters)
            {
                if (param)
                    param->accept(this);
            }
            if (node->returnType)
                node->returnType->accept(this);
            if (node->body)
                node->body->accept(this);
        }

        void visit(ConstructorDeclSyntax *node) override
        {
            visit(static_cast<BaseDeclSyntax *>(node));
            for (auto param : node->parameters)
            {
                if (param)
                    param->accept(this);
            }
            if (node->body)
                node->body->accept(this);
        }

        void visit(EnumCaseDeclSyntax *node) override
        {
            visit(static_cast<BaseDeclSyntax *>(node));
            if (node->name)
                node->name->accept(this);
            for (auto data : node->associatedData)
            {
                if (data)
                    data->accept(this);
            }
        }

        void visit(TypeDeclSyntax *node) override
        {
            visit(static_cast<BaseDeclSyntax *>(node));
            if (node->name)
                node->name->accept(this);
            for (auto typeParam : node->typeParameters)
            {
                if (typeParam)
                    typeParam->accept(this);
            }
            for (auto base : node->baseTypes)
            {
                if (base)
                    base->accept(this);
            }
            for (auto member : node->members)
            {
                if (member)
                    member->accept(this);
            }
        }

        void visit(TypeParameterDeclSyntax *node) override
        {
            visit(static_cast<BaseDeclSyntax *>(node));
            if (node->name)
                node->name->accept(this);
        }

        void visit(NamespaceDeclSyntax *node) override
        {
            visit(static_cast<BaseDeclSyntax *>(node));
            if (node->name)
                node->name->accept(this);
            if (node->body)
            {
                for (auto stmt : *node->body)
                {
                    if (stmt)
                        stmt->accept(this);
                }
            }
        }

        void visit(CompilationUnitSyntax *node) override
        {
            visit(static_cast<BaseSyntax *>(node));
            for (auto stmt : node->topLevelStatements)
            {
                if (stmt)
                    stmt->accept(this);
            }
        }
    };
#pragma endregion

#pragma region Implementation Details
    // Implementation of BaseSyntax::accept (needed for base case)
    inline void BaseSyntax::accept(Visitor *visitor)
    {
        visitor->visit(this);
    }

    // Default implementations for base type visits
    inline void Visitor::visit(BaseSyntax *node)
    {
        // Default: do nothing - override in derived visitors for uniform handling
    }

    inline void Visitor::visit(BaseExprSyntax *node)
    {
        visit(static_cast<BaseSyntax *>(node));
    }

    inline void Visitor::visit(BaseStmtSyntax *node)
    {
        visit(static_cast<BaseSyntax *>(node));
    }

    inline void Visitor::visit(BaseDeclSyntax *node)
    {
        visit(static_cast<BaseStmtSyntax *>(node));
    }
#pragma endregion

} // namespace Fern