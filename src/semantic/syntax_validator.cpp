#include "syntax_validator.hpp"
#include <unordered_set>

namespace Fern
{

    void SyntaxValidator::validate(CompilationUnitSyntax* unit)
    {
        if (unit)
        {
            unit->accept(this);
        }
    }

    void SyntaxValidator::visit(FunctionDeclSyntax* node)
    {
        // Check for duplicate parameter names
        std::unordered_set<std::string> paramNames;
        for (auto param : node->parameters)
        {
            if (param && param->param && param->param->name)
            {
                std::string name = param->param->name->get_name();
                if (paramNames.count(name))
                {
                    error("Duplicate parameter name '" + name + "'", param->location);
                }
                else
                {
                    paramNames.insert(name);
                }
            }
        }

        // Check: extern functions cannot have a body
        if (has_flag(node->modifiers, ModifierKindFlags::Extern) && node->body != nullptr)
        {
            error("External function cannot have a body", node->location);
        }

        // Visit parameters (will check for 'var' usage)
        for (auto param : node->parameters)
        {
            if (param)
                param->accept(this);
        }

        // Visit body with function context
        bool wasInFunction = inFunction;
        inFunction = true;

        if (node->body)
        {
            node->body->accept(this);
        }

        inFunction = wasInFunction;
    }

    void SyntaxValidator::visit(ParameterDeclSyntax* node)
    {
        if (!node->param)
            return;

        // Check for 'var' type - not allowed for parameters
        if (node->param->type == nullptr)
        {
            std::string name = node->param->name ? node->param->name->get_name() : "<unknown>";
            error("Parameter '" + name + "' must have an explicit type ('var' is not allowed for function parameters)", node->location);
        }

        // Let DefaultVisitor handle children
        DefaultVisitor::visit(node);
    }

    void SyntaxValidator::visit(VariableDeclSyntax* node)
    {
        // 'var' is allowed for variables if they have an initializer
        if (node->variable && node->variable->type == nullptr && node->initializer == nullptr)
        {
            std::string name = node->variable->name ? node->variable->name->get_name() : "<unknown>";
            error("Variable '" + name + "' declared with 'var' must have an initializer for type inference", node->location);
        }

        // Let DefaultVisitor handle children
        DefaultVisitor::visit(node);
    }

    void SyntaxValidator::visit(PropertyDeclSyntax* node)
    {
        // Properties are not yet supported
        std::string name = (node->variable && node->variable->variable && node->variable->variable->name)
            ? node->variable->variable->name->get_name() : "<unknown>";
        error("Properties are not yet supported: '" + name + "'", node->location);

        // Still visit accessors to catch other errors
        if (node->getter)
            node->getter->accept(this);
        if (node->setter)
            node->setter->accept(this);
    }

    void SyntaxValidator::visit(ConstructorDeclSyntax* node)
    {
        // Check for duplicate parameter names
        std::unordered_set<std::string> paramNames;
        for (auto param : node->parameters)
        {
            if (param && param->param && param->param->name)
            {
                std::string name = param->param->name->get_name();
                if (paramNames.count(name))
                {
                    error("Duplicate parameter name '" + name + "'", param->location);
                }
                else
                {
                    paramNames.insert(name);
                }
            }
        }

        // Visit parameters
        for (auto param : node->parameters)
        {
            if (param)
                param->accept(this);
        }

        // Visit body with function context
        bool wasInFunction = inFunction;
        inFunction = true;

        if (node->body)
        {
            node->body->accept(this);
        }

        inFunction = wasInFunction;
    }

    void SyntaxValidator::visit(WhileStmtSyntax* node)
    {
        if (node->condition)
            node->condition->accept(this);

        loopDepth++;
        if (node->body)
            node->body->accept(this);
        loopDepth--;
    }

    void SyntaxValidator::visit(ForStmtSyntax* node)
    {
        if (node->initializer)
            node->initializer->accept(this);
        if (node->condition)
            node->condition->accept(this);
        for (auto update : node->updates)
        {
            if (update)
                update->accept(this);
        }

        loopDepth++;
        if (node->body)
            node->body->accept(this);
        loopDepth--;
    }

    void SyntaxValidator::visit(ReturnStmtSyntax* node)
    {
        if (!inFunction)
        {
            error("'return' statement outside of function", node->location);
        }

        // Let DefaultVisitor handle children
        DefaultVisitor::visit(node);
    }

    void SyntaxValidator::visit(BreakStmtSyntax* node)
    {
        if (loopDepth == 0)
        {
            error("'break' statement outside of loop", node->location);
        }
    }

    void SyntaxValidator::visit(ContinueStmtSyntax* node)
    {
        if (loopDepth == 0)
        {
            error("'continue' statement outside of loop", node->location);
        }
    }

    void SyntaxValidator::visit(LambdaExprSyntax* node)
    {
        // Lambdas are not yet supported
        error("Lambda expressions are not yet supported", node->location);

        // Check for duplicate parameter names
        std::unordered_set<std::string> paramNames;
        for (auto param : node->parameters)
        {
            if (param && param->param && param->param->name)
            {
                std::string name = param->param->name->get_name();
                if (paramNames.count(name))
                {
                    error("Duplicate parameter name '" + name + "'", param->location);
                }
                else
                {
                    paramNames.insert(name);
                }
            }
        }

        // Visit parameters
        for (auto param : node->parameters)
        {
            if (param)
                param->accept(this);
        }

        // Visit body with function context
        bool wasInFunction = inFunction;
        inFunction = true;

        if (node->body)
            node->body->accept(this);

        inFunction = wasInFunction;
    }

    void SyntaxValidator::visit(PropertyAccessorSyntax* node)
    {
        bool wasInFunction = inFunction;
        inFunction = true;

        // Let DefaultVisitor handle the body
        DefaultVisitor::visit(node);

        inFunction = wasInFunction;
    }

    void SyntaxValidator::visit(TypeDeclSyntax* node)
    {
        std::string name = node->name ? node->name->get_name() : "<unknown>";

        // Check for enums (not yet supported)
        if (has_flag(node->modifiers, ModifierKindFlags::Enum))
        {
            error("Enums are not yet supported: '" + name + "'", node->location);
        }

        // Check for generics (not yet supported)
        if (!node->typeParameters.empty())
        {
            error("Generic types are not yet supported: '" + name + "'", node->location);
        }

        // Check for inheritance (not yet supported)
        if (!node->baseTypes.empty())
        {
            error("Type inheritance is not yet supported: '" + name + "'", node->location);
        }

        // Visit members
        for (auto member : node->members)
        {
            if (member)
                member->accept(this);
        }
    }

    void SyntaxValidator::visit(GenericNameSyntax* node)
    {
        // Generic type usage is not yet supported (e.g., List<int>)
        std::string name = node->identifier ? node->identifier->get_name() : "<unknown>";
        error("Generic type arguments are not yet supported: '" + name + "<...>'", node->location);

        // Still visit children to catch other errors
        DefaultVisitor::visit(node);
    }

    void SyntaxValidator::visit(NamespaceDeclSyntax* node)
    {
        // Namespaces are not yet supported
        std::string name = node->name ? node->name->get_name() : "<unknown>";
        error("Namespaces are not yet supported: '" + name + "'", node->location);

        // Still visit body to catch other errors
        if (node->body.has_value())
        {
            for (auto stmt : node->body.value())
            {
                if (stmt)
                    stmt->accept(this);
            }
        }
    }

} // namespace Fern
