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
        // For properties, we skip the VariableDeclSyntax validation because:
        // - Arrow properties (var X => expr) have no initializer on the variable but have a getter
        // - Properties with get/set blocks also don't need initializers
        // We still need to validate the accessors themselves

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

} // namespace Fern
