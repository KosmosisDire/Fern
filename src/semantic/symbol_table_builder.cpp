#include "symbol_table_builder.hpp"
#include <iostream>

namespace Fern
{

    #pragma region Core Helper Methods

    TypePtr SymbolTableBuilder::get_type_from_expr(BaseExprSyntax *typeExpr)
    {
        // For now, return unresolved type
        // This will be resolved in a later semantic pass
        return typeSystem.get_unresolved();
    }

    void SymbolTableBuilder::build(CompilationUnitSyntax *unit)
    {
        if (unit)
        {
            unit->accept(this);
        }
    }

    #pragma region Visitor Implementations

    void SymbolTableBuilder::visit(BaseSyntax *node)
    {
    }

    void SymbolTableBuilder::visit(CompilationUnitSyntax *node)
    {
        // Start at global namespace
        symbolTable.push_scope(symbolTable.get_global_namespace());

        for (auto stmt : node->topLevelStatements)
        {
            if (stmt)
                stmt->accept(this);
        }

        symbolTable.pop_scope();
    }

    void SymbolTableBuilder::visit(NamespaceDeclSyntax *node)
    {

        if (!node->name)
            return;

        std::string name = node->name->get_name();
        auto ns_symbol = symbolTable.define_namespace(name);

        if (!ns_symbol)
        {
            error("Failed to define namespace '" + name + "'", node->location);
            return;
        }

        // Enter namespace scope
        symbolTable.push_scope(ns_symbol);

        // Process body
        if (node->body)
        {
            for (auto stmt : *node->body)
            {
                if (stmt)
                    stmt->accept(this);
            }
        }

        // Exit scope unless file-scoped
        if (!node->isFileScoped)
        {
            symbolTable.pop_scope();
        }
    }

    void SymbolTableBuilder::visit(TypeDeclSyntax *node)
    {

        if (!node->name)
            return;

        std::string name = node->name->get_name();

        // Create the type (initially unresolved)
        auto type = typeSystem.get_unresolved();
        auto type_symbol = symbolTable.define_type(name, type);

        if (!type_symbol)
        {
            error("Failed to define type '" + name + "'", node->location);
            return;
        }

        type_symbol->modifiers = node->modifiers;

        // Enter type scope
        symbolTable.push_scope(type_symbol);

        // Process members
        for (auto member : node->members)
        {
            if (member)
                member->accept(this);
        }

        symbolTable.pop_scope();
    }

    void SymbolTableBuilder::visit(FunctionDeclSyntax *node)
    {

        if (!node->name)
            return;

        std::string name = node->name->get_name();
        TypePtr return_type = get_type_from_expr(node->returnType);

        auto func_symbol = symbolTable.define_function(name, return_type);
        if (!func_symbol)
        {
            error("Failed to define function '" + name + "'", node->location);
            return;
        }

        // Apply modifiers
        func_symbol->modifiers = node->modifiers;
        if (func_symbol->is_extern())
        {
            // Validate: extern functions must not have a body
            if (node->body != nullptr)
            {
                error("External function '" + name + "' cannot have a body", node->location);
            }
        }

        // Enter function scope
        symbolTable.push_scope(func_symbol);
        currentParameterIndex = 0;

        // Process parameters
        std::vector<ParameterSymbol *> params;
        for (auto param_decl : node->parameters)
        {
            if (param_decl)
            {
                param_decl->accept(this);

                // Extract the created parameter symbol
                if (param_decl->param && param_decl->param->name)
                {
                    std::string param_name = param_decl->param->name->get_name();
                    if (auto param_sym = symbolTable.resolve(param_name))
                    {
                        if (auto p = param_sym->as<ParameterSymbol>())
                        {
                            params.push_back(p);
                        }
                    }
                }
            }
        }
        func_symbol->parameters = std::move(params);

        // Process body
        if (node->body)
        {
            // Don't create a new scope - BlockSyntax will handle that
            for (auto stmt : node->body->statements)
            {
                if (stmt)
                    stmt->accept(this);
            }
        }

        symbolTable.pop_scope();
    }

    void SymbolTableBuilder::visit(ConstructorDeclSyntax *node)
    {
        // Constructors use a special "new" name to avoid collision with the type name
        auto containing_type = symbolTable.get_current_scope()->as<TypeSymbol>();
        if (!containing_type)
        {
            error("Constructor must be defined within a type", node->location);
            return;
        }

        std::string name = "New";  // Use "New" instead of type name to avoid collision
        TypePtr return_type = typeSystem.get_primitive("void");

        auto func_symbol = symbolTable.define_function(name, return_type);
        if (!func_symbol)
        {
            error("Failed to define constructor for type '" + containing_type->name + "'", node->location);
            return;
        }

        // Mark as constructor
        func_symbol->is_constructor = true;
        func_symbol->modifiers = node->modifiers;
        func_symbol->location = node->location;

        // Map AST node to symbol for binding phase
        symbolTable.map_ast_to_symbol(node, func_symbol);


        // Enter function scope
        symbolTable.push_scope(func_symbol);
        currentParameterIndex = 0;


        // Process parameters
        std::vector<ParameterSymbol *> params;
        for (auto param_decl : node->parameters)
        {
            if (param_decl)
            {
                param_decl->accept(this);

                // Extract the created parameter symbol
                if (param_decl->param && param_decl->param->name)
                {
                    std::string param_name = param_decl->param->name->get_name();
                    if (auto param_sym = symbolTable.resolve(param_name))
                    {
                        if (auto p = param_sym->as<ParameterSymbol>())
                        {
                            params.push_back(p);
                        }
                        else
                        {
                        }
                    }
                    else
                    {
                    }
                }
            }
        }
        func_symbol->parameters = std::move(params);

        // Process body
        if (node->body)
        {
            // Don't create a new scope - BlockSyntax will handle that
            for (auto stmt : node->body->statements)
            {
                if (stmt)
                    stmt->accept(this);
            }
        }

        symbolTable.pop_scope();
    }

    void SymbolTableBuilder::visit(ParameterDeclSyntax *node)
    {

        if (!node->param || !node->param->name)
            return;

        std::string name = node->param->name->get_name();
        TypePtr type = get_type_from_expr(node->param->type);

        auto param_symbol = symbolTable.define_parameter(name, type, currentParameterIndex++);
        if (!param_symbol)
        {
            error("Failed to define parameter '" + name + "'", node->location);
        }
        else
        {
        }

        // Visit children for annotation
        if (node->param)
            node->param->accept(this);
        if (node->defaultValue)
            node->defaultValue->accept(this);
    }

    void SymbolTableBuilder::visit(VariableDeclSyntax *node)
    {
        if (!node->variable || !node->variable->name)
            return;

        std::string name = node->variable->name->get_name();
        TypePtr type = get_type_from_expr(node->variable->type);

        auto symbol = symbolTable.define_variable(name, type);
        if (!symbol)
        {
            error("Failed to define variable '" + name + "'", node->location);
        }

        // Visit children
        if (node->variable)
            node->variable->accept(this);
        if (node->initializer)
            node->initializer->accept(this);
    }

    void SymbolTableBuilder::visit(PropertyDeclSyntax *node)
    {

        if (!node->variable || !node->variable->variable ||
            !node->variable->variable->name)
            return;

        std::string name = node->variable->variable->name->get_name();
        TypePtr type = get_type_from_expr(node->variable->variable->type);

        // Create a PropertySymbol
        auto prop_symbol = symbolTable.define_property(name, type);
        if (!prop_symbol)
        {
            error("Failed to define property '" + name + "'", node->location);
            return;
        }

        // Store whether this property has getter/setter and create function symbols
        prop_symbol->has_getter = (node->getter != nullptr);
        prop_symbol->has_setter = (node->setter != nullptr);
        
        // Enter property scope to create getter/setter as children
        symbolTable.push_scope(prop_symbol);
        
        // Create getter function symbol if present
        if (node->getter) {
            auto getter_symbol = symbolTable.define_function("get", type);
            if (!getter_symbol) {
                error("Failed to define getter function for property '" + name + "'", node->location);
            }
        }

        // Create setter function symbol if present
        if (node->setter) {
            auto void_type = typeSystem.get_void();
            auto setter_symbol = symbolTable.define_function("set", void_type);
            if (!setter_symbol) {
                error("Failed to define setter function for property '" + name + "'", node->location);
            }
        }
        
        // Exit property scope
        symbolTable.pop_scope();

        // Don't visit children - the accessor bodies will be handled during binding
    }

    void SymbolTableBuilder::visit(BlockSyntax *node)
    {
        // Create anonymous block scope
        auto block_symbol = symbolTable.define_block("$block");
        if (!block_symbol)
        {
            error("Failed to create block scope", node->location);
            return;
        }

        // Map AST node to block symbol so we can look it up during binding
        symbolTable.map_ast_to_symbol(node, block_symbol);

        symbolTable.push_scope(block_symbol);

        for (auto stmt : node->statements)
        {
            if (stmt)
                stmt->accept(this);
        }

        symbolTable.pop_scope();
    }

    void SymbolTableBuilder::visit(IfStmtSyntax *node)
    {
        // Visit condition in current scope
        if (node->condition)
            node->condition->accept(this);

        // Visit branches - they are BlockSyntax nodes that create their own scopes
        if (node->thenBranch)
            node->thenBranch->accept(this);
        if (node->elseBranch)
            node->elseBranch->accept(this);
    }

    void SymbolTableBuilder::visit(WhileStmtSyntax *node)
    {
        // Visit condition and body - body is BlockSyntax that creates its own scope
        if (node->condition)
            node->condition->accept(this);
        if (node->body)
            node->body->accept(this);
    }

    void SymbolTableBuilder::visit(ForStmtSyntax *node)
    {
        // Create a block scope for the entire for loop (to contain loop variable)
        auto for_block = symbolTable.define_block("$for");
        symbolTable.map_ast_to_symbol(node, for_block);
        symbolTable.push_scope(for_block);

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

        symbolTable.pop_scope();
    }

} // namespace Fern
