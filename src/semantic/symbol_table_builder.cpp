#include "symbol_table_builder.hpp"
#include <iostream>

namespace Fern
{

    // === Core Helper Methods ===

    void SymbolTableBuilder::push_error(const std::string &error)
    {
        errors.push_back(error);
    }

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

    // === Visitor Implementations ===

    // Annotate all nodes with their containing scope
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
            push_error("Failed to define namespace '" + name + "'");
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
            push_error("Failed to define type '" + name + "'");
            return;
        }

        // Apply modifiers
        if (has_flag(node->modifiers, ModifierKindFlags::Static))
        {
            type_symbol->isStatic = true;
        }
        if (has_flag(node->modifiers, ModifierKindFlags::Abstract))
        {
            type_symbol->isAbstract = true;
        }

        // Set access level
        type_symbol->access = get_access_modifier(node->modifiers);

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
            push_error("Failed to define function '" + name + "'");
            return;
        }

        // Apply modifiers
        if (has_flag(node->modifiers, ModifierKindFlags::Static))
        {
            func_symbol->isStatic = true;
        }
        if (has_flag(node->modifiers, ModifierKindFlags::Virtual))
        {
            func_symbol->isVirtual = true;
        }
        if (has_flag(node->modifiers, ModifierKindFlags::Override))
        {
            func_symbol->isOverride = true;
        }
        if (has_flag(node->modifiers, ModifierKindFlags::Abstract))
        {
            func_symbol->isAbstract = true;
        }
        if (has_flag(node->modifiers, ModifierKindFlags::Extern))
        {
            func_symbol->isExtern = true;
            // Validate: extern functions must not have a body
            if (node->body != nullptr)
            {
                push_error("External function '" + name + "' cannot have a body");
            }
        }

        func_symbol->access = get_access_modifier(node->modifiers);

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
            push_error("Constructor must be defined within a type");
            return;
        }

        std::string name = "New";  // Use "New" instead of type name to avoid collision
        TypePtr return_type = typeSystem.get_primitive("void");

        auto func_symbol = symbolTable.define_function(name, return_type);
        if (!func_symbol)
        {
            push_error("Failed to define constructor for type '" + name + "'");
            return;
        }

        // Mark as constructor
        func_symbol->is_constructor = true;
        func_symbol->access = get_access_modifier(node->modifiers);
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
            push_error("Failed to define parameter '" + name + "'");
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

        // Determine if we're in a type (field) or function/block (local)
        auto current = symbolTable.get_current_scope();
        if (current && current->kind == SymbolKind::Type)
        {
            // Field
            auto field_symbol = symbolTable.define_field(name, type);
            if (!field_symbol)
            {
                push_error("Failed to define field '" + name + "'");
            }
        }
        else
        {
            // Local variable
            auto local_symbol = symbolTable.define_local(name, type);
            if (!local_symbol)
            {
                push_error("Failed to define local variable '" + name + "'");
            }
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
            push_error("Failed to define property '" + name + "'");
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
                push_error("Failed to define getter function for property '" + name + "'");
            } else {
                getter_symbol->isStatic = false; // Properties are instance members
            }
        }
        
        // Create setter function symbol if present  
        if (node->setter) {
            auto void_type = typeSystem.get_void();
            auto setter_symbol = symbolTable.define_function("set", void_type);
            if (!setter_symbol) {
                push_error("Failed to define setter function for property '" + name + "'");
            } else {
                setter_symbol->isStatic = false; // Properties are instance members
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
            push_error("Failed to create block scope");
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
