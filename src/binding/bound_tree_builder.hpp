#pragma once

#include "bound_tree.hpp"
#include "ast/ast.hpp"
#include "binding_arena.hpp"
#include "semantic/symbol_table.hpp"
#include "common/error.hpp"

namespace Fern
{
    class BoundTreeBuilder : public DiagnosticSystem
    {
    private:
        BindingArena& arena;
        SymbolTable& symbol_table;
        
        #pragma region Symbol Resolution Helpers
        
        // Simple symbol lookup
        Symbol* resolve_symbol(const std::vector<std::string>& parts)
        {
            return symbol_table.resolve(parts)[0];
        }

        // Get current containing type for 'this' expressions
        TypeSymbol* get_containing_type()
        {
            auto current = symbol_table.get_current_scope();
            // First check if current scope itself is a type
            if (current && current->is<TypeSymbol>())
            {
                return current->as<TypeSymbol>();
            }
            // Otherwise look for enclosing type (for nested scopes)
            return current ? current->get_enclosing<TypeSymbol>() : nullptr;
        }
        
        bool matches_signature(FunctionSymbol* func, const std::vector<TypePtr>& arg_types)
        {
            if (func->parameters.size() != arg_types.size()) return false;
            
            for (size_t i = 0; i < arg_types.size(); ++i)
            {
                if (func->parameters[i]->type != arg_types[i])
                {
                    return false;
                }
            }
            return true;
        }
        
        #pragma region Scope Management Helpers
        
        class ScopeGuard
        {
            SymbolTable& table;
            bool pushed;
        public:
            ScopeGuard(SymbolTable& table, Symbol* symbol) 
                : table(table), pushed(symbol != nullptr)
            {
                if (pushed)
                {
                    table.push_scope(symbol);
                }
            }
            ~ScopeGuard()
            {
                if (pushed)
                {
                    table.pop_scope();
                }
            }
        };
        
    public:
        BoundTreeBuilder(BindingArena& arena, SymbolTable& symbol_table);
        
        // Main entry point
        BoundCompilationUnit* bind(CompilationUnitSyntax* syntax);
        
    private:

        #pragma region Statements

        BoundStatement* bind_statement(BaseStmtSyntax* syntax);
        BoundBlockStatement* bind_block(BlockSyntax* syntax);
        BoundVariableDeclaration* bind_variable_declaration(VariableDeclSyntax* syntax);
        BoundFunctionDeclaration* bind_function_declaration(FunctionDeclSyntax* syntax);
        BoundFunctionDeclaration* bind_constructor_declaration(ConstructorDeclSyntax* syntax);
        BoundTypeDeclaration* bind_type_declaration(TypeDeclSyntax* syntax);
        BoundNamespaceDeclaration* bind_namespace_declaration(NamespaceDeclSyntax* syntax);
        BoundPropertyDeclaration* bind_property_declaration(PropertyDeclSyntax* syntax);
        BoundIfStatement* bind_if_statement(IfStmtSyntax* syntax);
        BoundWhileStatement* bind_while_statement(WhileStmtSyntax* syntax);
        BoundForStatement* bind_for_statement(ForStmtSyntax* syntax);
        BoundReturnStatement* bind_return_statement(ReturnStmtSyntax* syntax);
        BoundBreakStatement* bind_break_statement(BreakStmtSyntax* syntax);
        BoundContinueStatement* bind_continue_statement(ContinueStmtSyntax* syntax);
        BoundExpressionStatement* bind_expression_statement(ExpressionStmtSyntax* syntax);
        BoundUsingStatement* bind_using_statement(UsingDirectiveSyntax* syntax);

        #pragma region Expressions

        BoundExpression* bind_expression(BaseExprSyntax* syntax);
        BoundLiteralExpression* bind_literal(LiteralExprSyntax* syntax);
        BoundExpression* bind_name(BaseNameExprSyntax* syntax);
        BoundBinaryExpression* bind_binary_expression(BinaryExprSyntax* syntax);
        BoundUnaryExpression* bind_unary_expression(UnaryExprSyntax* syntax);
        BoundAssignmentExpression* bind_assignment_expression(AssignmentExprSyntax* syntax);
        BoundCallExpression* bind_call_expression(CallExprSyntax* syntax);
        BoundMemberAccessExpression* bind_member_access(MemberAccessExprSyntax* syntax);
        BoundIndexExpression* bind_index_expression(IndexerExprSyntax* syntax);
        BoundCastExpression* bind_cast_expression(CastExprSyntax* syntax);
        BoundNewExpression* bind_new_expression(NewExprSyntax* syntax);
        BoundThisExpression* bind_this_expression(ThisExprSyntax* syntax);
        BoundArrayCreationExpression* bind_array_creation(ArrayLiteralExprSyntax* syntax);

        #pragma region Types

        BoundTypeExpression* bind_type_expression(BaseExprSyntax* syntax);
    };
    
} // namespace Fern