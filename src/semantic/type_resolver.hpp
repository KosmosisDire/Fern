#pragma once

#include "binding/bound_tree.hpp"
#include "semantic/symbol_table.hpp"
#include "semantic/type_system.hpp"
#include "binding/conversions.hpp"
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>

namespace Fern
{

    class TypeResolver : public BoundVisitor
    {
    private:
        SymbolTable& symbolTable;
        TypeSystem& typeSystem;
        std::vector<std::string> errors;
        
        // Type inference via unification
        std::unordered_map<TypePtr, TypePtr> substitution;
        std::unordered_set<TypePtr> pendingConstraints;
        std::unordered_map<TypePtr, BoundNode*> constraintNodes;  // Track where each constraint originated
        
        // Context tracking
        FunctionSymbol* currentFunction = nullptr;
        TypeSymbol* currentType = nullptr;  // For 'this' resolution
        
        // Multiple passes for type inference
        static constexpr int MAX_PASSES = 10;
        
    public:
        explicit TypeResolver(SymbolTable& st) 
            : symbolTable(st), typeSystem(st.get_type_system()) {}
        
        bool resolve(BoundCompilationUnit* unit);
        const std::vector<std::string>& get_errors() const { return errors; }
        
    private:
        // === Core Type Resolution ===
        TypePtr apply_substitution(TypePtr type);
        void unify(TypePtr t1, TypePtr t2, BoundNode* error_node, const std::string& context);
        void annotate_expression(BoundExpression* expr, TypePtr type, Symbol* symbol = nullptr);
        
        // === Symbol Resolution ===
        Symbol* resolve_qualified_name(const std::vector<std::string>& parts);
        FunctionSymbol* resolve_overload(const std::vector<FunctionSymbol*>& overloads, 
                                        const std::vector<TypePtr>& argTypes);
        
        // === Type Utilities ===
        TypePtr resolve_type_expression(BoundExpression* typeExpr);
        TypePtr infer_return_type(BoundStatement* body);
        ValueCategory compute_value_category(BoundExpression* expr, Symbol* symbol = nullptr);
        
        // === Conversion Checking ===
        ConversionKind check_conversion(TypePtr from, TypePtr to);
        bool check_implicit_conversion(TypePtr from, TypePtr to, BoundNode* node, const std::string& context);
        bool check_explicit_conversion(TypePtr from, TypePtr to, BoundNode* node, const std::string& context);
        
        // === Error Reporting ===
        void report_error(BoundNode* node, const std::string& message);
        void report_final_errors();
        
        // === Visitor Implementations ===
        // Expressions
        void visit(BoundLiteralExpression* node) override;
        void visit(BoundNameExpression* node) override;
        void visit(BoundBinaryExpression* node) override;
        void visit(BoundUnaryExpression* node) override;
        void visit(BoundAssignmentExpression* node) override;
        void visit(BoundCallExpression* node) override;
        void visit(BoundMemberAccessExpression* node) override;
        void visit(BoundIndexExpression* node) override;
        void visit(BoundNewExpression* node) override;
        void visit(BoundArrayCreationExpression* node) override;
        void visit(BoundCastExpression* node) override;
        void visit(BoundConditionalExpression* node) override;
        void visit(BoundThisExpression* node) override;
        void visit(BoundTypeOfExpression* node) override;
        void visit(BoundSizeOfExpression* node) override;
        void visit(BoundParenthesizedExpression* node) override;
        void visit(BoundConversionExpression* node) override;
        void visit(BoundTypeExpression* node) override;
        
        // Statements
        void visit(BoundBlockStatement* node) override;
        void visit(BoundExpressionStatement* node) override;
        void visit(BoundIfStatement* node) override;
        void visit(BoundWhileStatement* node) override;
        void visit(BoundForStatement* node) override;
        void visit(BoundBreakStatement* node) override;
        void visit(BoundContinueStatement* node) override;
        void visit(BoundReturnStatement* node) override;
        void visit(BoundUsingStatement* node) override;
        
        // Declarations
        void visit(BoundVariableDeclaration* node) override;
        void visit(BoundFunctionDeclaration* node) override;
        void visit(BoundPropertyDeclaration* node) override;
        void visit(BoundTypeDeclaration* node) override;
        void visit(BoundNamespaceDeclaration* node) override;
        
        // Top-level
        void visit(BoundCompilationUnit* node) override;
    };
    
} // namespace Fern