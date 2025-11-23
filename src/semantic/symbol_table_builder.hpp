#pragma once

#include "ast/ast.hpp"
#include "symbol_table.hpp"
#include "common/error.hpp"
#include <vector>
#include <string>

namespace Fern
{

class SymbolTableBuilder : public DefaultVisitor, public DiagnosticSystem
{
private:
    SymbolTable& symbolTable;
    TypeSystem& typeSystem;

    // Track parameter indices within function scopes
    uint32_t currentParameterIndex = 0;

    // === Core Helper Methods ===

    TypePtr get_type_from_expr(BaseExprSyntax* typeExpr);

public:
    explicit SymbolTableBuilder(SymbolTable& st)
        : DiagnosticSystem("SymbolTableBuilder"),
          symbolTable(st), typeSystem(st.get_type_system()) {}

    void build(CompilationUnitSyntax* unit);

    // === Visitor Implementations ===
    
    void visit(BaseSyntax* node) override;
    void visit(CompilationUnitSyntax* node) override;
    void visit(NamespaceDeclSyntax* node) override;
    void visit(TypeDeclSyntax* node) override;
    void visit(FunctionDeclSyntax* node) override;
    void visit(ConstructorDeclSyntax* node) override;
    void visit(ParameterDeclSyntax* node) override;
    void visit(VariableDeclSyntax* node) override;
    void visit(PropertyDeclSyntax* node) override;
    void visit(BlockSyntax* node) override;
    void visit(IfStmtSyntax* node) override;
    void visit(WhileStmtSyntax* node) override;
    void visit(ForStmtSyntax* node) override;
};

} // namespace Fern
