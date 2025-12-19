// bound_to_hlir.hpp
#pragma once

#include "binding/bound_tree.hpp"
#include "semantic/type_system.hpp"
#include "hlir_builder.hpp"
#include "common/error.hpp"
#include <unordered_map>
#include <stack>
#include <optional>
#include <functional>

namespace Fern::HLIR
{

#pragma region Types

struct LoweredExpr {
    HLIR::Value* result = nullptr;
    bool is_address = false;
};

struct LoopContext {
    HLIR::BasicBlock* continue_target;
    HLIR::BasicBlock* break_target;
};

#pragma endregion

class BoundToHLIR : public BoundVisitor, public DiagnosticSystem {
public:
    BoundToHLIR(HLIR::Module* mod, TypeSystem* types)
        : DiagnosticSystem("BoundToHLIR")
        , module(mod)
        , builder(types)
        , type_system(types) {}

    void build(BoundCompilationUnit* unit);

    #pragma region Expression Visitors

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
    void visit(BoundThisExpression* node) override;
    void visit(BoundParenthesizedExpression* node) override;
    void visit(BoundConversionExpression* node) override;
    void visit(BoundTypeExpression* node) override;

    #pragma endregion

    #pragma region Statement Visitors

    void visit(BoundBlockStatement* node) override;
    void visit(BoundExpressionStatement* node) override;
    void visit(BoundIfStatement* node) override;
    void visit(BoundWhileStatement* node) override;
    void visit(BoundForStatement* node) override;
    void visit(BoundBreakStatement* node) override;
    void visit(BoundContinueStatement* node) override;
    void visit(BoundReturnStatement* node) override;
    void visit(BoundUsingStatement* node) override;

    #pragma endregion

    #pragma region Declaration Visitors

    void visit(BoundVariableDeclaration* node) override;
    void visit(BoundFunctionDeclaration* node) override;
    void visit(BoundPropertyDeclaration* node) override;
    void visit(BoundTypeDeclaration* node) override;
    void visit(BoundNamespaceDeclaration* node) override;
    void visit(BoundCompilationUnit* node) override;

    #pragma endregion

    #pragma region Public State (for intrinsic handlers)

    HLIR::HLIRBuilder builder;

    #pragma endregion

private:
    friend class ScopedFunctionContext;

    #pragma region Core State

    HLIR::Module* module;
    TypeSystem* type_system;

    HLIR::Function* current_function = nullptr;
    HLIR::BasicBlock* current_block = nullptr;

    std::unordered_map<Symbol*, HLIR::Value*> variable_addresses;
    std::unordered_map<BoundExpression*, LoweredExpr> lowered;
    std::stack<LoopContext> loop_stack;

    #pragma endregion

    #pragma region Expression Evaluation

    HLIR::Value* emit_rvalue(BoundExpression* expr);
    HLIR::Value* emit_lvalue(BoundExpression* expr);

    #pragma endregion

    #pragma region Helpers

    HLIR::Value* get_this_param();
    size_t get_field_index(TypeSymbol* type_sym, Symbol* field_sym);
    HLIR::BasicBlock* create_block(const std::string& name);
    void branch_if_open(HLIR::BasicBlock* target);

    void emit_store(HLIR::Value* dest, HLIR::Value* src, TypePtr type);
    void emit_string_init(HLIR::Value* string_addr, HLIR::Value* data_ptr, size_t length);

    std::optional<HLIR::Value*> try_pointer_arithmetic(
        BoundBinaryExpression* node,
        HLIR::Value* left,
        HLIR::Value* right);

    std::optional<HLIR::Value*> try_emit_intrinsic(
        FunctionSymbol* method,
        const std::vector<HLIR::Value*>& args,
        const SourceRange& loc);

    #pragma endregion

    #pragma region Opcode Mapping

    HLIR::Opcode get_binary_opcode(BinaryOperatorKind kind);
    HLIR::Opcode get_unary_opcode(UnaryOperatorKind kind);
    HLIR::Opcode get_compound_opcode(AssignmentOperatorKind kind);

    #pragma endregion

    #pragma region Code Generation

    void generate_property_accessor(
        BoundPropertyDeclaration* prop_decl,
        BoundPropertyAccessor* accessor,
        bool is_getter);

    #pragma endregion
};

#pragma region Scoped Context

class ScopedFunctionContext {
public:
    ScopedFunctionContext(BoundToHLIR& hlir, HLIR::Function* func, HLIR::BasicBlock* block)
        : self(hlir)
        , prev_function(hlir.current_function)
        , prev_block(hlir.current_block) 
    {
        self.current_function = func;
        self.current_block = block;
        self.builder.set_function(func);
        self.builder.set_block(block);
    }
    
    ~ScopedFunctionContext() {
        self.current_function = prev_function;
        self.current_block = prev_block;
        if (prev_function) {
            self.builder.set_function(prev_function);
            if (prev_block) {
                self.builder.set_block(prev_block);
            }
        }
    }

    ScopedFunctionContext(const ScopedFunctionContext&) = delete;
    ScopedFunctionContext& operator=(const ScopedFunctionContext&) = delete;

private:
    BoundToHLIR& self;
    HLIR::Function* prev_function;
    HLIR::BasicBlock* prev_block;
};

#pragma endregion

} // namespace Fern::HLIR