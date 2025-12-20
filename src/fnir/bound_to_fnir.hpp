// bound_to_fnir.hpp
#pragma once

#include "binding/bound_tree.hpp"
#include "fnir_builder.hpp"
#include "common/error.hpp"
#include <unordered_map>
#include <stack>
#include <optional>
#include <functional>

namespace Fern::FNIR
{

#pragma region Types

struct LoweredExpr {
    FNIR::Value* result = nullptr;
    bool is_address = false;
};

struct LoopContext {
    FNIR::BasicBlock* continue_target;
    FNIR::BasicBlock* break_target;
};

#pragma endregion

class BoundToFNIR : public BoundVisitor, public DiagnosticSystem {
public:
    BoundToFNIR(FNIR::Module* mod)
        : DiagnosticSystem("BoundToFNIR")
        , module(mod)
        , builder(&mod->ir_types) {}

    // Initialize module with types and function declarations (call once)
    void init_module(NamespaceSymbol* global_ns);

    // Generate function bodies for a compilation unit (call per file)
    void generate(BoundCompilationUnit* unit);

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

    FNIR::FNIRBuilder builder;

    #pragma endregion

private:
    friend class ScopedFunctionContext;

    #pragma region Core State

    FNIR::Module* module;

    FNIR::Function* current_function = nullptr;
    FNIR::BasicBlock* current_block = nullptr;

    std::unordered_map<Symbol*, FNIR::Value*> variable_addresses;
    std::unordered_map<BoundExpression*, LoweredExpr> lowered;
    std::stack<LoopContext> loop_stack;

    #pragma endregion

    #pragma region Expression Evaluation

    FNIR::Value* emit_rvalue(BoundExpression* expr);
    FNIR::Value* emit_lvalue(BoundExpression* expr);

    #pragma endregion

    #pragma region Helpers

    // Type conversion helper
    FNIR::IRTypePtr convert(TypePtr type) { return module->ir_types.convert(type); }

    FNIR::Value* get_this_param();
    size_t get_field_index(TypeSymbol* type_sym, Symbol* field_sym);
    FNIR::BasicBlock* create_block(const std::string& name);
    void branch_if_open(FNIR::BasicBlock* target);

    void emit_store(FNIR::Value* dest, FNIR::Value* src, FNIR::IRTypePtr type);
    void emit_string_init(FNIR::Value* string_addr, FNIR::Value* data_ptr, size_t length);

    std::optional<FNIR::Value*> try_pointer_arithmetic(
        BoundBinaryExpression* node,
        FNIR::Value* left,
        FNIR::Value* right);

    std::optional<FNIR::Value*> try_emit_intrinsic(
        FunctionSymbol* method,
        const std::vector<FNIR::Value*>& args,
        const SourceRange& loc);

    #pragma endregion

    #pragma region Opcode Mapping

    FNIR::Opcode get_binary_opcode(BinaryOperatorKind kind);
    FNIR::Opcode get_unary_opcode(UnaryOperatorKind kind);
    FNIR::Opcode get_compound_opcode(AssignmentOperatorKind kind);

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
    ScopedFunctionContext(BoundToFNIR& fnir, FNIR::Function* func, FNIR::BasicBlock* block)
        : self(fnir)
        , prev_function(fnir.current_function)
        , prev_block(fnir.current_block) 
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
    BoundToFNIR& self;
    FNIR::Function* prev_function;
    FNIR::BasicBlock* prev_block;
};

#pragma endregion

} // namespace Fern::FNIR