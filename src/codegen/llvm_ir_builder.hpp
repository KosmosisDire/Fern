// llvm_ir_builder.hpp - LLVM IR Building Utilities
#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Function.h>
#include <initializer_list>
#include <string>
#include <vector>

namespace Fern
{
    /**
     * @brief Wraps common LLVM IR construction patterns
     *
     * This class provides convenience methods for LLVM IR generation
     * without any knowledge of HLIR. It's a thin wrapper around IRBuilder
     * that reduces boilerplate.
     */
    class LLVMIRBuilder
    {
    private:
        llvm::LLVMContext& context;
        llvm::IRBuilder<>& builder;
        llvm::Module* module;

    public:
        LLVMIRBuilder(llvm::LLVMContext& ctx, llvm::IRBuilder<>& bldr, llvm::Module* mod)
            : context(ctx), builder(bldr), module(mod) {}

        // === Constants ===
        llvm::Value* i1_constant(bool value);
        llvm::Value* i8_constant(int8_t value);
        llvm::Value* i32_constant(int32_t value);
        llvm::Value* i64_constant(int64_t value);
        llvm::Value* f32_constant(float value);
        llvm::Value* f64_constant(double value);

        // === Types ===
        llvm::Type* void_type();
        llvm::Type* i1_type();
        llvm::Type* i8_type();
        llvm::Type* i32_type();
        llvm::Type* i64_type();
        llvm::Type* f32_type();
        llvm::Type* f64_type();
        llvm::Type* ptr_type();

        // === Memory Operations ===
        llvm::Value* create_alloca(llvm::Type* type, const std::string& name = "");
        llvm::Value* create_malloc(llvm::Type* type, const std::string& name = "");
        llvm::Value* create_load(llvm::Type* type, llvm::Value* ptr, const std::string& name = "");
        void create_store(llvm::Value* value, llvm::Value* ptr);

        // === GEP Operations ===
        llvm::Value* create_struct_gep(llvm::Type* struct_type, llvm::Value* ptr,
                                       uint32_t index, const std::string& name = "");

        llvm::Value* create_gep(llvm::Type* type, llvm::Value* ptr,
                               llvm::Value* index, const std::string& name = "");

        llvm::Value* create_gep(llvm::Type* type, llvm::Value* ptr,
                               std::initializer_list<llvm::Value*> indices,
                               const std::string& name = "");

        llvm::Value* create_inbounds_gep(llvm::Type* type, llvm::Value* ptr,
                                        std::initializer_list<llvm::Value*> indices,
                                        const std::string& name = "");

        // === Arithmetic Operations ===
        llvm::Value* create_add(llvm::Value* left, llvm::Value* right, bool is_float,
                               const std::string& name = "");
        llvm::Value* create_sub(llvm::Value* left, llvm::Value* right, bool is_float,
                               const std::string& name = "");
        llvm::Value* create_mul(llvm::Value* left, llvm::Value* right, bool is_float,
                               const std::string& name = "");
        llvm::Value* create_div(llvm::Value* left, llvm::Value* right,
                               bool is_float, bool is_signed, const std::string& name = "");
        llvm::Value* create_rem(llvm::Value* left, llvm::Value* right,
                               bool is_float, bool is_signed, const std::string& name = "");

        // === Comparison Operations ===
        llvm::Value* create_eq(llvm::Value* left, llvm::Value* right, bool is_float,
                              const std::string& name = "");
        llvm::Value* create_ne(llvm::Value* left, llvm::Value* right, bool is_float,
                              const std::string& name = "");
        llvm::Value* create_lt(llvm::Value* left, llvm::Value* right,
                              bool is_float, bool is_signed, const std::string& name = "");
        llvm::Value* create_le(llvm::Value* left, llvm::Value* right,
                              bool is_float, bool is_signed, const std::string& name = "");
        llvm::Value* create_gt(llvm::Value* left, llvm::Value* right,
                              bool is_float, bool is_signed, const std::string& name = "");
        llvm::Value* create_ge(llvm::Value* left, llvm::Value* right,
                              bool is_float, bool is_signed, const std::string& name = "");

        // === Bitwise Operations ===
        llvm::Value* create_and(llvm::Value* left, llvm::Value* right,
                               const std::string& name = "");
        llvm::Value* create_or(llvm::Value* left, llvm::Value* right,
                              const std::string& name = "");
        llvm::Value* create_xor(llvm::Value* left, llvm::Value* right,
                               const std::string& name = "");
        llvm::Value* create_shl(llvm::Value* left, llvm::Value* right,
                               const std::string& name = "");
        llvm::Value* create_shr(llvm::Value* left, llvm::Value* right, bool is_signed,
                               const std::string& name = "");

        // === Unary Operations ===
        llvm::Value* create_neg(llvm::Value* operand, bool is_float,
                               const std::string& name = "");
        llvm::Value* create_not(llvm::Value* operand, const std::string& name = "");

        // === Cast Operations ===
        llvm::Value* create_int_cast(llvm::Value* value, llvm::Type* target_type,
                                    bool is_signed, const std::string& name = "");
        llvm::Value* create_float_cast(llvm::Value* value, llvm::Type* target_type,
                                      const std::string& name = "");
        llvm::Value* create_int_to_float(llvm::Value* value, llvm::Type* target_type,
                                        bool is_signed, const std::string& name = "");
        llvm::Value* create_float_to_int(llvm::Value* value, llvm::Type* target_type,
                                        bool is_signed, const std::string& name = "");

        // === Control Flow ===
        void create_ret(llvm::Value* value = nullptr);
        void create_br(llvm::BasicBlock* target);
        void create_cond_br(llvm::Value* condition, llvm::BasicBlock* true_block,
                           llvm::BasicBlock* false_block);
        llvm::PHINode* create_phi(llvm::Type* type, unsigned num_incoming,
                                  const std::string& name = "");

        // === Function Calls ===
        llvm::Value* create_call(llvm::Function* callee,
                                std::vector<llvm::Value*> args,
                                const std::string& name = "");

        // === String Constants ===
        llvm::Value* create_global_string(const std::string& str,
                                         const std::string& name = ".str");

        // === Helper Queries ===
        size_t get_type_size(llvm::Type* type);

        // Direct access to underlying builder when needed
        llvm::IRBuilder<>& get_builder() { return builder; }
        llvm::LLVMContext& get_context() { return context; }
    };

} // namespace Fern
