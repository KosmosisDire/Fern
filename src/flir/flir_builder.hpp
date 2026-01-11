#pragma once

#include "flir.hpp"
#include <stack>

namespace Fern::FLIR
{
    // Builder for structured control flow IR
    // Uses an emit target stack to handle nested control structures
    class FLIRBuilder {
        Function* current_func = nullptr;
        IRTypeSystem* ir_types = nullptr;

        // Stack of emission targets for nesting
        // Top of stack is where new instructions are emitted
        std::stack<InstructionList*> emit_stack;

    public:
        FLIRBuilder() = default;
        FLIRBuilder(IRTypeSystem* ts) : ir_types(ts) {}

        void set_function(Function* f) {
            current_func = f;
            // Clear emit stack and push function body as initial target
            while (!emit_stack.empty()) emit_stack.pop();
            emit_stack.push(&f->body);
        }

        void set_type_system(IRTypeSystem* ts) { ir_types = ts; }
        IRTypeSystem* types() const { return ir_types; }
        Function* function() const { return current_func; }

        // Get current emission target
        InstructionList* current_target() {
            return emit_stack.empty() ? nullptr : emit_stack.top();
        }

        // Push a new emission target (for entering nested structures)
        void push_target(InstructionList* target) {
            emit_stack.push(target);
        }

        // Pop emission target (for leaving nested structures)
        void pop_target() {
            if (!emit_stack.empty()) {
                emit_stack.pop();
            }
        }

        // Emit an instruction to current target
        template<typename T>
        T* emit(std::unique_ptr<T> inst) {
            T* ptr = inst.get();
            if (current_target()) {
                current_target()->push_back(std::move(inst));
            }
            return ptr;
        }

        // =====================================================================
        // Constants
        // =====================================================================

        Value* const_int(int64_t val, IRTypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstIntInst>(result, val);
            result->def = inst.get();
            emit(std::move(inst));
            return result;
        }

        Value* const_bool(bool val, IRTypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstBoolInst>(result, val);
            result->def = inst.get();
            emit(std::move(inst));
            return result;
        }

        Value* const_float(double val, IRTypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstFloatInst>(result, val);
            result->def = inst.get();
            emit(std::move(inst));
            return result;
        }

        Value* const_string(const std::string& val, IRTypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstStringInst>(result, val);
            result->def = inst.get();
            emit(std::move(inst));
            return result;
        }

        Value* const_null(IRTypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstNullInst>(result, type);
            result->def = inst.get();
            emit(std::move(inst));
            return result;
        }

        // =====================================================================
        // Memory - Allocation
        // =====================================================================

        Value* stack_alloc(IRTypePtr type, const std::string& name = "") {
            auto ptr_type = ir_types->get_pointer(type);
            auto result = current_func->create_value(ptr_type, name);
            auto inst = std::make_unique<StackAllocInst>(result, type);
            result->def = inst.get();
            emit(std::move(inst));
            return result;
        }

        Value* stack_alloc_bytes(Value* size, const std::string& name = "") {
            auto ptr_type = ir_types->get_pointer(ir_types->get_void());
            auto result = current_func->create_value(ptr_type, name);
            auto inst = std::make_unique<StackAllocBytesInst>(result, size);
            result->def = inst.get();
            size->uses.push_back(inst.get());
            emit(std::move(inst));
            return result;
        }

        Value* stack_alloc_nested(IRTypePtr type, const std::string& name = "") {
            return stack_alloc(ir_types->get_pointer(type), name);
        }

        Value* heap_alloc(IRTypePtr type, const std::string& name = "") {
            auto ptr_type = ir_types->get_pointer(type);
            auto result = current_func->create_value(ptr_type, name);
            auto inst = std::make_unique<HeapAllocInst>(result, type);
            result->def = inst.get();
            emit(std::move(inst));
            return result;
        }

        Value* heap_alloc_bytes(Value* size, const std::string& name = "") {
            auto ptr_type = ir_types->get_pointer(ir_types->get_void());
            auto result = current_func->create_value(ptr_type, name);
            auto inst = std::make_unique<HeapAllocBytesInst>(result, size);
            result->def = inst.get();
            size->uses.push_back(inst.get());
            emit(std::move(inst));
            return result;
        }

        Value* heap_alloc_nested(IRTypePtr type, const std::string& name = "") {
            return heap_alloc(ir_types->get_pointer(type), name);
        }

        Value* smart_alloc(IRTypePtr type, const std::string& name = "") {
            if (type->is_pointer() && type->pointee) {
                return heap_alloc(type->pointee, name);
            }
            return stack_alloc(type, name);
        }

        void heap_free(Value* ptr) {
            auto inst = std::make_unique<HeapFreeInst>(ptr);
            ptr->uses.push_back(inst.get());
            emit(std::move(inst));
        }

        // =====================================================================
        // Memory - Load/Store
        // =====================================================================

        Value* load(Value* addr, IRTypePtr type, const std::string& name = "") {
            auto result = current_func->create_value(type, name);
            auto inst = std::make_unique<LoadInst>(result, addr);
            result->def = inst.get();
            addr->uses.push_back(inst.get());
            emit(std::move(inst));
            return result;
        }

        void store(Value* val, Value* addr) {
            auto inst = std::make_unique<StoreInst>(val, addr);
            val->uses.push_back(inst.get());
            addr->uses.push_back(inst.get());
            emit(std::move(inst));
        }

        // =====================================================================
        // Memory - Field/Element Access
        // =====================================================================

        Value* field_addr(Value* object, uint32_t field_index, IRTypePtr field_type,
                          const std::string& field_name = "", IRTypePtr struct_type = nullptr,
                          const std::string& result_name = "") {
            auto ptr_type = ir_types->get_pointer(field_type);
            auto result = current_func->create_value(ptr_type, result_name.empty() ? field_name : result_name);
            auto inst = std::make_unique<FieldAddrInst>(result, object, field_index, field_name, struct_type, field_type);
            result->def = inst.get();
            object->uses.push_back(inst.get());
            emit(std::move(inst));
            return result;
        }

        Value* element_addr(Value* array, Value* index, IRTypePtr element_type, const std::string& name = "") {
            auto ptr_type = ir_types->get_pointer(element_type);
            auto result = current_func->create_value(ptr_type, name);
            auto inst = std::make_unique<ElementAddrInst>(result, array, index);
            result->def = inst.get();
            array->uses.push_back(inst.get());
            index->uses.push_back(inst.get());
            emit(std::move(inst));
            return result;
        }

        // =====================================================================
        // Memory - Bulk Operations
        // =====================================================================

        void memcpy(Value* dest, Value* src, Value* size, bool is_volatile = false) {
            auto inst = std::make_unique<MemCpyInst>(dest, src, size, is_volatile);
            dest->uses.push_back(inst.get());
            src->uses.push_back(inst.get());
            size->uses.push_back(inst.get());
            emit(std::move(inst));
        }

        void memset(Value* dest, Value* value, Value* size, bool is_volatile = false) {
            auto inst = std::make_unique<MemSetInst>(dest, value, size, is_volatile);
            dest->uses.push_back(inst.get());
            value->uses.push_back(inst.get());
            size->uses.push_back(inst.get());
            emit(std::move(inst));
        }

        // =====================================================================
        // Arithmetic/Logic/Comparison
        // =====================================================================

        Value* binary(Opcode op, Value* left, Value* right) {
            IRTypePtr result_type;
            if (op == Opcode::Eq || op == Opcode::Ne ||
                op == Opcode::Lt || op == Opcode::Le ||
                op == Opcode::Gt || op == Opcode::Ge) {
                result_type = ir_types->get_bool();
            } else {
                result_type = left->type;
            }

            auto result = current_func->create_value(result_type);
            auto inst = std::make_unique<BinaryInst>(op, result, left, right);
            result->def = inst.get();
            left->uses.push_back(inst.get());
            right->uses.push_back(inst.get());
            emit(std::move(inst));
            return result;
        }

        Value* unary(Opcode op, Value* operand) {
            auto result = current_func->create_value(operand->type);
            auto inst = std::make_unique<UnaryInst>(op, result, operand);
            result->def = inst.get();
            operand->uses.push_back(inst.get());
            emit(std::move(inst));
            return result;
        }

        Value* convert(Value* value, IRTypePtr target_type) {
            auto result = current_func->create_value(target_type);
            auto inst = std::make_unique<ConvertInst>(result, value, target_type);
            result->def = inst.get();
            value->uses.push_back(inst.get());
            emit(std::move(inst));
            return result;
        }

        // =====================================================================
        // Function Call/Return
        // =====================================================================

        Value* call(Function* func, std::vector<Value*> args, const std::string& name = "") {
            Value* result = nullptr;
            if (func->return_type && !func->return_type->is_void()) {
                result = current_func->create_value(func->return_type, name);
            }
            auto inst = std::make_unique<CallInst>(result, func, args);
            if (result) result->def = inst.get();
            for (auto arg : args) {
                arg->uses.push_back(inst.get());
            }
            emit(std::move(inst));
            return result;
        }

        Value* call_indirect(Value* func_ptr, IRTypePtr signature, std::vector<Value*> args,
                             IRTypePtr return_type, const std::string& name = "") {
            Value* result = nullptr;
            if (return_type && !return_type->is_void()) {
                result = current_func->create_value(return_type, name);
            }
            auto inst = std::make_unique<CallIndirectInst>(result, func_ptr, signature, args);
            if (result) result->def = inst.get();
            func_ptr->uses.push_back(inst.get());
            for (auto arg : args) {
                arg->uses.push_back(inst.get());
            }
            emit(std::move(inst));
            return result;
        }

        void emit_return(Value* val = nullptr) {
            auto inst = std::make_unique<ReturnInst>(val);
            if (val) val->uses.push_back(inst.get());
            emit(std::move(inst));
        }

        // =====================================================================
        // Control Flow
        // =====================================================================

        std::unique_ptr<IfInst> create_if(Value* condition) {
            auto inst = std::make_unique<IfInst>(condition);
            condition->uses.push_back(inst.get());
            return inst;
        }

        void emit_if(std::unique_ptr<IfInst> if_inst) {
            emit(std::move(if_inst));
        }

        std::unique_ptr<LoopInst> create_loop(const std::string& label = "") {
            auto inst = std::make_unique<LoopInst>();
            inst->label = label;
            return inst;
        }

        void emit_loop(std::unique_ptr<LoopInst> loop_inst) {
            emit(std::move(loop_inst));
        }

        std::unique_ptr<BlockInst> create_block(const std::string& label = "") {
            auto inst = std::make_unique<BlockInst>();
            inst->label = label;
            return inst;
        }

        void emit_block(std::unique_ptr<BlockInst> block_inst) {
            emit(std::move(block_inst));
        }

        void br(uint32_t depth = 0) {
            emit(std::make_unique<BrInst>(depth));
        }

        void br_if(Value* condition, uint32_t depth = 0) {
            auto inst = std::make_unique<BrIfInst>(condition, depth);
            condition->uses.push_back(inst.get());
            emit(std::move(inst));
        }

        void emit_continue(uint32_t depth = 0) {
            emit(std::make_unique<ContinueInst>(depth));
        }

        void continue_if(Value* condition, uint32_t depth = 0) {
            auto inst = std::make_unique<ContinueIfInst>(condition, depth);
            condition->uses.push_back(inst.get());
            emit(std::move(inst));
        }

    };
}
