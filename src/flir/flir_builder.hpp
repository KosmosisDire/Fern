#pragma once

#include "flir.hpp"

namespace Fern::FLIR
{
    class FLIRBuilder {
        Function* current_func = nullptr;
        BasicBlock* current_block = nullptr;
        IRTypeSystem* ir_types = nullptr;

    public:
        FLIRBuilder() = default;
        FLIRBuilder(IRTypeSystem* ts) : ir_types(ts) {}

        void set_function(Function* f) { current_func = f; }
        void set_block(BasicBlock* b) { current_block = b; }
        void set_type_system(IRTypeSystem* ts) { ir_types = ts; }
        IRTypeSystem* types() const { return ir_types; }

        Value* const_int(int64_t val, IRTypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstIntInst>(result, val);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }

        Value* const_bool(bool val, IRTypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstBoolInst>(result, val);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }

        Value* const_float(double val, IRTypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstFloatInst>(result, val);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }

        Value* const_string(const std::string& val, IRTypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstStringInst>(result, val);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }

        Value* const_null(IRTypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstNullInst>(result, type);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }

        // Stack allocation of a typed value
        Value* stack_alloc(IRTypePtr type, const std::string& name = "") {
            auto ptr_type = ir_types->get_pointer(type);
            auto result = current_func->create_value(ptr_type, name);
            auto inst = std::make_unique<StackAllocInst>(result, type);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }

        // Stack allocation of dynamic byte count
        Value* stack_alloc_bytes(Value* size, const std::string& name = "") {
            auto ptr_type = ir_types->get_pointer(ir_types->get_void());
            auto result = current_func->create_value(ptr_type, name);
            auto inst = std::make_unique<StackAllocBytesInst>(result, size);
            result->def = inst.get();
            size->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            return result;
        }

        // Stack allocation of pointer to type (for nested pointers like T**)
        Value* stack_alloc_nested(IRTypePtr type, const std::string& name = "") {
            return stack_alloc(ir_types->get_pointer(type), name);
        }

        // Heap allocation of a typed value (zero-initialized via calloc during codegen)
        Value* heap_alloc(IRTypePtr type, const std::string& name = "") {
            auto ptr_type = ir_types->get_pointer(type);
            auto result = current_func->create_value(ptr_type, name);
            auto inst = std::make_unique<HeapAllocInst>(result, type);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }

        // Heap allocation of dynamic byte count
        Value* heap_alloc_bytes(Value* size, const std::string& name = "") {
            auto ptr_type = ir_types->get_pointer(ir_types->get_void());
            auto result = current_func->create_value(ptr_type, name);
            auto inst = std::make_unique<HeapAllocBytesInst>(result, size);
            result->def = inst.get();
            size->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            return result;
        }

        // Heap allocation of pointer to type (for nested pointers like T**)
        Value* heap_alloc_nested(IRTypePtr type, const std::string& name = "") {
            return heap_alloc(ir_types->get_pointer(type), name);
        }

        // Smart allocation - allocates on heap for pointer types (ref types), stack otherwise
        // For pointer types (T*), allocates T on heap and returns T*
        // For value types, allocates on stack
        Value* smart_alloc(IRTypePtr type, const std::string& name = "") {
            if (type->is_pointer() && type->pointee) {
                // Type is T* (reference type) - allocate T on heap
                return heap_alloc(type->pointee, name);
            }
            // Value type - allocate on stack
            return stack_alloc(type, name);
        }

        Value* load(Value* addr, IRTypePtr type, const std::string& name = "") {
            auto result = current_func->create_value(type, name);
            auto inst = std::make_unique<LoadInst>(result, addr);
            result->def = inst.get();
            addr->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            return result;
        }

        void store(Value* val, Value* addr) {
            auto inst = std::make_unique<StoreInst>(val, addr);
            val->uses.push_back(inst.get());
            addr->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
        }

        Value* binary(Opcode op, Value* left, Value* right) {
            // Determine result type based on operation
            IRTypePtr result_type;
            if (op == Opcode::Eq || op == Opcode::Ne ||
                op == Opcode::Lt || op == Opcode::Le ||
                op == Opcode::Gt || op == Opcode::Ge) {
                // Comparison operations return bool
                result_type = ir_types->get_bool();
            } else {
                // Arithmetic/logical operations return operand type
                result_type = left->type;
            }

            auto result = current_func->create_value(result_type);
            auto inst = std::make_unique<BinaryInst>(op, result, left, right);
            result->def = inst.get();
            left->uses.push_back(inst.get());
            right->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            return result;
        }

        Value* unary(Opcode op, Value* operand) {
            auto result = current_func->create_value(operand->type);
            auto inst = std::make_unique<UnaryInst>(op, result, operand);
            result->def = inst.get();
            operand->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            return result;
        }

        Value* cast(Value* value, IRTypePtr target_type) {
            auto result = current_func->create_value(target_type);
            auto inst = std::make_unique<CastInst>(result, value, target_type);
            result->def = inst.get();
            value->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            return result;
        }

        Value* field_addr(Value* object, uint32_t field_index, IRTypePtr field_type, const std::string& name = "") {
            // Result type is pointer to field type
            auto ptr_type = ir_types->get_pointer(field_type);
            auto result = current_func->create_value(ptr_type, name);
            auto inst = std::make_unique<FieldAddrInst>(result, object, field_index);
            result->def = inst.get();
            object->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            return result;
        }

        Value* element_addr(Value* array, Value* index, IRTypePtr element_type, const std::string& name = "") {
            // Result type is pointer to element type
            auto ptr_type = ir_types->get_pointer(element_type);
            auto result = current_func->create_value(ptr_type, name);
            auto inst = std::make_unique<ElementAddrInst>(result, array, index);
            result->def = inst.get();
            array->uses.push_back(inst.get());
            index->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            return result;
        }

        // Heap deallocation (lowered to free call)
        void heap_free(Value* ptr) {
            auto inst = std::make_unique<HeapFreeInst>(ptr);
            ptr->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
        }

        // Memory copy (lowered to llvm.memcpy intrinsic)
        void memcpy(Value* dest, Value* src, Value* size, bool is_volatile = false) {
            auto inst = std::make_unique<MemCpyInst>(dest, src, size, is_volatile);
            dest->uses.push_back(inst.get());
            src->uses.push_back(inst.get());
            size->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
        }

        // Memory set (lowered to llvm.memset intrinsic)
        void memset(Value* dest, Value* value, Value* size, bool is_volatile = false) {
            auto inst = std::make_unique<MemSetInst>(dest, value, size, is_volatile);
            dest->uses.push_back(inst.get());
            value->uses.push_back(inst.get());
            size->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
        }

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
            current_block->add_inst(std::move(inst));
            return result;
        }

        void ret(Value* val = nullptr) {
            auto inst = std::make_unique<RetInst>(val);
            if (val) val->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
        }

        void br(BasicBlock* target) {
            auto inst = std::make_unique<BrInst>(target);
            current_block->add_inst(std::move(inst));
            current_block->successors.push_back(target);
            target->predecessors.push_back(current_block);
        }

        void cond_br(Value* cond, BasicBlock* t, BasicBlock* f) {
            auto inst = std::make_unique<CondBrInst>(cond, t, f);
            cond->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            current_block->successors.push_back(t);
            current_block->successors.push_back(f);
            t->predecessors.push_back(current_block);
            f->predecessors.push_back(current_block);
        }

    };
}
