

#include "hlir.hpp"
#include "../semantic/type_system.hpp"

namespace Fern::HLIR
{
    class HLIRBuilder {
        Function* current_func = nullptr;
        BasicBlock* current_block = nullptr;
        TypeSystem* type_system = nullptr;

    public:
        HLIRBuilder() = default;
        HLIRBuilder(TypeSystem* ts) : type_system(ts) {}

        void set_function(Function* f) { current_func = f; }
        void set_block(BasicBlock* b) { current_block = b; }
        void set_type_system(TypeSystem* ts) { type_system = ts; }
        
        Value* const_int(int64_t val, TypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstIntInst>(result, val);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }
        
        Value* const_bool(bool val, TypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstBoolInst>(result, val);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }
        
        Value* const_float(double val, TypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstFloatInst>(result, val);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }
        
        Value* const_string(const std::string& val, TypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstStringInst>(result, val);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }
        
        Value* const_null(TypePtr type) {
            // Create a proper ConstNull instruction for any type
            // The backend will lower this appropriately for each type
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<ConstNullInst>(result, type);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }
        
        Value* alloc(TypePtr type, bool stack = false) {
            // Result type is pointer to the allocated type
            auto ptr_type = type_system ? type_system->get_pointer(type) : type;
            auto result = current_func->create_value(ptr_type);
            auto inst = std::make_unique<AllocInst>(result, type);
            inst->on_stack = stack;
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }

        Value* alloc_nested(TypePtr type, bool stack = false) {
            // Result type is pointer to a pointer to the allocated type
            return alloc(type_system->get_pointer(type), stack);
        }
        
        Value* load(Value* addr, TypePtr type) {
            auto result = current_func->create_value(type);
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
            TypePtr result_type;
            if (op == Opcode::Eq || op == Opcode::Ne ||
                op == Opcode::Lt || op == Opcode::Le ||
                op == Opcode::Gt || op == Opcode::Ge) {
                // Comparison operations return bool
                result_type = type_system->get_bool();
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
        
        Value* cast(Value* value, TypePtr target_type) {
            auto result = current_func->create_value(target_type);
            auto inst = std::make_unique<CastInst>(result, value, target_type);
            result->def = inst.get();
            value->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            return result;
        }
        
        Value* field_addr(Value* object, uint32_t field_index, TypePtr field_type) {
            // Result type is pointer to field type
            auto ptr_type = type_system ? type_system->get_pointer(field_type) : field_type;
            auto result = current_func->create_value(ptr_type);
            auto inst = std::make_unique<FieldAddrInst>(result, object, field_index);
            result->def = inst.get();
            object->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            return result;
        }

        Value* element_addr(Value* array, Value* index, TypePtr element_type) {
            // Result type is pointer to element type
            auto ptr_type = type_system ? type_system->get_pointer(element_type) : element_type;
            auto result = current_func->create_value(ptr_type);
            auto inst = std::make_unique<ElementAddrInst>(result, array, index);
            result->def = inst.get();
            array->uses.push_back(inst.get());
            index->uses.push_back(inst.get());
            current_block->add_inst(std::move(inst));
            return result;
        }
        
        Value* call(Function* func, std::vector<Value*> args) {
            Value* result = nullptr;
            if (func->return_type() && !func->return_type()->is_void()) {
                result = current_func->create_value(func->return_type());
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
        
        Value* phi(TypePtr type) {
            auto result = current_func->create_value(type);
            auto inst = std::make_unique<PhiInst>(result);
            result->def = inst.get();
            current_block->add_inst(std::move(inst));
            return result;
        }
    };
}