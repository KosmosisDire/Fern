#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <memory>
#include <variant>
#include <unordered_map>
#include "semantic/type.hpp"
#include "semantic/symbol.hpp"
#include "semantic/type_system.hpp"
#include <set>
#include <sstream>
#include "magic_enum.hpp"
#include <iostream>

namespace Fern::HLIR
{
    struct Value;
    struct Instruction;
    struct BasicBlock;
    struct Function;
    struct Module;

    using ValuePtr = Value *;
    using InstPtr = Instruction *;
    using BlockPtr = BasicBlock *;

#pragma region SSA Value

    struct Value
    {
        uint32_t id;
        TypePtr type;
        Instruction *def = nullptr;
        std::vector<Instruction *> uses;
        std::string debug_name;

        Value(uint32_t id, TypePtr type) : id(id), type(type) {}
    };

#pragma region Inst Opcodes

    enum class Opcode
    {
        // Constants
        ConstInt,
        ConstFloat,
        ConstBool,
        ConstNull,
        ConstString,

        // Memory
        StackAlloc,       // Stack allocation of typed value
        StackAllocBytes,  // Stack allocation of dynamic byte count
        HeapAlloc,        // Heap allocation of typed value
        HeapAllocBytes,   // Heap allocation of dynamic byte count
        HeapFree,         // Heap deallocation (free)
        MemCpy,         // Memory copy (llvm.memcpy)
        MemSet,         // Memory set (llvm.memset)
        Load,
        Store,
        FieldAddr,
        ElementAddr,

        // Arithmetic
        Add,
        Sub,
        Mul,
        Div,
        Rem,
        Neg,

        // Comparison
        Eq,
        Ne,
        Lt,
        Le,
        Gt,
        Ge,

        // Logical
        And,
        Or,
        Not,

        // Bitwise
        BitAnd,
        BitOr,
        BitXor,
        BitNot,
        ShiftL,
        ShiftR,

        // Conversion
        Cast,
        Bitcast,

        // Control flow
        Call,
        Ret,
        Br,
        CondBr,
        Switch
    };

    static std::string to_string(Opcode op)
    {
        return std::string(magic_enum::enum_name(op));
    }

#pragma region Base Inst

    struct Instruction
    {
        Opcode op;
        Value *result = nullptr;
        BasicBlock *parent = nullptr;
        uint32_t debug_line = 0;

        virtual ~Instruction() = default;
    };

#pragma region Constant Inst

    struct ConstIntInst : Instruction
    {
        int64_t value;

        ConstIntInst(Value *result, int64_t val)
        {
            op = Opcode::ConstInt;
            this->result = result;
            this->value = val;
        }
    };

    struct ConstFloatInst : Instruction
    {
        double value;

        ConstFloatInst(Value *result, double val)
        {
            op = Opcode::ConstFloat;
            this->result = result;
            this->value = val;
        }
    };

    struct ConstBoolInst : Instruction
    {
        bool value;

        ConstBoolInst(Value *result, bool val)
        {
            op = Opcode::ConstBool;
            this->result = result;
            this->value = val;
        }
    };

    struct ConstStringInst : Instruction
    {
        std::string value;

        ConstStringInst(Value *result, const std::string &val)
        {
            op = Opcode::ConstString;
            this->result = result;
            this->value = val;
        }
    };

    struct ConstNullInst : Instruction
    {
        TypePtr null_type;

        ConstNullInst(Value *result, TypePtr type)
        {
            op = Opcode::ConstNull;
            this->result = result;
            this->null_type = type;
        }
    };

#pragma region Memory Inst

    // Stack allocation of a typed value (lowered to alloca)
    struct StackAllocInst : Instruction
    {
        TypePtr alloc_type;
        bool escapes = true;            // Pessimistic default
        std::set<Function *> escape_to; // Functions it escapes to

        StackAllocInst(Value *result, TypePtr type)
        {
            op = Opcode::StackAlloc;
            this->result = result;
            this->alloc_type = type;
        }
    };

    // Stack allocation of dynamic byte count (lowered to alloca with size)
    struct StackAllocBytesInst : Instruction
    {
        Value *size;

        StackAllocBytesInst(Value *result, Value *size_val)
        {
            op = Opcode::StackAllocBytes;
            this->result = result;
            this->size = size_val;
        }
    };

    struct LoadInst : Instruction
    {
        Value *address;

        LoadInst(Value *result, Value *addr)
        {
            op = Opcode::Load;
            this->result = result;
            this->address = addr;
        }
    };

    struct StoreInst : Instruction
    {
        Value *value;
        Value *address;

        StoreInst(Value *val, Value *addr)
        {
            op = Opcode::Store;
            this->value = val;
            this->address = addr;
        }
    };

    struct FieldAddrInst : Instruction
    {
        Value *object;
        uint32_t field_index;

        FieldAddrInst(Value *result, Value *obj, uint32_t idx)
        {
            op = Opcode::FieldAddr;
            this->result = result;
            this->object = obj;
            this->field_index = idx;
        }
    };

    struct ElementAddrInst : Instruction
    {
        Value *array;
        Value *index;

        ElementAddrInst(Value *result, Value *arr, Value *idx)
        {
            op = Opcode::ElementAddr;
            this->result = result;
            this->array = arr;
            this->index = idx;
        }
    };

    // Heap allocation of a typed value (lowered to malloc with computed size)
    struct HeapAllocInst : Instruction
    {
        TypePtr alloc_type;

        HeapAllocInst(Value *result, TypePtr type)
        {
            op = Opcode::HeapAlloc;
            this->result = result;
            this->alloc_type = type;
        }
    };

    // Heap allocation of dynamic byte count (lowered to malloc call)
    struct HeapAllocBytesInst : Instruction
    {
        Value *size;  // Size in bytes

        HeapAllocBytesInst(Value *result, Value *size_val)
        {
            op = Opcode::HeapAllocBytes;
            this->result = result;
            this->size = size_val;
        }
    };

    // Heap deallocation (lowered to free call)
    struct HeapFreeInst : Instruction
    {
        Value *ptr;

        HeapFreeInst(Value *ptr_val)
        {
            op = Opcode::HeapFree;
            this->ptr = ptr_val;
        }
    };

    // Memory copy (lowered to llvm.memcpy intrinsic)
    struct MemCpyInst : Instruction
    {
        Value *dest;
        Value *src;
        Value *size;
        bool is_volatile = false;

        MemCpyInst(Value *dest_val, Value *src_val, Value *size_val, bool volatile_flag = false)
        {
            op = Opcode::MemCpy;
            this->dest = dest_val;
            this->src = src_val;
            this->size = size_val;
            this->is_volatile = volatile_flag;
        }
    };

    // Memory set (lowered to llvm.memset intrinsic)
    struct MemSetInst : Instruction
    {
        Value *dest;
        Value *value;  // Byte value to set (i8)
        Value *size;
        bool is_volatile = false;

        MemSetInst(Value *dest_val, Value *byte_val, Value *size_val, bool volatile_flag = false)
        {
            op = Opcode::MemSet;
            this->dest = dest_val;
            this->value = byte_val;
            this->size = size_val;
            this->is_volatile = volatile_flag;
        }
    };

#pragma region Binary Inst

    struct BinaryInst : Instruction
    {
        Value *left;
        Value *right;

        BinaryInst(Opcode op, Value *result, Value *l, Value *r)
        {
            this->op = op;
            this->result = result;
            this->left = l;
            this->right = r;
        }
    };

#pragma region Unary Inst

    struct UnaryInst : Instruction
    {
        Value *operand;

        UnaryInst(Opcode op, Value *result, Value *oper)
        {
            this->op = op;
            this->result = result;
            this->operand = oper;
        }
    };

#pragma region Cast Inst

    struct CastInst : Instruction
    {
        Value *value;
        TypePtr target_type;

        CastInst(Value *result, Value *val, TypePtr type)
        {
            op = Opcode::Cast;
            this->result = result;
            this->value = val;
            this->target_type = type;
        }
    };

#pragma region Call Inst

    struct CallInst : Instruction
    {
        Function *callee;
        std::vector<Value *> args;

        CallInst(Value *result, Function *func, std::vector<Value *> arguments)
        {
            op = Opcode::Call;
            this->result = result;
            this->callee = func;
            this->args = std::move(arguments);
        }
    };

#pragma region Control Flow Inst

    struct RetInst : Instruction
    {
        Value *value = nullptr;

        RetInst(Value *val = nullptr)
        {
            op = Opcode::Ret;
            this->value = val;
        }
    };

    struct BrInst : Instruction
    {
        BasicBlock *target;

        BrInst(BasicBlock *dest)
        {
            op = Opcode::Br;
            this->target = dest;
        }
    };

    struct CondBrInst : Instruction
    {
        Value *condition;
        BasicBlock *true_block;
        BasicBlock *false_block;

        CondBrInst(Value *cond, BasicBlock *t, BasicBlock *f)
        {
            op = Opcode::CondBr;
            this->condition = cond;
            this->true_block = t;
            this->false_block = f;
        }
    };


#pragma region Basic Block

    struct BasicBlock
    {
        uint32_t id;
        std::string name;
        Function *parent;

        std::vector<std::unique_ptr<Instruction>> instructions;
        std::vector<BasicBlock *> predecessors;
        std::vector<BasicBlock *> successors;

        BasicBlock(uint32_t id, const std::string &name = "")
            : id(id), name(name) {}

        void add_inst(std::unique_ptr<Instruction> inst)
        {
            inst->parent = this;
            instructions.push_back(std::move(inst));
        }

        Instruction *terminator() const
        {
            if (instructions.empty())
                return nullptr;

            auto *last = instructions.back().get();
            // Check if the last instruction is actually a terminator
            if (last->op == Opcode::Ret ||
                last->op == Opcode::Br ||
                last->op == Opcode::CondBr ||
                last->op == Opcode::Switch)
            {
                return last;
            }

            return nullptr;
        }
    };

#pragma region Function

    struct Function
    {
        TypePtr return_type;
        std::vector<Value *> params;
        std::vector<bool> param_escapes;  // Which params escape
        std::vector<bool> param_modified; // Which params are modified

        std::vector<std::unique_ptr<BasicBlock>> blocks;
        std::vector<std::unique_ptr<Value>> values;
        BasicBlock *entry = nullptr;

        uint32_t next_value_id = 0;
        uint32_t next_block_id = 0;

        bool is_external = false;
        bool is_static = false; // if not then we need a this pointer as first arg

        Function(FunctionSymbol *symbol) : symbol(symbol){}

        bool is_empty() const
        {
            return blocks.empty();
        }

        Value *create_value(TypePtr type, const std::string &name = "")
        {
            auto val = std::make_unique<Value>(next_value_id++, type);
            val->debug_name = name;
            Value *ptr = val.get();
            values.push_back(std::move(val));
            return ptr;
        }

        BasicBlock *create_block(const std::string &name = "")
        {
            auto block = std::make_unique<BasicBlock>(next_block_id++, name);
            block->parent = this;
            BasicBlock *ptr = block.get();
            blocks.push_back(std::move(block));
            return ptr;
        }

        std::string name() const
        {
            return symbol ? (is_external ? symbol->name : symbol->get_qualified_name()) : "<!null symbol!>";
        }

    private:
        FunctionSymbol *symbol = nullptr;

    };

#pragma region Type Definition

    struct Field
    {
        public:
        TypePtr type;
        uint32_t offset = 0;
        uint32_t alignment = 1;

        Field(VariableSymbol *symbol)
            : symbol(symbol) {}

        std::string qualified_name() const
        {
            return symbol ? symbol->get_qualified_name() : "<!null symbol!>";
        }

        std::string name() const
        {
            return symbol ? symbol->name : "<!null symbol!>";
        }

        private:
        VariableSymbol *symbol;
    };

    struct TypeDefinition
    {
        std::vector<std::unique_ptr<Field>> fields;
        std::vector<Function*> functions;
        TypeDefinition *base_type = nullptr;
        
        TypeDefinition(TypeSymbol *symbol)
            : symbol(symbol)
        {
        }

        bool is_empty() const
        {
            return symbol->member_order.empty();
        }

        std::string name() const
        {
            return symbol ? symbol->get_qualified_name() : "<!null symbol!>";
        }

        TypePtr type() const
        {
            return symbol ? symbol->type : nullptr;
        }

    private:
        TypeSymbol *symbol;
    };

#pragma region Module

    struct Module
    {
        std::string name;
        std::vector<std::unique_ptr<Function>> functions;
        std::unordered_map<FunctionSymbol*, Function*> function_map;
        std::vector<std::unique_ptr<TypeDefinition>> types;
        std::unordered_map<TypeSymbol*, TypeDefinition*> type_map;
        TypeSystem *type_system;

        Module(const std::string &name, NamespaceSymbol *global_ns, TypeSystem *type_system)
            : name(name), type_system(type_system)
        {
            // Recursively define all types and functions in the global namespace
            for (const auto &member : global_ns->member_order)
            {
                if (auto type_sym = member->as<TypeSymbol>())
                {
                    define_type(type_sym);
                }
                else if (auto func_sym = member->as<FunctionSymbol>())
                {
                    if (func_sym->is_intrinsic) {
                        continue; // Skip intrinsic functions
                    }
                    create_function(func_sym);
                }
            }
        }

        // Lookup function by symbol
        Function* find_function(FunctionSymbol* sym)
        {
            auto it = function_map.find(sym);
            if (it != function_map.end())
            {
                return it->second;
            }
            std::cout << "Function not found: " << sym->get_qualified_name() << "\n";
            return nullptr;
        }
 
        Function* find_function_by_name(const std::string& name)
        {
            for (const auto& func : functions)
            {
                if (func->name() == name)
                {
                    return func.get();
                }
            }
            return nullptr;
        }

        Function *create_function(FunctionSymbol *sym)
        {
            auto func = std::make_unique<Function>(sym);

            func->return_type = sym->return_type->lower_references_to_ptrs(type_system);

            Function *ptr = func.get();
            functions.push_back(std::move(func));
            function_map[sym] = ptr; 
            return ptr;
        }
        
        TypeDefinition *define_type(TypeSymbol *sym)
        {
            auto def = std::make_unique<TypeDefinition>(sym);

            // define all the members as well
            for (const auto &member : sym->member_order) {
                if (auto func_sym = member->as<FunctionSymbol>()) {
                    create_function(func_sym);
                }
                else if (auto type_sym = member->as<TypeSymbol>()) {
                    define_type(type_sym);
                }
                else if (auto prop_sym = member->as<PropertySymbol>()) {
                    // Create functions for property getter/setter
                    for (const auto &prop_member : prop_sym->member_order) {
                        if (auto prop_func_sym = prop_member->as<FunctionSymbol>()) {
                            create_function(prop_func_sym);
                        }
                    }
                }
                else if (auto var_sym = member->as<VariableSymbol>())
                {
                    // Fields are handled within the TypeDefinition itself
                    auto field = std::make_unique<Field>(var_sym);
                    field->type = var_sym->type->lower_references_to_ptrs(type_system);
                    def->fields.push_back(std::move(field));
                }
            }
            
            TypeDefinition *ptr = def.get();
            types.push_back(std::move(def));
            type_map[sym] = ptr;
            return ptr;
        }

        // Dump human-readable text representation
        std::string dump() const
        {
            std::stringstream ss;
            ss << "Module: " << name << "\n";
            ss << "===============================================\n\n";

            // Dump type definitions first
            for (const auto &type_def : types)
            {
                ss << dump_type_definition(type_def.get());
                ss << "\n";
            }

            for (const auto &func : functions)
            {
                ss << dump_function(func.get());
                ss << "\n";
            }

            return ss.str();
        }

#pragma region Dump Functions

    private:
    
        static std::string dump_type_definition(const TypeDefinition *type_def)
        {
            std::stringstream ss;

            ss << "type " << type_def->name();

            if (type_def->base_type)
            {
                ss << " extends " << type_def->base_type->name();
            }

            if (type_def->is_empty())
            {
                ss << " { }\n";
                return ss.str();
            }

            ss << " \n{\n";

            // dump fields
            for (const auto &field : type_def->fields)
            {
                ss << "  ";
                if (field->type)
                {
                    ss << field->type->get_name();
                }
                else
                {
                    ss << "?";
                }

                ss << " " << field->name() << "\n";
            }

            ss << "}\n";
            return ss.str();
        }

        static std::string dump_function(const Function *func)
        {
            std::stringstream ss;

            if (func->is_external)
            {
                ss << "extern ";
            }

            // Function signature
            ss << "fn " << func->name() << "(";
            for (size_t i = 0; i < func->params.size(); ++i)
            {
                if (i > 0)
                    ss << ", ";
                ss << value_ref(func->params[i]) << ": " << func->params[i]->type->get_name();
            }
            ss << ") -> " << func->return_type->get_name();

            if (func->is_external)
            {
                return ss.str();
            }

            if (func->is_empty())
            {
                ss << " { }\n";
                return ss.str();
            }

            ss << "\n{\n";

            // Dump all blocks
            for (const auto &block : func->blocks)
            {
                ss << dump_block(block.get());
            }

            ss << "}\n";
            return ss.str();
        }

        static std::string dump_block(const BasicBlock *block)
        {
            std::stringstream ss;

            // Block label
            ss << "  bb" << block->id;
            if (!block->name.empty())
            {
                ss << " <" << block->name << ">";
            }

            // Show predecessors if any
            if (!block->predecessors.empty())
            {
                ss << "  ; preds: ";
                for (size_t i = 0; i < block->predecessors.size(); ++i)
                {
                    if (i > 0)
                        ss << ", ";
                    ss << "bb" << block->predecessors[i]->id;
                }
            }
            ss << ":\n";

            // Dump instructions
            for (const auto &inst : block->instructions)
            {
                ss << "    " << dump_instruction(inst.get()) << "\n";
            }

            return ss.str();
        }

        static std::string dump_instruction(const Instruction *inst)
        {
            std::stringstream ss;

            // Result value if any
            if (inst->result)
            {
                ss << value_ref(inst->result) << " = ";
            }

            // Opcode and operands
            switch (inst->op)
            {
            case Opcode::ConstInt:
            {
                auto *ci = static_cast<const ConstIntInst *>(inst);
                ss << "const.int " << ci->value;
                break;
            }
            case Opcode::ConstFloat:
            {
                auto *cf = static_cast<const ConstFloatInst *>(inst);
                ss << "const.float " << cf->value;
                break;
            }
            case Opcode::ConstBool:
            {
                auto *cb = static_cast<const ConstBoolInst *>(inst);
                ss << "const.bool " << (cb->value ? "true" : "false");
                break;
            }
            case Opcode::ConstString:
            {
                auto *cs = static_cast<const ConstStringInst *>(inst);
                ss << "const.string \"" << cs->value << "\"";
                break;
            }
            case Opcode::ConstNull:
            {
                auto *cn = static_cast<const ConstNullInst *>(inst);
                ss << "const.null " << cn->null_type->get_name();
                break;
            }
            case Opcode::StackAlloc:
            {
                auto *alloc = static_cast<const StackAllocInst *>(inst);
                ss << "stack.alloc " << alloc->alloc_type->get_name();
                if (!alloc->escapes)
                    ss << " [no-escape]";
                break;
            }
            case Opcode::StackAllocBytes:
            {
                auto *alloc = static_cast<const StackAllocBytesInst *>(inst);
                ss << "stack.alloc.bytes " << value_ref(alloc->size);
                break;
            }
            case Opcode::Load:
            {
                auto *load = static_cast<const LoadInst *>(inst);
                ss << "load " << value_ref(load->address);
                break;
            }
            case Opcode::Store:
            {
                auto *store = static_cast<const StoreInst *>(inst);
                ss << "store " << value_ref(store->value) << ", " << value_ref(store->address);
                break;
            }
            case Opcode::FieldAddr:
            {
                auto *field = static_cast<const FieldAddrInst *>(inst);
                ss << "fieldaddr " << value_ref(field->object) << ", " << field->field_index;
                break;
            }
            case Opcode::ElementAddr:
            {
                auto *elem = static_cast<const ElementAddrInst *>(inst);
                ss << "elementaddr " << value_ref(elem->array) << ", " << value_ref(elem->index);
                break;
            }
            case Opcode::HeapAlloc:
            {
                auto *alloc = static_cast<const HeapAllocInst *>(inst);
                ss << "heap.alloc " << alloc->alloc_type->get_name();
                break;
            }
            case Opcode::HeapAllocBytes:
            {
                auto *alloc = static_cast<const HeapAllocBytesInst *>(inst);
                ss << "heap.alloc.bytes " << value_ref(alloc->size);
                break;
            }
            case Opcode::HeapFree:
            {
                auto *free_inst = static_cast<const HeapFreeInst *>(inst);
                ss << "heap.free " << value_ref(free_inst->ptr);
                break;
            }
            case Opcode::MemCpy:
            {
                auto *memcpy = static_cast<const MemCpyInst *>(inst);
                ss << "memcpy " << value_ref(memcpy->dest) << ", " << value_ref(memcpy->src) << ", " << value_ref(memcpy->size);
                if (memcpy->is_volatile) ss << " [volatile]";
                break;
            }
            case Opcode::MemSet:
            {
                auto *memset = static_cast<const MemSetInst *>(inst);
                ss << "memset " << value_ref(memset->dest) << ", " << value_ref(memset->value) << ", " << value_ref(memset->size);
                if (memset->is_volatile) ss << " [volatile]";
                break;
            }
            case Opcode::Add:
            case Opcode::Sub:
            case Opcode::Mul:
            case Opcode::Div:
            case Opcode::Rem:
            case Opcode::Eq:
            case Opcode::Ne:
            case Opcode::Lt:
            case Opcode::Le:
            case Opcode::Gt:
            case Opcode::Ge:
            case Opcode::And:
            case Opcode::Or:
            case Opcode::BitAnd:
            case Opcode::BitOr:
            case Opcode::BitXor:
            case Opcode::ShiftL:
            case Opcode::ShiftR:
            {
                auto *bin = static_cast<const BinaryInst *>(inst);
                ss << to_string(inst->op) << " "
                   << value_ref(bin->left) << ", " << value_ref(bin->right);
                break;
            }
            case Opcode::Neg:
            case Opcode::Not:
            case Opcode::BitNot:
            {
                auto *un = static_cast<const UnaryInst *>(inst);
                ss << to_string(inst->op) << " " << value_ref(un->operand);
                break;
            }
            case Opcode::Cast:
            {
                auto *cast = static_cast<const CastInst *>(inst);
                ss << "cast " << value_ref(cast->value) << " to " << cast->target_type->get_name();
                break;
            }
            case Opcode::Call:
            {
                auto *call = static_cast<const CallInst *>(inst);
                ss << "call @" << call->callee->name() << "(";
                for (size_t i = 0; i < call->args.size(); ++i)
                {
                    if (i > 0)
                        ss << ", ";
                    ss << value_ref(call->args[i]);
                }
                ss << ")";
                break;
            }
            case Opcode::Ret:
            {
                auto *ret = static_cast<const RetInst *>(inst);
                ss << "ret";
                if (ret->value)
                {
                    ss << " " << value_ref(ret->value);
                }
                break;
            }
            case Opcode::Br:
            {
                auto *br = static_cast<const BrInst *>(inst);
                ss << "br bb" << br->target->id;
                break;
            }
            case Opcode::CondBr:
            {
                auto *cbr = static_cast<const CondBrInst *>(inst);
                ss << "condbr " << value_ref(cbr->condition)
                   << ", bb" << cbr->true_block->id
                   << ", bb" << cbr->false_block->id;
                break;
            }
            default:
                ss << "unknown_op_" << static_cast<int>(inst->op);
            }

            // Debug info
            if (inst->debug_line > 0)
            {
                ss << "  ; line " << inst->debug_line;
            }

            return ss.str();
        }

        static std::string value_ref(const Value *val)
        {
            if (!val)
                return "<null>";
            std::stringstream ss;
            // Use name as primary identifier if available, otherwise use numeric ID
            if (!val->debug_name.empty())
            {
                ss << "%" << val->debug_name;
            }
            else
            {
                ss << "%" << val->id;
            }
            return ss.str();
        }

        
    };

} // namespace Fern::HLIR