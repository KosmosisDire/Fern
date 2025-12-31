#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <memory>
#include <variant>
#include <unordered_map>
#include <set>
#include <sstream>
#include <iostream>
#include <iomanip>
#include "ir_type.hpp"
#include "semantic/symbol.hpp"
#include "magic_enum.hpp"

namespace Fern::FLIR
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
        IRTypePtr type;
        Instruction *def = nullptr;
        std::vector<Instruction *> uses;
        std::string debug_name;

        Value(uint32_t id, IRTypePtr type) : id(id), type(type) {}
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
        IRTypePtr null_type;

        ConstNullInst(Value *result, IRTypePtr type)
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
        IRTypePtr alloc_type;
        bool escapes = true;            // Pessimistic default
        std::set<Function *> escape_to; // Functions it escapes to

        StackAllocInst(Value *result, IRTypePtr type)
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
        IRTypePtr alloc_type;

        HeapAllocInst(Value *result, IRTypePtr type)
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
        IRTypePtr target_type;

        CastInst(Value *result, Value *val, IRTypePtr type)
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
        IRTypePtr return_type = nullptr;
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

        Value *create_value(IRTypePtr type, const std::string &name = "")
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

        std::string get_mangled_name() const
        {
            std::string mangled = symbol->get_qualified_name();

            // use symbol instead of params so lowered types are still differentiated
            for (auto param : symbol->parameters) {
                mangled += "_";
                if (param->type) {
                    mangled += param->type->get_name();
                }
            }
            return mangled;
        }

        std::string name() const
        {
            return symbol ? (is_external ? symbol->name : get_mangled_name()) : "<!null symbol!>";
        }

        bool has_valid_symbol() const { return symbol != nullptr; }

    private:
        FunctionSymbol *symbol = nullptr;

    };

#pragma region Type Definition


#pragma region Module

    struct Module
    {
        std::string name;
        std::vector<std::unique_ptr<Function>> functions;
        std::unordered_map<FunctionSymbol*, Function*> function_map;
        IRTypeSystem ir_types;

        Module(const std::string &name) : name(name) {}

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

        // Create a function shell (signature will be filled in by lowering)
        // Returns existing function if already created
        Function* create_function(FunctionSymbol *sym)
        {
            // Check if already exists
            auto it = function_map.find(sym);
            if (it != function_map.end()) {
                return it->second;
            }

            auto func = std::make_unique<Function>(sym);
            Function *ptr = func.get();
            functions.push_back(std::move(func));
            function_map[sym] = ptr;
            return ptr;
        }

        // Dump human-readable text representation
        std::string dump() const
        {
            std::stringstream ss;
            ss << "Module: " << name << "\n";
            ss << "===============================================\n\n";

            // Dump type definitions first
            for (const auto &struct_def : ir_types.get_all_structs())
            {
                ss << dump_type_definition(struct_def.get());
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
    
        static std::string dump_type_definition(const IRStruct *type_def)
        {
            std::stringstream ss;

            ss << "type " << type_def->name << "   ::   " << type_def->size << " bytes, align " << type_def->alignment;

            if (type_def->fields.empty())
            {
            ss << " { }\n";
            return ss.str();
            }

            ss << " \n{\n";

            // dump fields
            for (const auto &field : type_def->fields)
            {
            ss << "  ";
            if (field.type)
            {
                ss << field.type->get_name();
            }
            else
            {
                ss << "?";
            }

            ss << " " << field.name << "   ::   " << field.type->get_size() << " bytes, align " << field.type->get_alignment() << ", offset " << field.offset << "\n";
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
                ss << "\n" << dump_block(block.get());
            }

            ss << "}\n";
            return ss.str();
        }

        // Helper struct for table-formatted instruction output
        struct InstructionParts {
            std::string result;      // e.g., "%x" or ""
            std::string operation;   // e.g., "const.int 0" or "load %addr"
            std::string type;        // e.g., "i32" or ""
        };

        // Get just the operation string (without result prefix or type suffix)
        static std::string get_operation_string(const Instruction *inst)
        {
            std::stringstream ss;

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
                auto *memcpy_inst = static_cast<const MemCpyInst *>(inst);
                ss << "memcpy " << value_ref(memcpy_inst->dest) << ", " << value_ref(memcpy_inst->src) << ", " << value_ref(memcpy_inst->size);
                if (memcpy_inst->is_volatile) ss << " [volatile]";
                break;
            }
            case Opcode::MemSet:
            {
                auto *memset_inst = static_cast<const MemSetInst *>(inst);
                ss << "memset " << value_ref(memset_inst->dest) << ", " << value_ref(memset_inst->value) << ", " << value_ref(memset_inst->size);
                if (memset_inst->is_volatile) ss << " [volatile]";
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
                ss << to_string(inst->op) << " " << value_ref(bin->left) << ", " << value_ref(bin->right);
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
                ss << "call ";
                if (call->callee->is_external)
                {
                    ss << "external ";
                }
                ss << call->callee->name() << "(";
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

            return ss.str();
        }

        static InstructionParts get_instruction_parts(const Instruction *inst)
        {
            InstructionParts parts;

            // Result value
            if (inst->result)
            {
                parts.result = value_ref(inst->result);
            }

            // Operation (reuse existing logic but without result prefix)
            parts.operation = get_operation_string(inst);

            // Type annotation
            if (inst->result && inst->result->type)
            {
                parts.type = inst->result->type->get_name();
            }

            return parts;
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

            // First pass: collect parts and compute column widths
            std::vector<InstructionParts> all_parts;
            size_t max_result_width = 0;
            size_t max_op_width = 0;

            for (const auto &inst : block->instructions)
            {
                auto parts = get_instruction_parts(inst.get());
                max_result_width = std::max(max_result_width, parts.result.length());
                max_op_width = std::max(max_op_width, parts.operation.length());
                all_parts.push_back(std::move(parts));
            }

            // Second pass: format with alignment
            for (const auto &parts : all_parts)
            {
                ss << "    ";

                if (!parts.result.empty())
                {
                    // Left-align result in its column
                    ss << std::left << std::setw(max_result_width) << parts.result;
                    ss << " = ";
                }
                else
                {
                    // No result - pad to align with instructions that have results
                    if (max_result_width > 0)
                    {
                        ss << std::string(max_result_width + 3, ' '); // +3 for " = "
                    }
                }

                // Left-align operation
                ss << std::left << std::setw(max_op_width) << parts.operation;

                // Type annotation
                if (!parts.type.empty())
                {
                    ss << "  ::  " << parts.type;
                }

                ss << "\n";
            }

            return ss.str();
        }

        // Legacy single-line dump (still used for debug output in some places)
        static std::string dump_instruction(const Instruction *inst)
        {
            std::stringstream ss;

            if (inst->result)
            {
                ss << value_ref(inst->result) << " = ";
            }

            ss << get_operation_string(inst);

            if (inst->result && inst->result->type)
            {
                ss << "  ::  " << inst->result->type->get_name();
            }

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

} // namespace Fern::FLIR