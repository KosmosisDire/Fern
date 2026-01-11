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
    struct Function;
    struct Module;

    using ValuePtr = Value *;
    using InstPtr = std::unique_ptr<Instruction>;
    using InstructionList = std::vector<InstPtr>;

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

        // Allocation
        StackAlloc,
        HeapAlloc,
        HeapFree,
        StackAllocBytes,
        HeapAllocBytes,

        // Memory
        Load,
        Store,
        FieldAddr,
        ElementAddr,
        MemCpy,
        MemSet,

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
        Shl,
        Shr,
        Sar,

        // Conversion
        Convert,

        // Control Flow
        Block,
        Loop,
        If,
        // Match (Will implement once the rest of the match pipeline is set up)
        Br,
        BrIf,
        Continue,
        ContinueIf,

        // Calls
        Call,
        CallIndirect,
        Return,
    };

    inline std::string to_string(Opcode op)
    {
        switch (op)
        {
        case Opcode::ConstInt:       return "const.int";
        case Opcode::ConstFloat:     return "const.float";
        case Opcode::ConstBool:      return "const.bool";
        case Opcode::ConstNull:      return "const.null";
        case Opcode::ConstString:    return "const.string";
        case Opcode::StackAlloc:     return "stack.alloc";
        case Opcode::HeapAlloc:      return "heap.alloc";
        case Opcode::HeapFree:       return "heap.free";
        case Opcode::StackAllocBytes: return "stack.alloc.bytes";
        case Opcode::HeapAllocBytes: return "heap.alloc.bytes";
        case Opcode::Load:           return "load";
        case Opcode::Store:          return "store";
        case Opcode::FieldAddr:      return "fieldaddr";
        case Opcode::ElementAddr:    return "elementaddr";
        case Opcode::MemCpy:         return "memcpy";
        case Opcode::MemSet:         return "memset";
        case Opcode::Add:            return "add";
        case Opcode::Sub:            return "sub";
        case Opcode::Mul:            return "mul";
        case Opcode::Div:            return "div";
        case Opcode::Rem:            return "rem";
        case Opcode::Neg:            return "neg";
        case Opcode::Eq:             return "eq";
        case Opcode::Ne:             return "ne";
        case Opcode::Lt:             return "lt";
        case Opcode::Le:             return "le";
        case Opcode::Gt:             return "gt";
        case Opcode::Ge:             return "ge";
        case Opcode::And:            return "and";
        case Opcode::Or:             return "or";
        case Opcode::Not:            return "not";
        case Opcode::BitAnd:         return "bit_and";
        case Opcode::BitOr:          return "bit_or";
        case Opcode::BitXor:         return "bit_xor";
        case Opcode::BitNot:         return "bit_not";
        case Opcode::Shl:            return "shl";
        case Opcode::Shr:            return "shr";
        case Opcode::Sar:            return "sar";
        case Opcode::Convert:        return "convert";
        case Opcode::Block:          return "block";
        case Opcode::Loop:           return "loop";
        case Opcode::If:             return "if";
        case Opcode::Br:             return "br";
        case Opcode::BrIf:           return "br_if";
        case Opcode::Continue:       return "continue";
        case Opcode::ContinueIf:     return "continue_if";
        case Opcode::Call:           return "call";
        case Opcode::CallIndirect:   return "call_indirect";
        case Opcode::Return:         return "return";
        }
        return "unknown";
    }

#pragma region Base Inst

    struct Instruction
    {
        Opcode op;
        Value *result = nullptr;
        uint32_t debug_line = 0;

        virtual ~Instruction() = default;

        // Is this instruction a terminator for its containing region?
        virtual bool is_region_terminator() const { return false; }

        // Does this instruction contain nested instruction lists?
        virtual bool is_control_flow() const { return false; }
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

    struct StackAllocInst : Instruction
    {
        IRTypePtr alloc_type;
        bool escapes = true;
        std::set<Function *> escape_to;

        StackAllocInst(Value *result, IRTypePtr type)
        {
            op = Opcode::StackAlloc;
            this->result = result;
            this->alloc_type = type;
        }
    };

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

    struct HeapAllocBytesInst : Instruction
    {
        Value *size;

        HeapAllocBytesInst(Value *result, Value *size_val)
        {
            op = Opcode::HeapAllocBytes;
            this->result = result;
            this->size = size_val;
        }
    };

    struct HeapFreeInst : Instruction
    {
        Value *ptr;

        HeapFreeInst(Value *ptr_val)
        {
            op = Opcode::HeapFree;
            this->ptr = ptr_val;
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
        std::string field_name;
        IRTypePtr struct_type;
        IRTypePtr field_type;

        FieldAddrInst(Value *result, Value *obj, uint32_t idx,
                      const std::string &name = "",
                      IRTypePtr stype = nullptr,
                      IRTypePtr ftype = nullptr)
            : object(obj), field_index(idx), field_name(name),
              struct_type(stype), field_type(ftype)
        {
            op = Opcode::FieldAddr;
            this->result = result;
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

    struct MemCpyInst : Instruction
    {
        Value *dest;
        Value *src;
        Value *size;
        bool is_volatile = false;

        MemCpyInst(Value *dest_ptr, Value *src_ptr, Value *size_val, bool volatile_flag = false)
            : dest(dest_ptr), src(src_ptr), size(size_val), is_volatile(volatile_flag)
        {
            op = Opcode::MemCpy;
        }
    };

    struct MemSetInst : Instruction
    {
        Value *dest;
        Value *value;
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

#pragma region Conversion Inst

    struct ConvertInst : Instruction
    {
        Value *value;
        IRTypePtr target_type;

        ConvertInst(Value *result, Value *val, IRTypePtr type)
        {
            op = Opcode::Convert;
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

    struct CallIndirectInst : Instruction
    {
        Value *callee;
        IRTypePtr signature;
        std::vector<Value *> args;

        CallIndirectInst(Value *result, Value *func_ptr, IRTypePtr sig, std::vector<Value *> arguments)
            : callee(func_ptr), signature(sig), args(std::move(arguments))
        {
            op = Opcode::CallIndirect;
            this->result = result;
        }
    };

#pragma region Control Flow

    struct BlockInst : Instruction
    {
        InstructionList body;
        std::string label;

        BlockInst()
        {
            op = Opcode::Block;
        }

        bool is_control_flow() const override { return true; }
    };

    struct LoopInst : Instruction
    {
        InstructionList body;
        std::string label;

        LoopInst()
        {
            op = Opcode::Loop;
        }

        bool is_control_flow() const override { return true; }
    };

    struct IfInst : Instruction
    {
        Value *condition;
        InstructionList then_body;
        InstructionList else_body;

        IfInst(Value *cond) : condition(cond)
        {
            op = Opcode::If;
        }

        bool is_control_flow() const override { return true; }
    };

    struct BrInst : Instruction
    {
        uint32_t depth;

        BrInst(uint32_t depth = 0) : depth(depth)
        {
            op = Opcode::Br;
        }

        bool is_region_terminator() const override { return true; }
    };

    struct BrIfInst : Instruction
    {
        Value *condition;
        uint32_t depth;

        BrIfInst(Value *cond, uint32_t depth = 0)
            : condition(cond), depth(depth)
        {
            op = Opcode::BrIf;
        }
    };

    struct ContinueInst : Instruction
    {
        uint32_t depth;

        ContinueInst(uint32_t depth = 0) : depth(depth)
        {
            op = Opcode::Continue;
        }

        bool is_region_terminator() const override { return true; }
    };

    struct ContinueIfInst : Instruction
    {
        Value *condition;
        uint32_t depth;

        ContinueIfInst(Value *cond, uint32_t depth = 0)
            : condition(cond), depth(depth)
        {
            op = Opcode::ContinueIf;
        }
    };

    struct ReturnInst : Instruction
    {
        Value *value = nullptr;

        ReturnInst(Value *val = nullptr)
        {
            op = Opcode::Return;
            this->value = val;
        }

        bool is_region_terminator() const override { return true; }
    };

#pragma region Helpers

    inline bool definitely_terminates(const InstructionList &list)
    {
        for (const auto &inst : list)
        {
            if (inst->is_region_terminator())
                return true;

            if (inst->op == Opcode::If)
            {
                auto *if_inst = static_cast<IfInst *>(inst.get());
                if (!if_inst->else_body.empty() &&
                    definitely_terminates(if_inst->then_body) &&
                    definitely_terminates(if_inst->else_body))
                {
                    return true;
                }
            }

            if (inst->op == Opcode::Block)
            {
                auto *block = static_cast<BlockInst *>(inst.get());
                if (definitely_terminates(block->body))
                    return true;
            }
        }
        return false;
    }

#pragma region Function

    struct Function
    {
        IRTypePtr return_type = nullptr;
        std::vector<Value *> params;
        std::vector<bool> param_escapes;  // Which params escape
        std::vector<bool> param_modified; // Which params are modified

        // Flat instruction list with nested control structures
        InstructionList body;

        // Value storage
        std::vector<std::unique_ptr<Value>> values;
        uint32_t next_value_id = 0;

        bool is_external = false;
        bool is_static = false;

        // This pointer tracking (for methods)
        Value *this_param = nullptr;

        Function(FunctionSymbol *symbol) : symbol(symbol) {}

        bool is_empty() const
        {
            return body.empty();
        }

        Value *create_value(IRTypePtr type, const std::string &name = "")
        {
            auto val = std::make_unique<Value>(next_value_id++, type);
            val->debug_name = name;
            Value *ptr = val.get();
            values.push_back(std::move(val));
            return ptr;
        }

        std::string get_mangled_name() const
        {
            std::string mangled = symbol->get_qualified_name();

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

        FunctionSymbol *get_symbol() const { return symbol; }

    private:
        FunctionSymbol *symbol = nullptr;
    };

#pragma region Module

    struct Global
    {
        std::string name;
        IRTypePtr type;
        bool is_mutable = true;
    };

    struct Module
    {
        std::string name;
        std::vector<std::unique_ptr<Function>> functions;
        std::unordered_map<FunctionSymbol *, Function *> function_map;
        IRTypeSystem ir_types;

        std::vector<Global> globals;

        Module(const std::string &name) : name(name) {}

        Function *find_function(FunctionSymbol *sym)
        {
            auto it = function_map.find(sym);
            if (it != function_map.end())
            {
                return it->second;
            }
            std::cout << "Function not found: " << sym->get_qualified_name() << "\n";
            return nullptr;
        }

        Function *find_function_by_name(const std::string &name)
        {
            for (const auto &func : functions)
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
            auto it = function_map.find(sym);
            if (it != function_map.end())
            {
                return it->second;
            }

            auto func = std::make_unique<Function>(sym);
            Function *ptr = func.get();
            functions.push_back(std::move(func));
            function_map[sym] = ptr;
            return ptr;
        }

        std::string dump() const;

    private:
        static std::string dump_type_definition(const IRStruct *type_def);
        static std::string dump_function(const Function *func);
        static std::string dump_instruction_list(const InstructionList &list, int indent);
        static std::string dump_instruction(const Instruction *inst, int indent);
        static std::string value_ref(const Value *val);
    };

} // namespace Fern::FLIR
