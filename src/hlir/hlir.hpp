#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <memory>
#include <variant>
#include <unordered_map>
#include "semantic/type.hpp"
#include "semantic/symbol.hpp"
#include <set>
#include <sstream>

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
        Alloc,
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
        Shl,
        Shr,

        // Conversion
        Cast,
        Bitcast,

        // Control flow
        Call,
        Ret,
        Br,
        CondBr,
        Switch,
        Phi,

        // Misc
        Copy,
    };

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

    struct AllocInst : Instruction
    {
        TypePtr alloc_type;
        bool on_stack = false;
        bool escapes = true;            // Pessimistic default
        std::set<Function *> escape_to; // Functions it escapes to

        AllocInst(Value *result, TypePtr type)
        {
            op = Opcode::Alloc;
            this->result = result;
            this->alloc_type = type;
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

    struct PhiInst : Instruction
    {
        std::vector<std::pair<Value *, BasicBlock *>> incoming;

        PhiInst(Value *result)
        {
            op = Opcode::Phi;
            this->result = result;
        }

        void add_incoming(Value *val, BasicBlock *block)
        {
            incoming.push_back({val, block});
            if (val) {
                val->uses.push_back(this);
            }
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
        FunctionSymbol *symbol = nullptr;
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
            return symbol ? symbol->get_qualified_name() : "<!null symbol!>";
        }

        TypePtr return_type() const
        {
            return symbol ? symbol->return_type : nullptr;
        }
    };

#pragma region Type Definition

    struct TypeDefinition
    {
        TypeSymbol *symbol;
        TypeDefinition *base_type = nullptr;
        std::vector<Function *> vtable;
    };

#pragma region Module

    struct Module
    {
        std::string name;
        std::vector<std::unique_ptr<Function>> functions;
        std::vector<std::unique_ptr<TypeDefinition>> types;

        Module(const std::string &name, NamespaceSymbol *global_ns)
        {
            this->name = name;

            // Recursively define all types and functions in the global namespace
            for (const auto &member : global_ns->member_order)
            {
                if (auto type_sym = member->as<TypeSymbol>())
                {
                    define_type(type_sym);
                }
                else if (auto func_sym = member->as<FunctionSymbol>())
                {
                    create_function(func_sym);
                }
            }
        }
        
        // Lookup function by symbol
        Function* find_function(FunctionSymbol* sym)
        {
            for (const auto& func : functions)
            {
                if (func->symbol == sym)
                {
                    return func.get();
                }
            }
            return nullptr;
        }
        
        // Lookup type definition by symbol
        TypeDefinition* find_type(TypeSymbol* sym)
        {
            for (const auto& type : types)
            {
                if (type->symbol == sym)
                {
                    return type.get();
                }
            }
            return nullptr;
        }

        Function *create_function(FunctionSymbol *sym)
        {
            auto func = std::make_unique<Function>();
            func->symbol = sym;
            Function *ptr = func.get();
            functions.push_back(std::move(func));
            return ptr;
        }
        
        TypeDefinition *define_type(TypeSymbol *sym)
        {
            auto def = std::make_unique<TypeDefinition>();
            def->symbol = sym;
            TypeDefinition *ptr = def.get();
            types.push_back(std::move(def));

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
            }

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

            ss << "type @" << type_def->symbol->get_qualified_name();

            if (type_def->base_type)
            {
                ss << " extends @" << type_def->base_type->symbol->get_qualified_name();
            }

            ss << " {\n";

            // Dump vtable if present
            if (!type_def->vtable.empty())
            {
                ss << "  vtable:\n";
                for (size_t i = 0; i < type_def->vtable.size(); ++i)
                {
                    ss << "    [" << i << "] @" << type_def->vtable[i]->name() << "\n";
                }
            }

            // dump member variables from symbol
            for (const auto &member : type_def->symbol->member_order)
            {
                if (auto var_sym = member->as<VariableSymbol>())
                {
                    ss << "  " << var_sym->name << ": ";
                    if (var_sym->type)
                    {
                        ss << var_sym->type->get_name();
                    }
                    else
                    {
                        ss << "?";
                    }
                    ss << ";\n";
                }
            }

            ss << "}\n";
            return ss.str();
        }

        static std::string dump_function(const Function *func)
        {
            std::stringstream ss;

            // Function signature
            ss << "function @" << func->name() << "(";
            for (size_t i = 0; i < func->params.size(); ++i)
            {
                if (i > 0)
                    ss << ", ";
                ss << value_ref(func->params[i]) << ": " << type_to_string(func->params[i]->type);
            }
            ss << ") -> " << type_to_string(func->return_type());

            if (func->is_external)
            {
                ss << " [external]\n";
                return ss.str();
            }

            ss << " {\n";

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
                ss << "const.null " << type_to_string(cn->null_type);
                break;
            }
            case Opcode::Alloc:
            {
                auto *alloc = static_cast<const AllocInst *>(inst);
                ss << "alloc " << type_to_string(alloc->alloc_type);
                if (alloc->on_stack)
                    ss << " [stack]";
                if (!alloc->escapes)
                    ss << " [no-escape]";
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
            case Opcode::Shl:
            case Opcode::Shr:
            {
                auto *bin = static_cast<const BinaryInst *>(inst);
                ss << opcode_to_string(inst->op) << " "
                   << value_ref(bin->left) << ", " << value_ref(bin->right);
                break;
            }
            case Opcode::Neg:
            case Opcode::Not:
            case Opcode::BitNot:
            {
                auto *un = static_cast<const UnaryInst *>(inst);
                ss << opcode_to_string(inst->op) << " " << value_ref(un->operand);
                break;
            }
            case Opcode::Cast:
            {
                auto *cast = static_cast<const CastInst *>(inst);
                ss << "cast " << value_ref(cast->value) << " to " << type_to_string(cast->target_type);
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
            case Opcode::Phi:
            {
                auto *phi = static_cast<const PhiInst *>(inst);
                ss << "phi ";
                for (size_t i = 0; i < phi->incoming.size(); ++i)
                {
                    if (i > 0)
                        ss << ", ";
                    ss << "[" << value_ref(phi->incoming[i].first)
                       << ", bb" << phi->incoming[i].second->id << "]";
                }
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

        static std::string type_to_string(TypePtr type)
        {
            if (!type)
                return "void";
            return type->get_name();
        }

        static std::string opcode_to_string(Opcode op)
        {
            switch (op)
            {
            case Opcode::Add:
                return "add";
            case Opcode::Sub:
                return "sub";
            case Opcode::Mul:
                return "mul";
            case Opcode::Div:
                return "div";
            case Opcode::Rem:
                return "rem";
            case Opcode::Neg:
                return "neg";
            case Opcode::Eq:
                return "eq";
            case Opcode::Ne:
                return "ne";
            case Opcode::Lt:
                return "lt";
            case Opcode::Le:
                return "le";
            case Opcode::Gt:
                return "gt";
            case Opcode::Ge:
                return "ge";
            case Opcode::And:
                return "and";
            case Opcode::Or:
                return "or";
            case Opcode::Not:
                return "not";
            case Opcode::BitAnd:
                return "bitand";
            case Opcode::BitOr:
                return "bitor";
            case Opcode::BitXor:
                return "bitxor";
            case Opcode::BitNot:
                return "bitnot";
            case Opcode::Shl:
                return "shl";
            case Opcode::Shr:
                return "shr";
            default:
                return "unknown";
            }
        }
    };

} // namespace Fern::HLIR