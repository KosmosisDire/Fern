// flir.cpp - FLIR dump implementation
#include "flir.hpp"

namespace Fern::FLIR
{

std::string Module::dump() const
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

std::string Module::dump_type_definition(const IRStruct *type_def)
{
    std::stringstream ss;

    ss << "type " << type_def->name << "   ::   " << type_def->size << " bytes, align " << type_def->alignment;

    if (type_def->fields.empty())
    {
        ss << " { }\n";
        return ss.str();
    }

    ss << " \n{\n";

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

std::string Module::dump_function(const Function *func)
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
        ss << "\n";
        return ss.str();
    }

    if (func->is_empty())
    {
        ss << " { }\n";
        return ss.str();
    }

    ss << "\n{\n";
    ss << dump_instruction_list(func->body, 1);
    ss << "}\n";

    return ss.str();
}

std::string Module::dump_instruction_list(const InstructionList &list, int indent)
{
    std::stringstream ss;
    std::string pad(indent * 4, ' ');

    for (const auto &inst : list)
    {
        ss << dump_instruction(inst.get(), indent);
    }

    return ss.str();
}

std::string Module::dump_instruction(const Instruction *inst, int indent)
{
    std::stringstream ss;
    std::string pad(indent * 4, ' ');

    switch (inst->op)
    {
#pragma region Control Flow

    case Opcode::If:
    {
        auto *if_inst = static_cast<const IfInst *>(inst);
        ss << pad << to_string(inst->op) << " " << value_ref(if_inst->condition) << "\n";
        ss << pad << "{\n";
        ss << dump_instruction_list(if_inst->then_body, indent + 1);
        ss << pad << "}\n";
        if (!if_inst->else_body.empty())
        {
            ss << pad << "else\n";
            ss << pad << "{\n";
            ss << dump_instruction_list(if_inst->else_body, indent + 1);
            ss << pad << "}\n";
        }
        break;
    }
    case Opcode::Loop:
    {
        auto *loop_inst = static_cast<const LoopInst *>(inst);
        ss << pad << to_string(inst->op);
        if (!loop_inst->label.empty())
            ss << " <" << loop_inst->label << ">";
        ss << "\n" << pad << "{\n";
        ss << dump_instruction_list(loop_inst->body, indent + 1);
        ss << pad << "}\n";
        break;
    }
    case Opcode::Block:
    {
        auto *block_inst = static_cast<const BlockInst *>(inst);
        ss << pad << to_string(inst->op);
        if (!block_inst->label.empty())
            ss << " <" << block_inst->label << ">";
        ss << "\n" << pad << "{\n";
        ss << dump_instruction_list(block_inst->body, indent + 1);
        ss << pad << "}\n";
        break;
    }
    case Opcode::Br:
    {
        auto *br = static_cast<const BrInst *>(inst);
        ss << pad << to_string(inst->op) << " " << br->depth << "\n";
        break;
    }
    case Opcode::BrIf:
    {
        auto *br = static_cast<const BrIfInst *>(inst);
        ss << pad << to_string(inst->op) << " " << value_ref(br->condition) << ", " << br->depth << "\n";
        break;
    }
    case Opcode::Continue:
    {
        auto *cont = static_cast<const ContinueInst *>(inst);
        ss << pad << to_string(inst->op) << " " << cont->depth << "\n";
        break;
    }
    case Opcode::ContinueIf:
    {
        auto *cont = static_cast<const ContinueIfInst *>(inst);
        ss << pad << to_string(inst->op) << " " << value_ref(cont->condition) << ", " << cont->depth << "\n";
        break;
    }

#pragma region Value Instructions

    default:
    {
        ss << pad;
        if (inst->result)
            ss << value_ref(inst->result) << " = ";

        switch (inst->op)
        {
        case Opcode::ConstInt:
        {
            auto *ci = static_cast<const ConstIntInst *>(inst);
            ss << to_string(inst->op) << " " << ci->value;
            break;
        }
        case Opcode::ConstFloat:
        {
            auto *cf = static_cast<const ConstFloatInst *>(inst);
            ss << to_string(inst->op) << " " << cf->value;
            break;
        }
        case Opcode::ConstBool:
        {
            auto *cb = static_cast<const ConstBoolInst *>(inst);
            ss << to_string(inst->op) << " " << (cb->value ? "true" : "false");
            break;
        }
        case Opcode::ConstString:
        {
            auto *cs = static_cast<const ConstStringInst *>(inst);
            ss << to_string(inst->op) << " \"" << cs->value << "\"";
            break;
        }
        case Opcode::ConstNull:
        {
            auto *cn = static_cast<const ConstNullInst *>(inst);
            ss << to_string(inst->op) << " " << cn->null_type->get_name();
            break;
        }
        case Opcode::StackAlloc:
        {
            auto *alloc = static_cast<const StackAllocInst *>(inst);
            ss << to_string(inst->op) << " " << alloc->alloc_type->get_name();
            if (!alloc->escapes)
                ss << " [no-escape]";
            break;
        }
        case Opcode::StackAllocBytes:
        {
            auto *alloc = static_cast<const StackAllocBytesInst *>(inst);
            ss << to_string(inst->op) << " " << value_ref(alloc->size);
            break;
        }
        case Opcode::HeapAlloc:
        {
            auto *alloc = static_cast<const HeapAllocInst *>(inst);
            ss << to_string(inst->op) << " " << alloc->alloc_type->get_name();
            break;
        }
        case Opcode::HeapAllocBytes:
        {
            auto *alloc = static_cast<const HeapAllocBytesInst *>(inst);
            ss << to_string(inst->op) << " " << value_ref(alloc->size);
            break;
        }
        case Opcode::HeapFree:
        {
            auto *free_inst = static_cast<const HeapFreeInst *>(inst);
            ss << to_string(inst->op) << " " << value_ref(free_inst->ptr);
            break;
        }
        case Opcode::Load:
        {
            auto *load = static_cast<const LoadInst *>(inst);
            ss << to_string(inst->op) << " " << value_ref(load->address);
            break;
        }
        case Opcode::Store:
        {
            auto *store = static_cast<const StoreInst *>(inst);
            ss << to_string(inst->op) << " " << value_ref(store->value) << ", " << value_ref(store->address);
            break;
        }
        case Opcode::FieldAddr:
        {
            auto *field = static_cast<const FieldAddrInst *>(inst);
            ss << to_string(inst->op) << " " << value_ref(field->object) << ", " << field->field_index;
            if (!field->field_name.empty())
                ss << " \"" << field->field_name << "\"";
            break;
        }
        case Opcode::ElementAddr:
        {
            auto *elem = static_cast<const ElementAddrInst *>(inst);
            ss << to_string(inst->op) << " " << value_ref(elem->array) << ", " << value_ref(elem->index);
            break;
        }
        case Opcode::MemCpy:
        {
            auto *memcpy_inst = static_cast<const MemCpyInst *>(inst);
            ss << to_string(inst->op) << " " << value_ref(memcpy_inst->dest) << ", " << value_ref(memcpy_inst->src) << ", " << value_ref(memcpy_inst->size);
            if (memcpy_inst->is_volatile)
                ss << " [volatile]";
            break;
        }
        case Opcode::MemSet:
        {
            auto *memset_inst = static_cast<const MemSetInst *>(inst);
            ss << to_string(inst->op) << " " << value_ref(memset_inst->dest) << ", " << value_ref(memset_inst->value) << ", " << value_ref(memset_inst->size);
            if (memset_inst->is_volatile)
                ss << " [volatile]";
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
        case Opcode::Sar:
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
        case Opcode::Convert:
        {
            auto *conv = static_cast<const ConvertInst *>(inst);
            ss << to_string(inst->op) << " " << value_ref(conv->value) << " to " << conv->target_type->get_name();
            break;
        }
        case Opcode::Call:
        {
            auto *call = static_cast<const CallInst *>(inst);
            ss << to_string(inst->op) << " ";
            if (call->callee->is_external)
                ss << "external ";
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
        case Opcode::CallIndirect:
        {
            auto *call = static_cast<const CallIndirectInst *>(inst);
            ss << to_string(inst->op) << " " << value_ref(call->callee) << "(";
            for (size_t i = 0; i < call->args.size(); ++i)
            {
                if (i > 0)
                    ss << ", ";
                ss << value_ref(call->args[i]);
            }
            ss << ")";
            break;
        }
        case Opcode::Return:
        {
            auto *ret = static_cast<const ReturnInst *>(inst);
            ss << to_string(inst->op);
            if (ret->value)
                ss << " " << value_ref(ret->value);
            break;
        }
        default:
            ss << "unknown_op_" << static_cast<int>(inst->op);
        }

        if (inst->result && inst->result->type)
            ss << "  ::  " << inst->result->type->get_name();

        ss << "\n";
        break;
    }
    }

    return ss.str();
}

std::string Module::value_ref(const Value *val)
{
    if (!val)
        return "<null>";
    std::stringstream ss;
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

} // namespace Fern::FLIR
