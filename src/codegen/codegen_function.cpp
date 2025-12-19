// codegen_function.cpp - Function-Level Code Generation Context Implementation
#include "codegen_function.hpp"
#include <llvm/IR/BasicBlock.h>
#include <sstream>
#include <stdexcept>

namespace Fern
{
    // === Value Management ===

    llvm::Value* CodeGenFunction::get_value(HLIR::Value* hlir_value)
    {
        auto it = value_map.find(hlir_value);
        if (it == value_map.end())
        {
            throw std::runtime_error(format_value_error(hlir_value, "HLIR value not found"));
        }
        return it->second;
    }

    void CodeGenFunction::map_value(HLIR::Value* hlir_value, llvm::Value* llvm_value)
    {
        value_map[hlir_value] = llvm_value;
    }

    bool CodeGenFunction::has_value(HLIR::Value* hlir_value) const
    {
        return value_map.find(hlir_value) != value_map.end();
    }

    // === Block Management ===

    llvm::BasicBlock* CodeGenFunction::get_block(HLIR::BasicBlock* hlir_block)
    {
        auto it = block_map.find(hlir_block);
        if (it == block_map.end())
        {
            throw std::runtime_error(format_block_error(hlir_block, "HLIR block not found"));
        }
        return it->second;
    }

    void CodeGenFunction::map_block(HLIR::BasicBlock* hlir_block, llvm::BasicBlock* llvm_block)
    {
        block_map[hlir_block] = llvm_block;
    }

    bool CodeGenFunction::has_block(HLIR::BasicBlock* hlir_block) const
    {
        return block_map.find(hlir_block) != block_map.end();
    }

    void CodeGenFunction::create_all_blocks()
    {
        for (const auto& hlir_block : hlir_function->blocks)
        {
            std::string block_name = hlir_block->name.empty()
                ? "bb" + std::to_string(hlir_block->id)
                : hlir_block->name;

            llvm::BasicBlock* llvm_block = llvm::BasicBlock::Create(
                CGM.get_context(),
                block_name,
                llvm_function);

            map_block(hlir_block.get(), llvm_block);
        }
    }

    // === Parameter Mapping ===

    void CodeGenFunction::map_parameters()
    {
        size_t arg_idx = 0;
        for (auto& arg : llvm_function->args())
        {
            if (arg_idx < hlir_function->params.size())
            {
                map_value(hlir_function->params[arg_idx], &arg);
            }
            arg_idx++;
        }
    }

    // === Error Helpers ===

    std::string CodeGenFunction::format_value_error(HLIR::Value* value, const std::string& message)
    {
        std::stringstream ss;
        ss << message << ": %" << value->id;
        if (!value->debug_name.empty())
        {
            ss << " <" << value->debug_name << ">";
        }
        ss << " : " << (value->type ? value->type->get_name() : "null");
        return ss.str();
    }

    std::string CodeGenFunction::format_block_error(HLIR::BasicBlock* block, const std::string& message)
    {
        std::stringstream ss;
        ss << message << ": bb" << block->id;
        if (!block->name.empty())
        {
            ss << " <" << block->name << ">";
        }
        return ss.str();
    }

} // namespace Fern
