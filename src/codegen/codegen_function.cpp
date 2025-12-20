// codegen_function.cpp - Function-Level Code Generation Context Implementation
#include "codegen_function.hpp"
#include <llvm/IR/BasicBlock.h>
#include <sstream>
#include <stdexcept>

namespace Fern
{
    // === Value Management ===

    llvm::Value* CodeGenFunction::get_value(FNIR::Value* fnir_value)
    {
        auto it = value_map.find(fnir_value);
        if (it == value_map.end())
        {
            throw std::runtime_error(format_value_error(fnir_value, "FNIR value not found"));
        }
        return it->second;
    }

    void CodeGenFunction::map_value(FNIR::Value* fnir_value, llvm::Value* llvm_value)
    {
        value_map[fnir_value] = llvm_value;
    }

    bool CodeGenFunction::has_value(FNIR::Value* fnir_value) const
    {
        return value_map.find(fnir_value) != value_map.end();
    }

    // === Block Management ===

    llvm::BasicBlock* CodeGenFunction::get_block(FNIR::BasicBlock* fnir_block)
    {
        auto it = block_map.find(fnir_block);
        if (it == block_map.end())
        {
            throw std::runtime_error(format_block_error(fnir_block, "FNIR block not found"));
        }
        return it->second;
    }

    void CodeGenFunction::map_block(FNIR::BasicBlock* fnir_block, llvm::BasicBlock* llvm_block)
    {
        block_map[fnir_block] = llvm_block;
    }

    bool CodeGenFunction::has_block(FNIR::BasicBlock* fnir_block) const
    {
        return block_map.find(fnir_block) != block_map.end();
    }

    void CodeGenFunction::create_all_blocks()
    {
        for (const auto& fnir_block : fnir_function->blocks)
        {
            std::string block_name = fnir_block->name.empty()
                ? "bb" + std::to_string(fnir_block->id)
                : fnir_block->name;

            llvm::BasicBlock* llvm_block = llvm::BasicBlock::Create(
                CGM.get_context(),
                block_name,
                llvm_function);

            map_block(fnir_block.get(), llvm_block);
        }
    }

    // === Parameter Mapping ===

    void CodeGenFunction::map_parameters()
    {
        size_t arg_idx = 0;
        for (auto& arg : llvm_function->args())
        {
            if (arg_idx < fnir_function->params.size())
            {
                map_value(fnir_function->params[arg_idx], &arg);
            }
            arg_idx++;
        }
    }

    // === Error Helpers ===

    std::string CodeGenFunction::format_value_error(FNIR::Value* value, const std::string& message)
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

    std::string CodeGenFunction::format_block_error(FNIR::BasicBlock* block, const std::string& message)
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
