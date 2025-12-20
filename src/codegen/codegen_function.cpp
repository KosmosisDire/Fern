// codegen_function.cpp - Function-Level Code Generation Context Implementation
#include "codegen_function.hpp"
#include <llvm/IR/BasicBlock.h>
#include <sstream>
#include <stdexcept>

namespace Fern
{
    // === Value Management ===

    llvm::Value* CodeGenFunction::get_value(FLIR::Value* flir_value)
    {
        auto it = value_map.find(flir_value);
        if (it == value_map.end())
        {
            throw std::runtime_error(format_value_error(flir_value, "FLIR value not found"));
        }
        return it->second;
    }

    void CodeGenFunction::map_value(FLIR::Value* flir_value, llvm::Value* llvm_value)
    {
        value_map[flir_value] = llvm_value;
    }

    bool CodeGenFunction::has_value(FLIR::Value* flir_value) const
    {
        return value_map.find(flir_value) != value_map.end();
    }

    // === Block Management ===

    llvm::BasicBlock* CodeGenFunction::get_block(FLIR::BasicBlock* flir_block)
    {
        auto it = block_map.find(flir_block);
        if (it == block_map.end())
        {
            throw std::runtime_error(format_block_error(flir_block, "FLIR block not found"));
        }
        return it->second;
    }

    void CodeGenFunction::map_block(FLIR::BasicBlock* flir_block, llvm::BasicBlock* llvm_block)
    {
        block_map[flir_block] = llvm_block;
    }

    bool CodeGenFunction::has_block(FLIR::BasicBlock* flir_block) const
    {
        return block_map.find(flir_block) != block_map.end();
    }

    void CodeGenFunction::create_all_blocks()
    {
        for (const auto& flir_block : flir_function->blocks)
        {
            std::string block_name = flir_block->name.empty()
                ? "bb" + std::to_string(flir_block->id)
                : flir_block->name;

            llvm::BasicBlock* llvm_block = llvm::BasicBlock::Create(
                CGM.get_context(),
                block_name,
                llvm_function);

            map_block(flir_block.get(), llvm_block);
        }
    }

    // === Parameter Mapping ===

    void CodeGenFunction::map_parameters()
    {
        size_t arg_idx = 0;
        for (auto& arg : llvm_function->args())
        {
            if (arg_idx < flir_function->params.size())
            {
                map_value(flir_function->params[arg_idx], &arg);
            }
            arg_idx++;
        }
    }

    // === Error Helpers ===

    std::string CodeGenFunction::format_value_error(FLIR::Value* value, const std::string& message)
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

    std::string CodeGenFunction::format_block_error(FLIR::BasicBlock* block, const std::string& message)
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
