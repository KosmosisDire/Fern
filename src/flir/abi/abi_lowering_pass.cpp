#include "abi_lowering_pass.hpp"
#include <algorithm>

namespace Fern::FLIR::ABI
{

LoweringPass::LoweringPass(std::unique_ptr<Rules> rules)
    : rules(std::move(rules))
{
}

void LoweringPass::run(Module& module)
{
    current_module = &module;

    // Phase 1: Analyze and transform extern function signatures
    lower_extern_functions(module);

    // Phase 2: Transform call sites to extern functions
    lower_extern_calls(module);

    current_module = nullptr;
}

#pragma region Extern Sig

void LoweringPass::lower_extern_functions(Module& module)
{
    for (auto& func : module.functions)
    {
        if (func->is_external)
        {
            lower_extern_signature(*func);
        }
    }
}

void LoweringPass::lower_extern_signature(Function& fn)
{
    // Build list of original param types
    std::vector<IRTypePtr> param_types;
    for (auto* param : fn.params)
        param_types.push_back(param->type);

    FunctionABIInfo abi_info = rules->classify_function(fn.return_type, param_types);

    // Cache for later use when lowering call sites
    function_abi_cache[&fn] = abi_info;

    if (!abi_info.requires_transformation())
        return;

    // Handle return type transformation
    if (abi_info.return_info.is_sret())
    {
        IRTypePtr sret_ptr_type = types().get_pointer(fn.return_type);
        auto* sret_param = fn.create_value(sret_ptr_type, "sret");
        fn.params.insert(fn.params.begin(), sret_param);
        fn.return_type = types().get_void();
    }
    else if (abi_info.return_info.is_coerce())
    {
        fn.return_type = abi_info.return_info.coerced_type;
    }

    // Transform parameters
    // If sret was added, param indices are offset by 1
    size_t param_offset = abi_info.return_info.is_sret() ? 1 : 0;
    for (size_t i = 0; i < abi_info.param_infos.size(); ++i)
    {
        const auto& param_info = abi_info.param_infos[i];
        size_t param_index = i + param_offset;

        if (param_info.is_coerce())
        {
            fn.params[param_index]->type = param_info.coerced_type;
        }
        else if (param_info.is_indirect())
        {
            // Parameter becomes a pointer type
            IRTypePtr original_type = fn.params[param_index]->type;
            fn.params[param_index]->type = types().get_pointer(original_type);
        }
    }
}

#pragma region Call Sites

void LoweringPass::lower_extern_calls(Module& module)
{
    for (auto& func : module.functions)
    {
        if (!func->is_external)
        {
            lower_calls_in_function(*func);
        }
    }
}

void LoweringPass::lower_calls_in_function(Function& fn)
{
    for (auto& block : fn.blocks)
    {
        // Build a new instruction list to avoid modifying while iterating
        std::vector<std::unique_ptr<Instruction>> new_instructions;

        for (size_t i = 0; i < block->instructions.size(); ++i)
        {
            auto& inst = block->instructions[i];

            if (inst->op == Opcode::Call)
            {
                auto* call = static_cast<CallInst*>(inst.get());

                if (call->callee->is_external)
                {
                    // Check if this function has ABI lowering info
                    auto it = function_abi_cache.find(call->callee);
                    if (it != function_abi_cache.end() && it->second.requires_transformation())
                    {
                        lower_extern_call_inst(fn, *block, new_instructions, call, it->second);
                        continue;
                    }
                }
            }

            // No transformation needed
            new_instructions.push_back(std::move(inst));
        }

        // Replace instructions with transformed ones
        block->instructions = std::move(new_instructions);
    }
}

void LoweringPass::lower_extern_call_inst(Function& fn, BasicBlock& block,
                                           std::vector<std::unique_ptr<Instruction>>& output,
                                           CallInst* call, const FunctionABIInfo& abi_info)
{
    std::vector<Value*> new_args;
    Value* sret_alloc = nullptr;
    IRTypePtr original_return_type = nullptr;

    // Determine original return type before any transformation
    if (call->result && call->result->type)
    {
        original_return_type = call->result->type;
    }

    // Handle sret return: allocate space and pass as first arg
    if (abi_info.return_info.is_sret() && original_return_type)
    {
        // Create stack allocation for the return value
        IRTypePtr ptr_type = types().get_pointer(original_return_type);
        sret_alloc = fn.create_value(ptr_type, "sret.tmp");

        auto alloc_inst = std::make_unique<StackAllocInst>(sret_alloc, original_return_type);
        alloc_inst->parent = &block;
        output.push_back(std::move(alloc_inst));

        // Add sret pointer as first argument
        new_args.push_back(sret_alloc);
    }

    // Transform each argument
    for (size_t i = 0; i < call->args.size(); ++i)
    {
        Value* arg = call->args[i];
        const auto& param_info = abi_info.param_infos[i];

        if (param_info.is_direct())
        {
            new_args.push_back(arg);
        }
        else if (param_info.is_coerce())
        {
            // The arg is a struct value; we need to reinterpret it as an integer
            // If it came from a load, we can load the address as the coerced type instead
            if (arg->def && arg->def->op == Opcode::Load)
            {
                auto* load = static_cast<LoadInst*>(arg->def);
                // Create a new load that loads as the coerced type
                Value* coerced = fn.create_value(param_info.coerced_type, "coerce.arg");
                auto new_load = std::make_unique<LoadInst>(coerced, load->address);
                new_load->parent = &block;
                output.push_back(std::move(new_load));
                new_args.push_back(coerced);
            }
            else
            {
                // Need to store to temp and reload as coerced type
                IRTypePtr ptr_type = types().get_pointer(arg->type);
                Value* temp = fn.create_value(ptr_type, "coerce.tmp");

                auto alloc = std::make_unique<StackAllocInst>(temp, arg->type);
                alloc->parent = &block;
                output.push_back(std::move(alloc));

                auto store = std::make_unique<StoreInst>(arg, temp);
                store->parent = &block;
                output.push_back(std::move(store));

                Value* coerced = fn.create_value(param_info.coerced_type, "coerce.arg");
                auto load = std::make_unique<LoadInst>(coerced, temp);
                load->parent = &block;
                output.push_back(std::move(load));

                new_args.push_back(coerced);
            }
        }
        else if (param_info.is_indirect())
        {
            // Pass pointer instead of value
            // If arg came from a load, use the load's address
            if (arg->def && arg->def->op == Opcode::Load)
            {
                auto* load = static_cast<LoadInst*>(arg->def);
                new_args.push_back(load->address);
            }
            else
            {
                // Need to allocate and store
                IRTypePtr ptr_type = types().get_pointer(arg->type);
                Value* temp = fn.create_value(ptr_type, "indirect.tmp");

                auto alloc = std::make_unique<StackAllocInst>(temp, arg->type);
                alloc->parent = &block;
                output.push_back(std::move(alloc));

                auto store = std::make_unique<StoreInst>(arg, temp);
                store->parent = &block;
                output.push_back(std::move(store));

                new_args.push_back(temp);
            }
        }
    }

    // Create the new call instruction
    Value* new_result = nullptr;

    if (abi_info.return_info.is_sret())
    {
        // Call returns void, result is in sret location
        new_result = nullptr;
    }
    else if (abi_info.return_info.is_coerce() && call->result)
    {
        // Call returns coerced type
        new_result = fn.create_value(abi_info.return_info.coerced_type, "coerce.ret");
    }
    else
    {
        new_result = call->result;
    }

    auto new_call = std::make_unique<CallInst>(new_result, call->callee, std::move(new_args));
    new_call->parent = &block;
    output.push_back(std::move(new_call));

    // Handle return value conversion
    if (abi_info.return_info.is_sret() && call->result)
    {
        // Load the result from sret location into the original result value
        auto load = std::make_unique<LoadInst>(call->result, sret_alloc);
        load->parent = &block;
        output.push_back(std::move(load));
    }
    else if (abi_info.return_info.is_coerce() && call->result && original_return_type)
    {
        // The result came back as an integer; store it to temp, load as struct
        IRTypePtr ptr_type = types().get_pointer(original_return_type);
        Value* temp = fn.create_value(ptr_type, "ret.tmp");

        auto alloc = std::make_unique<StackAllocInst>(temp, original_return_type);
        alloc->parent = &block;
        output.push_back(std::move(alloc));

        // Store the coerced return value
        auto store = std::make_unique<StoreInst>(new_result, temp);
        store->parent = &block;
        output.push_back(std::move(store));

        // Load as original struct type into the original result
        auto load = std::make_unique<LoadInst>(call->result, temp);
        load->parent = &block;
        output.push_back(std::move(load));
    }
}

} // namespace Fern::FLIR::ABI
