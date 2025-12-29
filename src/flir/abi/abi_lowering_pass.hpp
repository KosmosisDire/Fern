#pragma once

#include "abi_rules.hpp"
#include "../flir.hpp"
#include <memory>
#include <unordered_map>

namespace Fern::FLIR::ABI
{

class LoweringPass
{
public:
    LoweringPass(std::unique_ptr<Rules> rules);

    void run(Module& module);

private:
    std::unique_ptr<Rules> rules;
    Module* current_module = nullptr;

    // Cache of function ABI info for extern functions
    std::unordered_map<Function*, FunctionABIInfo> function_abi_cache;

    #pragma region Pass Phases

    // Phase 1: Analyze and transform extern function signatures
    void lower_extern_functions(Module& module);

    // Phase 2: Transform call sites to extern functions
    void lower_extern_calls(Module& module);

    #pragma region Signature Lowering

    void lower_extern_signature(Function& fn);

    #pragma region Call Site Lowering

    void lower_calls_in_function(Function& fn);

    void lower_extern_call_inst(Function& fn, BasicBlock& block,
                                 std::vector<std::unique_ptr<Instruction>>& output,
                                 CallInst* call, const FunctionABIInfo& abi_info);

    #pragma region Helpers

    IRTypeSystem& types() { return current_module->ir_types; }
};

} // namespace Fern::FLIR::ABI
