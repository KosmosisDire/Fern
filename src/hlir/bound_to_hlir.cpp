// bound_to_hlir.cpp
#include "bound_to_hlir.hpp"
#include <cassert>
#include <iostream>
#include <functional>

namespace Fern::HLIR
{

#pragma region Intrinsic Table

using IntrinsicHandler = std::function<HLIR::Value*(BoundToHLIR*, const std::vector<HLIR::Value*>&)>;

struct IntrinsicInfo {
    size_t arity;
    IntrinsicHandler handler;
};

static const std::unordered_map<std::string, IntrinsicInfo> intrinsic_table = {
    {"alloca", {1, [](BoundToHLIR* self, const std::vector<HLIR::Value*>& args) {
        return self->builder.stack_alloc_bytes(args[0]);
    }}},
    {"malloc", {1, [](BoundToHLIR* self, const std::vector<HLIR::Value*>& args) {
        return self->builder.heap_alloc_bytes(args[0]);
    }}},
    {"free", {1, [](BoundToHLIR* self, const std::vector<HLIR::Value*>& args) {
        self->builder.heap_free(args[0]);
        return nullptr;
    }}},
    {"memcpy", {3, [](BoundToHLIR* self, const std::vector<HLIR::Value*>& args) {
        self->builder.memcpy(args[0], args[1], args[2]);
        return args[0];
    }}},
    {"memset", {3, [](BoundToHLIR* self, const std::vector<HLIR::Value*>& args) {
        self->builder.memset(args[0], args[1], args[2]);
        return args[0];
    }}},
};

#pragma endregion

#pragma region Core Infrastructure

void BoundToHLIR::build(BoundCompilationUnit* unit) {
    visit(unit);
}

HLIR::Value* BoundToHLIR::emit_rvalue(BoundExpression* expr) {
    if (!expr) return nullptr;
    
    expr->accept(this);
    auto it = lowered.find(expr);
    if (it == lowered.end() || !it->second.result) {
        return nullptr;
    }
    
    auto& lr = it->second;
    
    if (!lr.is_address) {
        return lr.result;
    }
    
    auto type = expr->type;
    
    if (type->is<ArrayType>()) {
        return lr.result;
    }
    
    if (type->is_reference_type()) {
        auto ptr_type = type_system->get_pointer(type);
        return builder.load(lr.result, ptr_type);
    }
    
    return builder.load(lr.result, type);
}

HLIR::Value* BoundToHLIR::emit_lvalue(BoundExpression* expr) {
    if (!expr) return nullptr;
    
    expr->accept(this);
    auto it = lowered.find(expr);
    if (it == lowered.end() || !it->second.result) {
        return nullptr;
    }
    
    auto& lr = it->second;
    
    if (lr.is_address) {
        return lr.result;
    }
    
    auto temp = builder.stack_alloc(expr->type);
    builder.store(lr.result, temp);
    return temp;
}

#pragma endregion

#pragma region Helpers

HLIR::Value* BoundToHLIR::get_this_param() {
    if (!current_function || current_function->is_static || current_function->params.empty()) {
        return nullptr;
    }
    return current_function->params[0];
}

void BoundToHLIR::emit_store(HLIR::Value* dest, HLIR::Value* src, TypePtr type) {
    if (auto arr = type->as<ArrayType>(); arr && arr->size > 0) {
        size_t byte_size = arr->element->get_size() * arr->size;
        auto size_val = builder.const_int(static_cast<int64_t>(byte_size), type_system->get_i32());
        builder.memcpy(dest, src, size_val);
    } else {
        builder.store(src, dest);
    }
}

std::optional<HLIR::Value*> BoundToHLIR::try_pointer_arithmetic(
    BoundBinaryExpression* node, 
    HLIR::Value* left, 
    HLIR::Value* right) 
{
    auto ltype = node->left->type;
    auto rtype = node->right->type;
    bool is_add = node->operatorKind == BinaryOperatorKind::Add;
    bool is_sub = node->operatorKind == BinaryOperatorKind::Subtract;
    
    if (!is_add && !is_sub) {
        return std::nullopt;
    }
    
    if (ltype->is<PointerType>() && rtype->is<PrimitiveType>()) {
        auto elem = ltype->as<PointerType>()->pointee;
        HLIR::Value* index = right;
        if (is_sub) {
            index = builder.unary(Opcode::Neg, right);
        }
        return builder.element_addr(left, index, elem);
    }
    
    if (is_add && rtype->is<PointerType>() && ltype->is<PrimitiveType>()) {
        auto elem = rtype->as<PointerType>()->pointee;
        return builder.element_addr(right, left, elem);
    }
    
    if (is_sub && ltype->is<PointerType>() && rtype->is<PointerType>()) {
        return builder.binary(Opcode::Sub, left, right);
    }
    
    return std::nullopt;
}

std::optional<HLIR::Value*> BoundToHLIR::try_emit_intrinsic(
    FunctionSymbol* method, 
    const std::vector<HLIR::Value*>& args, 
    const SourceRange& loc) 
{
    if (!method->is_intrinsic) {
        return std::nullopt;
    }
    
    auto it = intrinsic_table.find(method->name);
    if (it == intrinsic_table.end()) {
        error("Unknown intrinsic: " + method->name, loc);
        return nullptr;
    }
    
    const auto& info = it->second;
    if (args.size() != info.arity) {
        error("Intrinsic '" + method->name + "' expects " + 
              std::to_string(info.arity) + " arguments", loc);
        return nullptr;
    }
    
    return info.handler(this, args);
}

void BoundToHLIR::branch_if_open(HLIR::BasicBlock* target) {
    if (current_block && !current_block->terminator()) {
        builder.set_block(current_block);
        builder.br(target);
    }
}

size_t BoundToHLIR::get_field_index(TypeSymbol* type_sym, Symbol* field_sym) {
    if (!type_sym || !field_sym) return 0;

    size_t index = 0;
    for (auto* member : type_sym->member_order) {
        if (member->is<VariableSymbol>()) {
            if (member == field_sym) {
                return index;
            }
            index++;
        }
    }
    return 0;
}

HLIR::BasicBlock* BoundToHLIR::create_block(const std::string& name) {
    return current_function->create_block(name);
}

HLIR::Opcode BoundToHLIR::get_binary_opcode(BinaryOperatorKind kind) {
    switch (kind) {
        case BinaryOperatorKind::Add: return HLIR::Opcode::Add;
        case BinaryOperatorKind::Subtract: return HLIR::Opcode::Sub;
        case BinaryOperatorKind::Multiply: return HLIR::Opcode::Mul;
        case BinaryOperatorKind::Divide: return HLIR::Opcode::Div;
        case BinaryOperatorKind::Modulo: return HLIR::Opcode::Rem;
        case BinaryOperatorKind::Equals: return HLIR::Opcode::Eq;
        case BinaryOperatorKind::NotEquals: return HLIR::Opcode::Ne;
        case BinaryOperatorKind::LessThan: return HLIR::Opcode::Lt;
        case BinaryOperatorKind::LessThanOrEqual: return HLIR::Opcode::Le;
        case BinaryOperatorKind::GreaterThan: return HLIR::Opcode::Gt;
        case BinaryOperatorKind::GreaterThanOrEqual: return HLIR::Opcode::Ge;
        case BinaryOperatorKind::LogicalAnd: return HLIR::Opcode::And;
        case BinaryOperatorKind::LogicalOr: return HLIR::Opcode::Or;
        case BinaryOperatorKind::BitwiseAnd: return HLIR::Opcode::BitAnd;
        case BinaryOperatorKind::BitwiseOr: return HLIR::Opcode::BitOr;
        case BinaryOperatorKind::BitwiseXor: return HLIR::Opcode::BitXor;
        case BinaryOperatorKind::LeftShift: return HLIR::Opcode::ShiftL;
        case BinaryOperatorKind::RightShift: return HLIR::Opcode::ShiftR;
        default: return HLIR::Opcode::Add;
    }
}

HLIR::Opcode BoundToHLIR::get_unary_opcode(UnaryOperatorKind kind) {
    switch (kind) {
        case UnaryOperatorKind::Minus: return HLIR::Opcode::Neg;
        case UnaryOperatorKind::Not: return HLIR::Opcode::Not;
        case UnaryOperatorKind::BitwiseNot: return HLIR::Opcode::BitNot;
        default: return HLIR::Opcode::Neg;
    }
}

HLIR::Opcode BoundToHLIR::get_compound_opcode(AssignmentOperatorKind kind) {
    switch (kind) {
        case AssignmentOperatorKind::Add: return HLIR::Opcode::Add;
        case AssignmentOperatorKind::Subtract: return HLIR::Opcode::Sub;
        case AssignmentOperatorKind::Multiply: return HLIR::Opcode::Mul;
        case AssignmentOperatorKind::Divide: return HLIR::Opcode::Div;
        case AssignmentOperatorKind::Modulo: return HLIR::Opcode::Rem;
        case AssignmentOperatorKind::And: return HLIR::Opcode::BitAnd;
        case AssignmentOperatorKind::Or: return HLIR::Opcode::BitOr;
        case AssignmentOperatorKind::Xor: return HLIR::Opcode::BitXor;
        case AssignmentOperatorKind::LeftShift: return HLIR::Opcode::ShiftL;
        case AssignmentOperatorKind::RightShift: return HLIR::Opcode::ShiftR;
        default: return HLIR::Opcode::Add;
    }
}

void BoundToHLIR::emit_string_init(HLIR::Value* string_addr, HLIR::Value* data_ptr, size_t length) {
    auto char_ptr_type = type_system->get_pointer(type_system->get_primitive("char"));
    auto data_field = builder.field_addr(string_addr, 0, char_ptr_type, "data");
    builder.store(data_ptr, data_field);

    auto len_val = builder.const_int(static_cast<int64_t>(length), type_system->get_i32());
    auto len_field = builder.field_addr(string_addr, 1, type_system->get_i32(), "length");
    builder.store(len_val, len_field);
}

#pragma endregion

#pragma region Expression Visitors

void BoundToHLIR::visit(BoundLiteralExpression* node) {
    HLIR::Value* result = nullptr;

    if (std::holds_alternative<int64_t>(node->constantValue)) {
        result = builder.const_int(std::get<int64_t>(node->constantValue), node->type);
    }
    else if (std::holds_alternative<uint64_t>(node->constantValue)) {
        result = builder.const_int(static_cast<int64_t>(std::get<uint64_t>(node->constantValue)), node->type);
    }
    else if (std::holds_alternative<bool>(node->constantValue)) {
        result = builder.const_bool(std::get<bool>(node->constantValue), node->type);
    }
    else if (std::holds_alternative<double>(node->constantValue)) {
        result = builder.const_float(std::get<double>(node->constantValue), node->type);
    }
    else if (std::holds_alternative<std::string>(node->constantValue))
    {
        const auto& str_val = std::get<std::string>(node->constantValue);

        if (type_system->is_string_type(node->type))
        {
            auto string_addr = builder.smart_alloc(node->type);
            auto data_ptr = builder.const_string(str_val, type_system->get_pointer(type_system->get_primitive("char")));
            auto len_val = builder.const_int(static_cast<int64_t>(str_val.length()), type_system->get_i32());

            // Call String.New constructor
            auto string_ctor = module->find_function_by_name("String.New");
            if (string_ctor) {
                builder.call(string_ctor, {string_addr, data_ptr, len_val});
            } else {
                error("String.New constructor not found", node->location);
            }

            if (node->type->is_reference_type()) {
                result = string_addr;
            } else {
                result = builder.load(string_addr, node->type);
            }

        } else {
            result = builder.const_string(str_val, node->type);
        }
    }
    else if (std::holds_alternative<std::monostate>(node->constantValue)) {
        if (node->literalKind == LiteralKind::Null) {
            result = builder.const_null(node->type);
        } else {
            error("Literal expression has no constant value (type: " +
                  (node->type ? node->type->get_name() : "null") + ")", node->location);
        }
    }
    else {
        error("Literal expression has unknown variant type (index: " +
              std::to_string(node->constantValue.index()) + ", type: " +
              (node->type ? node->type->get_name() : "null") + ")", node->location);
    }

    lowered[node] = {result, false};
}

void BoundToHLIR::visit(BoundNameExpression* node) {
    if (!node->symbol) {
        lowered[node] = {nullptr, false};
        return;
    }

    if (node->symbol->is<VariableSymbol>()) {
        auto parent = node->symbol->parent;
        if (parent && parent->is<TypeSymbol>()) {
            auto this_ptr = get_this_param();
            if (!this_ptr) {
                error("Cannot access field without 'this'", node->location);
                lowered[node] = {nullptr, false};
                return;
            }
            size_t field_index = get_field_index(parent->as<TypeSymbol>(), node->symbol);
            auto field_addr = builder.field_addr(this_ptr, field_index, node->type, node->symbol->name);
            lowered[node] = {field_addr, true};
            return;
        }
    }

    auto var_addr = variable_addresses[node->symbol];
    if (!var_addr) {
        error("Undefined variable: " + node->symbol->name, node->location);
        lowered[node] = {nullptr, false};
        return;
    }

    if (node->type->is<ArrayType>() && node->symbol->is<ParameterSymbol>()) {
        auto ptr_type = type_system->get_pointer(node->type->as<ArrayType>()->element);
        auto loaded_ptr = builder.load(var_addr, ptr_type, node->symbol->name);
        lowered[node] = {loaded_ptr, false};
        return;
    }

    lowered[node] = {var_addr, true};
}

void BoundToHLIR::visit(BoundBinaryExpression* node) {
    // Handle short-circuit evaluation for logical operators
    if (node->operatorKind == BinaryOperatorKind::LogicalAnd ||
        node->operatorKind == BinaryOperatorKind::LogicalOr) {

        auto left = emit_rvalue(node->left);
        if (!left) {
            lowered[node] = {nullptr, false};
            return;
        }

        // Allocate a temporary to hold the result
        auto result_slot = builder.stack_alloc(node->type);

        // Store left as the initial result (short-circuit value)
        builder.store(left, result_slot);

        auto* eval_right_block = create_block(node->operatorKind == BinaryOperatorKind::LogicalAnd ? "and.rhs" : "or.rhs");
        auto* merge_block = create_block(node->operatorKind == BinaryOperatorKind::LogicalAnd ? "and.merge" : "or.merge");

        // For &&: if left is true, evaluate right; if false, skip (result stays false)
        // For ||: if left is false, evaluate right; if true, skip (result stays true)
        if (node->operatorKind == BinaryOperatorKind::LogicalAnd) {
            builder.cond_br(left, eval_right_block, merge_block);
        } else {
            builder.cond_br(left, merge_block, eval_right_block);
        }

        // Evaluate right operand and store as result
        builder.set_block(eval_right_block);
        current_block = eval_right_block;
        auto right = emit_rvalue(node->right);
        if (!right) {
            lowered[node] = {nullptr, false};
            return;
        }
        builder.store(right, result_slot);
        builder.br(merge_block);

        // Merge block: load the result
        builder.set_block(merge_block);
        current_block = merge_block;

        auto result = builder.load(result_slot, node->type);
        lowered[node] = {result, false};
        return;
    }

    auto left = emit_rvalue(node->left);
    auto right = emit_rvalue(node->right);

    if (!left || !right) {
        std::string msg = "Null operand in binary expression";
        if (!left && node->left && node->left->type) {
            msg += " (left operand type: " + node->left->type->get_name() + ")";
        }
        if (!right && node->right && node->right->type) {
            msg += " (right operand type: " + node->right->type->get_name() + ")";
        }
        error(msg, node->location);
        lowered[node] = {nullptr, false};
        return;
    }

    if (auto ptr_result = try_pointer_arithmetic(node, left, right)) {
        lowered[node] = {*ptr_result, false};
        return;
    }

    auto opcode = get_binary_opcode(node->operatorKind);
    auto result = builder.binary(opcode, left, right);
    lowered[node] = {result, false};
}

void BoundToHLIR::visit(BoundUnaryExpression* node) {
    auto operand = emit_rvalue(node->operand);
    if (!operand) {
        lowered[node] = {nullptr, false};
        return;
    }
    
    auto opcode = get_unary_opcode(node->operatorKind);
    auto result = builder.unary(opcode, operand);
    lowered[node] = {result, false};
}

void BoundToHLIR::visit(BoundAssignmentExpression* node) {
    auto rhs_value = emit_rvalue(node->value);
    if (!rhs_value) {
        lowered[node] = {nullptr, false};
        return;
    }

    auto target_addr = emit_lvalue(node->target);
    if (!target_addr) {
        error("Invalid assignment target", node->location);
        lowered[node] = {nullptr, false};
        return;
    }

    HLIR::Value* final_value = rhs_value;
    if (node->operatorKind != AssignmentOperatorKind::Assign) {
        auto current_value = builder.load(target_addr, node->target->type);
        auto opcode = get_compound_opcode(node->operatorKind);
        final_value = builder.binary(opcode, current_value, rhs_value);
    }

    emit_store(target_addr, final_value, node->target->type);
    lowered[node] = {final_value, false};
}

void BoundToHLIR::visit(BoundCallExpression* node) {
    // Check method validity FIRST
    if (!node->method || !node->method->as<FunctionSymbol>()) {
        lowered[node] = {nullptr, false};
        return;
    }

    auto func_sym = static_cast<FunctionSymbol*>(node->method);

    // Special handling for sizeof intrinsic - extracts type from MetaType
    if (func_sym->is_intrinsic && func_sym->name == "sizeof") {
        if (node->arguments.empty()) {
            error("sizeof requires one argument", node->location);
            lowered[node] = {nullptr, false};
            return;
        }

        auto arg_type = node->arguments[0]->type;
        TypePtr type_to_measure = nullptr;

        // If the argument is a MetaType (type expression), get the inner type
        if (arg_type && arg_type->is<MetaType>()) {
            type_to_measure = arg_type->as<MetaType>()->inner;
        } else {
            // Otherwise, measure the type of the value itself
            type_to_measure = arg_type;
        }

        if (!type_to_measure) {
            error("Cannot determine type for sizeof", node->location);
            lowered[node] = {nullptr, false};
            return;
        }

        size_t size = type_to_measure->get_size();
        auto result = builder.const_int(static_cast<int64_t>(size), type_system->get_i32());
        lowered[node] = {result, false};
        return;
    }

    std::vector<HLIR::Value*> args;

    if (auto member_expr = node->callee->as<BoundMemberAccessExpression>()) {
        bool is_value_type = member_expr->object->type->as<NamedType>() &&
                            member_expr->object->type->is_value_type();
        
        if (is_value_type) {
            auto receiver = emit_lvalue(member_expr->object);
            if (receiver) {
                args.push_back(receiver);
            }
        } else {
            auto receiver = emit_rvalue(member_expr->object);
            if (receiver) {
                args.push_back(receiver);
            }
        }
    }

    for (auto arg : node->arguments) {
        auto arg_val = emit_rvalue(arg);
        if (arg_val) {
            args.push_back(arg_val);
        }
    }

    // Check for other intrinsics
    if (auto intrinsic_result = try_emit_intrinsic(func_sym, args, node->location)) {
        lowered[node] = {*intrinsic_result, false};
        return;
    }

    HLIR::Function* func = module->find_function(func_sym);
    if (!func) {
        lowered[node] = {nullptr, false};
        return;
    }

    auto result = builder.call(func, args);
    lowered[node] = {result, false};
}

void BoundToHLIR::visit(BoundMemberAccessExpression* node) {
    if (!node->member) {
        lowered[node] = {nullptr, false};
        return;
    }

    if (node->member->as<FunctionSymbol>()) {
        lowered[node] = {nullptr, false};
        return;
    }

    bool is_value_type = node->object->type->as<NamedType>() && 
                        node->object->type->is_value_type();
    
    HLIR::Value* obj_addr;
    if (is_value_type) {
        obj_addr = emit_lvalue(node->object);
    } else {
        obj_addr = emit_rvalue(node->object);
    }
    
    if (!obj_addr) {
        lowered[node] = {nullptr, false};
        return;
    }

    if (auto var = node->member->as<VariableSymbol>())
    {
        if (var->is_field())
        {
            size_t field_index = get_field_index(var->parent->as<TypeSymbol>(), var);
            auto field_addr = builder.field_addr(obj_addr, field_index, var->type, var->name);
            lowered[node] = {field_addr, true};
            return;
        }
        else
        {
            auto var_addr = variable_addresses[var];
            lowered[node] = {var_addr, var_addr != nullptr};
        }
        return;
    }
    
    lowered[node] = {nullptr, false};
}

void BoundToHLIR::visit(BoundIndexExpression* node) {
    auto obj_val = emit_rvalue(node->object);
    auto index_val = emit_rvalue(node->index);

    if (!obj_val || !index_val) {
        lowered[node] = {nullptr, false};
        return;
    }

    TypePtr element_type = nullptr;
    if (auto array_type = node->object->type->as<ArrayType>()) {
        element_type = array_type->element;
    } else if (auto ptr_type = node->object->type->as<PointerType>()) {
        element_type = ptr_type->pointee;
    }

    if (!element_type) {
        error("Cannot index non-array/non-pointer type", node->location);
        lowered[node] = {nullptr, false};
        return;
    }

    auto elem_addr = builder.element_addr(obj_val, index_val, element_type);

    // TODO: Remove when arrays become a generic Fern type.
    // Currently needed because nested arrays store pointers but
    // the type system reports element type as T[], not ptr<T[]>.
    if (element_type->is<ArrayType>()) {
        auto loaded = builder.load(elem_addr, element_type);
        lowered[node] = {loaded, false};
    } else {
        lowered[node] = {elem_addr, true};
    }
}

void BoundToHLIR::visit(BoundNewExpression* node) {
    if (!node->type) {
        lowered[node] = {nullptr, false};
        return;
    }

    bool is_value_type = node->type->as<NamedType>() && node->type->is_value_type();

    HLIR::Value* storage;
    if (is_value_type) {
        storage = builder.stack_alloc(node->type);
    } else {
        storage = builder.heap_alloc(node->type);
    }
    
    if (node->constructor) {
        auto ctor_func = module->find_function(node->constructor);
        if (ctor_func) {
            std::vector<HLIR::Value*> args;
            args.push_back(storage);
            
            for (auto arg : node->arguments) {
                auto arg_val = emit_rvalue(arg);
                if (arg_val) {
                    args.push_back(arg_val);
                }
            }
            
            builder.call(ctor_func, args);
        }
    }
    
    if (is_value_type) {
        lowered[node] = {storage, true};
    } else {
        lowered[node] = {storage, false};
    }
}

void BoundToHLIR::visit(BoundArrayCreationExpression* node) {
    auto alloc_result = builder.stack_alloc(node->type);

    if (!node->initializers.empty()) {
        TypePtr element_type = nullptr;
        if (auto array_type = node->type->as<ArrayType>()) {
            element_type = array_type->element;
        }

        if (element_type) {
            auto i32_type = type_system->get_primitive("i32");
            
            for (size_t i = 0; i < node->initializers.size(); i++) {
                auto init_val = emit_rvalue(node->initializers[i]);
                if (init_val) {
                    auto index_val = builder.const_int(static_cast<int64_t>(i), i32_type);
                    auto elem_addr = builder.element_addr(alloc_result, index_val, element_type);
                    builder.store(init_val, elem_addr);
                }
            }
        }
    }

    lowered[node] = {alloc_result, true};
}

void BoundToHLIR::visit(BoundCastExpression* node) {
    auto expr = emit_rvalue(node->expression);
    if (!expr) {
        lowered[node] = {nullptr, false};
        return;
    }
    
    auto result = builder.cast(expr, node->type);
    lowered[node] = {result, false};
}

void BoundToHLIR::visit(BoundThisExpression* node) {
    auto this_ptr = get_this_param();
    if (!this_ptr) {
        error("'this' used in static or non-member context", node->location);
        lowered[node] = {nullptr, false};
        return;
    }

    bool is_value_type = node->type->as<NamedType>() && node->type->is_value_type();
    lowered[node] = {this_ptr, is_value_type};
}

void BoundToHLIR::visit(BoundParenthesizedExpression* node) {
    node->expression->accept(this);
    lowered[node] = lowered[node->expression];
}

void BoundToHLIR::visit(BoundConversionExpression* node) {
    auto expr_val = emit_rvalue(node->expression);
    if (!expr_val) {
        lowered[node] = {nullptr, false};
        return;
    }

    if (node->conversionKind != ConversionKind::Identity && node->type) {
        lowered[node] = {builder.cast(expr_val, node->type), false};
    } else {
        lowered[node] = {expr_val, false};
    }
}

void BoundToHLIR::visit(BoundTypeExpression* node) {
    lowered[node] = {nullptr, false};
}

#pragma endregion

#pragma region Statement Visitors

void BoundToHLIR::visit(BoundBlockStatement* node) {
    for (auto stmt : node->statements) {
        stmt->accept(this);
    }
}

void BoundToHLIR::visit(BoundExpressionStatement* node) {
    emit_rvalue(node->expression);
}

void BoundToHLIR::visit(BoundIfStatement* node) {
    auto cond = emit_rvalue(node->condition);
    if (!cond) return;

    auto then_block = create_block("if.then");
    auto else_block = create_block("if.else");
    HLIR::BasicBlock* merge_block = nullptr;

    builder.cond_br(cond, then_block, else_block);

    builder.set_block(then_block);
    current_block = then_block;
    node->thenStatement->accept(this);
    bool then_terminated = !current_block || current_block->terminator();
    if (!then_terminated) {
        if (!merge_block) merge_block = create_block("if.merge");
        branch_if_open(merge_block);
    }

    builder.set_block(else_block);
    current_block = else_block;
    bool else_terminated = false;
    if (node->elseStatement) {
        node->elseStatement->accept(this);
        else_terminated = !current_block || current_block->terminator();
    }
    if (!else_terminated) {
        if (!merge_block) merge_block = create_block("if.merge");
        branch_if_open(merge_block);
    }

    if (merge_block) {
        builder.set_block(merge_block);
        current_block = merge_block;
    } else {
        current_block = nullptr;
    }
}

void BoundToHLIR::visit(BoundWhileStatement* node) {
    auto header = create_block("while.header");
    auto body = create_block("while.body");
    auto exit = create_block("while.exit");

    builder.br(header);
    builder.set_block(header);
    current_block = header;

    LoopContext ctx;
    ctx.continue_target = header;
    ctx.break_target = exit;
    loop_stack.push(ctx);

    auto cond = emit_rvalue(node->condition);
    if (!cond) {
        std::string msg = "While condition evaluated to null";
        if (node->condition && node->condition->type) {
            msg += " (condition type: " + node->condition->type->get_name() + ")";
        }
        error(msg, node->location);
        builder.br(exit);
        loop_stack.pop();
        builder.set_block(exit);
        current_block = exit;
        return;
    }

    builder.cond_br(cond, body, exit);

    builder.set_block(body);
    current_block = body;
    node->body->accept(this);
    branch_if_open(header);

    loop_stack.pop();

    builder.set_block(exit);
    current_block = exit;
}

void BoundToHLIR::visit(BoundForStatement* node) {
    if (node->initializer) {
        node->initializer->accept(this);
    }

    auto header = create_block("for.header");
    auto body = create_block("for.body");
    auto update = create_block("for.update");
    auto exit = create_block("for.exit");

    builder.br(header);
    builder.set_block(header);
    current_block = header;

    LoopContext ctx;
    ctx.continue_target = update;
    ctx.break_target = exit;
    loop_stack.push(ctx);

    if (node->condition) {
        auto cond = emit_rvalue(node->condition);
        if (cond) {
            builder.cond_br(cond, body, exit);
        }
    } else {
        builder.br(body);
    }

    builder.set_block(body);
    current_block = body;
    node->body->accept(this);
    branch_if_open(update);

    builder.set_block(update);
    current_block = update;
    for (auto inc : node->incrementors) {
        emit_rvalue(inc);
    }
    builder.br(header);

    loop_stack.pop();

    builder.set_block(exit);
    current_block = exit;
}

void BoundToHLIR::visit(BoundBreakStatement* node) {
    if (!loop_stack.empty()) {
        builder.br(loop_stack.top().break_target);
    }
}

void BoundToHLIR::visit(BoundContinueStatement* node) {
    if (!loop_stack.empty()) {
        builder.br(loop_stack.top().continue_target);
    }
}

void BoundToHLIR::visit(BoundReturnStatement* node) {
    if (node->value) {
        auto val = emit_rvalue(node->value);
        builder.ret(val);
    } else {
        builder.ret(nullptr);
    }
}

void BoundToHLIR::visit(BoundUsingStatement* node) {
}

#pragma endregion

#pragma region Declaration Visitors

void BoundToHLIR::visit(BoundVariableDeclaration* node)
{
    auto var_sym = node->symbol->as<VariableSymbol>();
    if (!var_sym) return;

    // fields are preinserted when the types are predefined during init
    if (var_sym->is_field())
    {
        return;
    }

    if (var_sym->type->is_reference_type()) {
        auto ref_ptr = builder.stack_alloc_nested(var_sym->type, node->name);
        variable_addresses[node->symbol] = ref_ptr;

        if (node->initializer) {
            auto init_value = emit_rvalue(node->initializer);
            if (init_value) {
                builder.store(init_value, ref_ptr);
            }
        } else {
            auto ptr_type = type_system->get_pointer(var_sym->type);
            auto null_ptr = builder.const_null(ptr_type);
            builder.store(null_ptr, ref_ptr);
        }
    } else {
        auto var_addr = builder.stack_alloc(var_sym->type, node->name);
        variable_addresses[node->symbol] = var_addr;

        if (node->initializer) {
            if (auto new_expr = node->initializer->as<BoundNewExpression>()) {
                bool is_new_value_type = new_expr->type &&
                    new_expr->type->as<NamedType>() &&
                    new_expr->type->is_value_type();

                if (is_new_value_type && new_expr->constructor) {
                    auto ctor_func = module->find_function(new_expr->constructor);
                    if (ctor_func) {
                        std::vector<HLIR::Value*> args;
                        args.push_back(var_addr);

                        for (auto arg : new_expr->arguments) {
                            auto arg_val = emit_rvalue(arg);
                            if (arg_val) {
                                args.push_back(arg_val);
                            }
                        }

                        builder.call(ctor_func, args);
                        return;
                    }
                }
            }

            auto init_value = emit_rvalue(node->initializer);
            if (init_value) {
                emit_store(var_addr, init_value, var_sym->type);
            }
        } else {
            auto default_value = builder.const_null(var_sym->type);
            builder.store(default_value, var_addr);
        }
    } 
}

void BoundToHLIR::visit(BoundFunctionDeclaration* node) {
    auto func_sym = node->symbol->as<FunctionSymbol>();
    if (!func_sym) return;

    auto func = module->find_function(func_sym);
    if (!func) return;

    func->is_external = func_sym->is_extern();

    variable_addresses.clear();
    lowered.clear();
    current_function = func;

    bool is_member_function = func_sym->parent && func_sym->parent->is<TypeSymbol>();

    if (is_member_function && !func_sym->is_static()) {
        auto parent_type = func_sym->parent->as<TypeSymbol>();
        auto this_ptr_type = type_system->get_pointer(parent_type->type);
        auto this_param = func->create_value(this_ptr_type, "this");
        func->params.push_back(this_param);
    }

    for (size_t i = 0; i < node->parameters.size(); i++) {
        auto param_sym = node->parameters[i]->symbol->as<ParameterSymbol>();
        auto param_type = param_sym->type;
        if (param_type->is_reference_type()) {
            param_type = type_system->get_pointer(param_type);
        }
        auto param = func->create_value(param_type, node->parameters[i]->name);
        func->params.push_back(param);
    }

    if (func_sym->is_extern()) {
        current_function = nullptr;
        return;
    }

    auto entry = func->create_block("entry");
    func->entry = entry;
    current_block = entry;
    builder.set_function(func);
    builder.set_block(entry);

    for (size_t i = 0; i < node->parameters.size(); i++) {
        auto param_sym = node->parameters[i]->symbol->as<ParameterSymbol>();
        size_t param_idx = is_member_function && !func_sym->is_static() ? i + 1 : i;
        auto param_value = func->params[param_idx];

        TypePtr alloc_type = param_sym->type;
        if (auto array_type = param_sym->type->as<ArrayType>()) {
            alloc_type = type_system->get_pointer(array_type->element);
        }

        if (alloc_type->is_reference_type()) {
            alloc_type = type_system->get_pointer(alloc_type);
        }

        auto param_addr = builder.stack_alloc(alloc_type, param_sym->name + ".addr");
        builder.store(param_value, param_addr);
        variable_addresses[param_sym] = param_addr;
    }

    if (node->body) {
        node->body->accept(this);
    }

    if (current_block && !current_block->terminator()) {
        builder.ret(nullptr);
    }

    current_function = nullptr;
    current_block = nullptr;
}

void BoundToHLIR::visit(BoundPropertyDeclaration* node) {
    // auto prop_sym = node->symbol->as<PropertySymbol>();
    // if (!prop_sym) return;
    
    // if (node->getter && prop_sym->has_getter) {
    //     generate_property_accessor(node, node->getter, true);
    // }
    
    // if (node->setter && prop_sym->has_setter) {
    //     generate_property_accessor(node, node->setter, false);
    // }
}

void BoundToHLIR::generate_property_accessor(
    BoundPropertyDeclaration* prop_decl, 
    BoundPropertyAccessor* accessor,
    bool is_getter) 
{
    // auto prop_sym = prop_decl->symbol->as<PropertySymbol>();
    // if (!prop_sym || !accessor->function_symbol) return;
    
    // auto func = module->find_function(accessor->function_symbol);
    // if (!func) return;
    
    // if (prop_sym->parent && prop_sym->parent->is<TypeSymbol>()) {
    //     auto parent_type = prop_sym->parent->as<TypeSymbol>();
    //     auto this_ptr_type = type_system->get_pointer(parent_type->type);
    //     auto this_param = func->create_value(this_ptr_type, "this");
    //     func->params.push_back(this_param);
    // }
    
    // if (!is_getter) {
    //     auto value_param = func->create_value(prop_sym->type, "value");
    //     func->params.push_back(value_param);
    // }
    
    // auto entry_block = func->create_block("entry");
    // func->entry = entry_block;
    
    // {
    //     ScopedFunctionContext ctx(*this, func, entry_block);
    //     variable_addresses.clear();
    //     lowered.clear();
        
    //     if (accessor->expression) {
    //         auto result = emit_rvalue(accessor->expression);
    //         if (is_getter) {
    //             builder.ret(result);
    //         }
    //     } else if (accessor->body) {
    //         accessor->body->accept(this);
    //     }
        
    //     if (current_block && !current_block->terminator()) {
    //         if (is_getter) {
    //             auto default_val = builder.const_null(prop_sym->type);
    //             builder.ret(default_val);
    //         } else {
    //             builder.ret(nullptr);
    //         }
    //     }
    // }
}

void BoundToHLIR::visit(BoundTypeDeclaration* node) {
    auto type_sym = node->symbol->as<TypeSymbol>();
    if (!type_sym) return;
    
    for (auto member : node->members) {
        member->accept(this);
    }
}

void BoundToHLIR::visit(BoundNamespaceDeclaration* node) {
    // for (auto member : node->members) {
    //     member->accept(this);
    // }
}

void BoundToHLIR::visit(BoundCompilationUnit* node) {
    for (auto stmt : node->statements) {
        stmt->accept(this);
    }
}

#pragma endregion

}