
#include "bound_to_flir.hpp"
#include "ir_type.hpp"
#include <cassert>
#include <iostream>
#include <functional>

namespace Fern::FLIR
{

using IRTypePtr = FLIR::IRTypePtr;

#pragma region Intrinsic Table

using IntrinsicHandler = std::function<FLIR::Value*(BoundToFLIR*, const std::vector<FLIR::Value*>&)>;

struct IntrinsicInfo {
    size_t arity;
    IntrinsicHandler handler;
};

static const std::unordered_map<std::string, IntrinsicInfo> intrinsic_table = {
    {"alloca", {1, [](BoundToFLIR* self, const std::vector<FLIR::Value*>& args) {
        return self->builder.stack_alloc_bytes(args[0]);
    }}},
    {"malloc", {1, [](BoundToFLIR* self, const std::vector<FLIR::Value*>& args) {
        return self->builder.heap_alloc_bytes(args[0]);
    }}},
    {"free", {1, [](BoundToFLIR* self, const std::vector<FLIR::Value*>& args) {
        self->builder.heap_free(args[0]);
        return nullptr;
    }}},
    {"memcpy", {3, [](BoundToFLIR* self, const std::vector<FLIR::Value*>& args) {
        self->builder.memcpy(args[0], args[1], args[2]);
        return args[0];
    }}},
    {"memset", {3, [](BoundToFLIR* self, const std::vector<FLIR::Value*>& args) {
        self->builder.memset(args[0], args[1], args[2]);
        return args[0];
    }}},
};

#pragma endregion

#pragma region Initialization Helpers

static void collect_functions(Symbol* symbol, std::vector<FunctionSymbol*>& functions) {
    if (auto* func_sym = symbol->as<FunctionSymbol>()) {
        functions.push_back(func_sym);
    }
    if (auto* container = symbol->as<ContainerSymbol>()) {
        for (auto [name, child] : *container) {
            collect_functions(child, functions);
        }
    }
}

#pragma endregion

#pragma region Core Infrastructure

void BoundToFLIR::init_module(NamespaceSymbol* global_ns, const std::vector<TypeSymbol*>& sorted_types)
{
    try
    {
        // Pass 1: Define all struct shells (so they exist for forward references)
        for (auto* type_sym : sorted_types)
        {
            module->ir_types.define_struct(type_sym);
        }

        // Pass 2: Calculate field layouts
        for (auto* type_sym : sorted_types)
        {
            auto* ir_struct = module->ir_types.find_struct(type_sym);

            size_t offset = 0;
            size_t max_align = 1;

            for (auto [name, member_ptr] : *type_sym) {
                if (auto* var = member_ptr->as<VariableSymbol>()) {
                    if (!var->is_field()) continue;

                    IRTypePtr field_ir_type = convert(var->type);
                    size_t align = field_ir_type->get_alignment();
                    size_t size = field_ir_type->get_size();

                    offset = (offset + align - 1) & ~(align - 1);

                    ir_struct->fields.push_back({var->name, field_ir_type, offset});
                    offset += size;
                    max_align = std::max(max_align, align);
                }
            }

            if (offset == 0) {
                ir_struct->fields.push_back({"_padding", module->ir_types.get_u8(), offset});
                offset = 1;
                max_align = 1;
            }

            ir_struct->size = (offset + max_align - 1) & ~(max_align - 1);
            ir_struct->alignment = max_align;
        }

        std::vector<FunctionSymbol*> functions;
        collect_functions(global_ns, functions);

        for (auto* func_sym : functions) {
            auto* func = module->create_function(func_sym);
            func->return_type = convert(func_sym->return_type);
            func->is_static = func_sym->is_static();
        }
    }
    catch (const std::exception& ex)
    {
        error(std::string("Error initializing FLIR module: ") + ex.what(), SourceRange());
        std::cout << module->dump() << std::endl;
    }
}

void BoundToFLIR::generate(BoundCompilationUnit* unit)
{
    try
    {
        visit(unit);
    }
    catch (const std::exception& ex)
    {
        error(std::string("Error generating FLIR from bound tree: ") + ex.what(), SourceRange());
        std::cout << module->dump() << std::endl;
    }
}

FLIR::Value* BoundToFLIR::emit_rvalue(BoundExpression* expr) {
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

    return builder.load(lr.result, convert(type));
}

FLIR::Value* BoundToFLIR::emit_lvalue(BoundExpression* expr) {
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

    auto temp = builder.stack_alloc(convert(expr->type));
    builder.store(lr.result, temp);
    return temp;
}

#pragma endregion

#pragma region Helpers

FLIR::Value* BoundToFLIR::get_this_param() {
    if (!current_function || current_function->is_static || current_function->params.empty()) {
        return nullptr;
    }
    return current_function->params[0];
}

void BoundToFLIR::emit_store(FLIR::Value* dest, FLIR::Value* src, FLIR::IRTypePtr type) {
    if (type->is_array() && type->array_size > 0) {
        size_t byte_size = type->get_size();
        auto size_val = builder.const_int(static_cast<int64_t>(byte_size), module->ir_types.get_i32());
        builder.memcpy(dest, src, size_val);
    } else {
        builder.store(src, dest);
    }
}

std::optional<FLIR::Value*> BoundToFLIR::try_pointer_arithmetic(
    BoundBinaryExpression* node,
    FLIR::Value* left,
    FLIR::Value* right)
{
    auto ltype = node->left->type;
    auto rtype = node->right->type;
    bool is_add = node->operatorKind == BinaryOperatorKind::Add;
    bool is_sub = node->operatorKind == BinaryOperatorKind::Subtract;

    if (!is_add && !is_sub) {
        return std::nullopt;
    }

    if (ltype->is<PointerType>() && rtype->is<PrimitiveType>()) {
        auto elem = convert(ltype->as<PointerType>()->pointee);
        FLIR::Value* index = right;
        if (is_sub) {
            index = builder.unary(Opcode::Neg, right);
        }
        return builder.element_addr(left, index, elem);
    }

    if (is_add && rtype->is<PointerType>() && ltype->is<PrimitiveType>()) {
        auto elem = convert(rtype->as<PointerType>()->pointee);
        return builder.element_addr(right, left, elem);
    }

    if (is_sub && ltype->is<PointerType>() && rtype->is<PointerType>()) {
        return builder.binary(Opcode::Sub, left, right);
    }

    return std::nullopt;
}

std::optional<FLIR::Value*> BoundToFLIR::try_emit_intrinsic(
    FunctionSymbol* method,
    const std::vector<FLIR::Value*>& args,
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

size_t BoundToFLIR::get_field_index(TypeSymbol* type_sym, Symbol* field_sym) {
    if (!type_sym || !field_sym) return 0;

    size_t index = 0;
    for (auto [name, member_ptr] : *type_sym) {
        if (member_ptr->is<VariableSymbol>()) {
            if (member_ptr == field_sym) {
                return index;
            }
            index++;
        }
    }
    return 0;
}

FLIR::Opcode BoundToFLIR::get_binary_opcode(BinaryOperatorKind kind) {
    switch (kind) {
        case BinaryOperatorKind::Add: return FLIR::Opcode::Add;
        case BinaryOperatorKind::Subtract: return FLIR::Opcode::Sub;
        case BinaryOperatorKind::Multiply: return FLIR::Opcode::Mul;
        case BinaryOperatorKind::Divide: return FLIR::Opcode::Div;
        case BinaryOperatorKind::Modulo: return FLIR::Opcode::Rem;
        case BinaryOperatorKind::Equals: return FLIR::Opcode::Eq;
        case BinaryOperatorKind::NotEquals: return FLIR::Opcode::Ne;
        case BinaryOperatorKind::LessThan: return FLIR::Opcode::Lt;
        case BinaryOperatorKind::LessThanOrEqual: return FLIR::Opcode::Le;
        case BinaryOperatorKind::GreaterThan: return FLIR::Opcode::Gt;
        case BinaryOperatorKind::GreaterThanOrEqual: return FLIR::Opcode::Ge;
        case BinaryOperatorKind::LogicalAnd: return FLIR::Opcode::And;
        case BinaryOperatorKind::LogicalOr: return FLIR::Opcode::Or;
        case BinaryOperatorKind::BitwiseAnd: return FLIR::Opcode::BitAnd;
        case BinaryOperatorKind::BitwiseOr: return FLIR::Opcode::BitOr;
        case BinaryOperatorKind::BitwiseXor: return FLIR::Opcode::BitXor;
        case BinaryOperatorKind::LeftShift: return FLIR::Opcode::Shl;
        case BinaryOperatorKind::RightShift: return FLIR::Opcode::Shr;
        default: return FLIR::Opcode::Add;
    }
}

FLIR::Opcode BoundToFLIR::get_unary_opcode(UnaryOperatorKind kind) {
    switch (kind) {
        case UnaryOperatorKind::Minus: return FLIR::Opcode::Neg;
        case UnaryOperatorKind::Not: return FLIR::Opcode::Not;
        case UnaryOperatorKind::BitwiseNot: return FLIR::Opcode::BitNot;
        default: return FLIR::Opcode::Neg;
    }
}

FLIR::Opcode BoundToFLIR::get_compound_opcode(AssignmentOperatorKind kind) {
    switch (kind) {
        case AssignmentOperatorKind::Add: return FLIR::Opcode::Add;
        case AssignmentOperatorKind::Subtract: return FLIR::Opcode::Sub;
        case AssignmentOperatorKind::Multiply: return FLIR::Opcode::Mul;
        case AssignmentOperatorKind::Divide: return FLIR::Opcode::Div;
        case AssignmentOperatorKind::Modulo: return FLIR::Opcode::Rem;
        case AssignmentOperatorKind::And: return FLIR::Opcode::BitAnd;
        case AssignmentOperatorKind::Or: return FLIR::Opcode::BitOr;
        case AssignmentOperatorKind::Xor: return FLIR::Opcode::BitXor;
        case AssignmentOperatorKind::LeftShift: return FLIR::Opcode::Shl;
        case AssignmentOperatorKind::RightShift: return FLIR::Opcode::Shr;
        default: return FLIR::Opcode::Add;
    }
}

void BoundToFLIR::emit_string_init(FLIR::Value* string_addr, FLIR::Value* data_ptr, size_t length) {
    auto char_ptr_type = module->ir_types.get_pointer(module->ir_types.get_i8());

    auto data_field = builder.field_addr(string_addr, 0, char_ptr_type, "data");
    builder.store(data_ptr, data_field);

    auto len_val = builder.const_int(static_cast<int64_t>(length), module->ir_types.get_i32());
    auto len_field = builder.field_addr(string_addr, 1, module->ir_types.get_i32(), "length");
    builder.store(len_val, len_field);
}

#pragma endregion

#pragma region Expression Visitors

void BoundToFLIR::visit(BoundLiteralExpression* node) {
    FLIR::Value* result = nullptr;
    auto ir_type = convert(node->type);

    if (std::holds_alternative<int64_t>(node->constantValue)) {
        result = builder.const_int(std::get<int64_t>(node->constantValue), ir_type);
    }
    else if (std::holds_alternative<uint64_t>(node->constantValue)) {
        result = builder.const_int(static_cast<int64_t>(std::get<uint64_t>(node->constantValue)), ir_type);
    }
    else if (std::holds_alternative<bool>(node->constantValue)) {
        result = builder.const_bool(std::get<bool>(node->constantValue), ir_type);
    }
    else if (std::holds_alternative<double>(node->constantValue)) {
        result = builder.const_float(std::get<double>(node->constantValue), ir_type);
    }
    else if (std::holds_alternative<std::string>(node->constantValue))
    {
        const auto& str_val = std::get<std::string>(node->constantValue);

        bool is_string = false;
        if (auto named = node->type->as<NamedType>()) {
            is_string = named->symbol && named->symbol->name == "String";
        }

        if (is_string)
        {
            auto string_addr = builder.smart_alloc(ir_type);
            auto char_ptr_type = module->ir_types.get_pointer(module->ir_types.get_i8());
            auto data_ptr = builder.const_string(str_val, char_ptr_type);
            auto len_val = builder.const_int(static_cast<int64_t>(str_val.length()), module->ir_types.get_i32());

            auto string_ctor = module->find_function_by_name("String.New_char*_i32");
            if (string_ctor) {
                builder.call(string_ctor, {string_addr, data_ptr, len_val});
            } else {
                error("String.New constructor not found", node->location);
            }

            if (node->type->is_reference_type()) {
                result = string_addr;
            } else {
                result = builder.load(string_addr, ir_type);
            }

        } else {
            result = builder.const_string(str_val, ir_type);
        }
    }
    else if (std::holds_alternative<std::monostate>(node->constantValue)) {
        if (node->literalKind == LiteralKind::Null) {
            result = builder.const_null(ir_type);
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

void BoundToFLIR::visit(BoundNameExpression* node) {
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
            auto field_addr = builder.field_addr(this_ptr, field_index, convert(node->type), node->symbol->name);
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
        auto elem_type = convert(node->type->as<ArrayType>()->element);
        auto ptr_type = module->ir_types.get_pointer(elem_type);
        auto loaded_ptr = builder.load(var_addr, ptr_type, node->symbol->name);
        lowered[node] = {loaded_ptr, false};
        return;
    }

    lowered[node] = {var_addr, true};
}

void BoundToFLIR::visit(BoundBinaryExpression* node) {
    // Handle short-circuit evaluation using structured If
    if (node->operatorKind == BinaryOperatorKind::LogicalAnd ||
        node->operatorKind == BinaryOperatorKind::LogicalOr) {

        auto left = emit_rvalue(node->left);
        if (!left) {
            lowered[node] = {nullptr, false};
            return;
        }

        // Allocate a temporary to hold the result
        auto result_slot = builder.stack_alloc(convert(node->type));
        builder.store(left, result_slot);

        // Create If instruction for short-circuit
        auto if_inst = builder.create_if(left);

        // For &&: evaluate right only if left is true
        // For ||: evaluate right only if left is false
        InstructionList* eval_target;
        if (node->operatorKind == BinaryOperatorKind::LogicalAnd) {
            eval_target = &if_inst->then_body;
        } else {
            eval_target = &if_inst->else_body;
        }

        builder.push_target(eval_target);
        auto right = emit_rvalue(node->right);
        if (right) {
            builder.store(right, result_slot);
        }
        builder.pop_target();

        builder.emit_if(std::move(if_inst));

        auto result = builder.load(result_slot, convert(node->type));
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

void BoundToFLIR::visit(BoundUnaryExpression* node) {
    if (node->operatorKind == UnaryOperatorKind::AddressOf) {
        auto addr = emit_lvalue(node->operand);
        if (!addr) {
            lowered[node] = {nullptr, false};
            return;
        }
        lowered[node] = {addr, false};
        return;
    }

    if (node->operatorKind == UnaryOperatorKind::Dereference) {
        auto ptr = emit_rvalue(node->operand);
        if (!ptr) {
            lowered[node] = {nullptr, false};
            return;
        }
        lowered[node] = {ptr, true};
        return;
    }

    auto operand = emit_rvalue(node->operand);
    if (!operand) {
        lowered[node] = {nullptr, false};
        return;
    }

    auto opcode = get_unary_opcode(node->operatorKind);
    auto result = builder.unary(opcode, operand);
    lowered[node] = {result, false};
}

void BoundToFLIR::visit(BoundAssignmentExpression* node) {
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

    FLIR::Value* final_value = rhs_value;
    auto target_ir_type = convert(node->target->type);
    if (node->operatorKind != AssignmentOperatorKind::Assign) {
        auto current_value = builder.load(target_addr, target_ir_type);
        auto opcode = get_compound_opcode(node->operatorKind);
        final_value = builder.binary(opcode, current_value, rhs_value);
    }

    emit_store(target_addr, final_value, target_ir_type);
    lowered[node] = {final_value, false};
}

void BoundToFLIR::visit(BoundCallExpression* node) {
    if (!node->method || !node->method->as<FunctionSymbol>()) {
        lowered[node] = {nullptr, false};
        return;
    }

    auto func_sym = static_cast<FunctionSymbol*>(node->method);

    // Special handling for sizeof intrinsic
    if (func_sym->is_intrinsic && func_sym->name == "sizeof") {
        if (node->arguments.empty()) {
            error("sizeof requires one argument", node->location);
            lowered[node] = {nullptr, false};
            return;
        }

        auto arg_type = node->arguments[0]->type;
        TypePtr type_to_measure = nullptr;

        if (arg_type && arg_type->is<MetaType>()) {
            type_to_measure = arg_type->as<MetaType>()->inner;
        } else {
            type_to_measure = arg_type;
        }

        if (!type_to_measure) {
            error("Cannot determine type for sizeof", node->location);
            lowered[node] = {nullptr, false};
            return;
        }

        auto ir_type_to_measure = convert(type_to_measure);
        size_t size = ir_type_to_measure->get_size();
        auto result = builder.const_int(static_cast<int64_t>(size), module->ir_types.get_i32());
        lowered[node] = {result, false};
        return;
    }

    std::vector<FLIR::Value*> args;

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

    if (auto intrinsic_result = try_emit_intrinsic(func_sym, args, node->location)) {
        lowered[node] = {*intrinsic_result, false};
        return;
    }

    FLIR::Function* func = module->find_function(func_sym);
    if (!func) {
        lowered[node] = {nullptr, false};
        return;
    }

    auto result = builder.call(func, args);
    lowered[node] = {result, false};
}

void BoundToFLIR::visit(BoundMemberAccessExpression* node) {
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

    FLIR::Value* obj_addr;
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
            auto field_addr = builder.field_addr(obj_addr, field_index, convert(var->type), var->name);
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

void BoundToFLIR::visit(BoundIndexExpression* node) {
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

    auto ir_element_type = convert(element_type);
    auto elem_addr = builder.element_addr(obj_val, index_val, ir_element_type);

    lowered[node] = {elem_addr, true};
}

void BoundToFLIR::visit(BoundNewExpression* node) {
    if (!node->type) {
        lowered[node] = {nullptr, false};
        return;
    }

    bool is_value_type = node->type->as<NamedType>() && node->type->is_value_type();

    FLIR::Value* storage;
    if (is_value_type) {
        auto ir_type = convert(node->type);
        storage = builder.stack_alloc(ir_type);
    } else {
        auto named = node->type->as<NamedType>();
        auto struct_type = module->ir_types.get_struct(named->symbol);
        storage = builder.heap_alloc(struct_type);
    }

    if (node->constructor) {
        auto ctor_func = module->find_function(node->constructor);
        if (ctor_func) {
            std::vector<FLIR::Value*> args;
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

void BoundToFLIR::visit(BoundArrayCreationExpression* node) {
    auto alloc_result = builder.stack_alloc(convert(node->type));

    if (!node->initializers.empty()) {
        IRTypePtr ir_element_type = nullptr;
        if (auto array_type = node->type->as<ArrayType>()) {
            ir_element_type = convert(array_type->element);
        }

        if (ir_element_type) {
            auto i32_type = module->ir_types.get_i32();

            for (size_t i = 0; i < node->initializers.size(); i++) {
                auto init_val = emit_rvalue(node->initializers[i]);
                if (init_val) {
                    auto index_val = builder.const_int(static_cast<int64_t>(i), i32_type);
                    auto elem_addr = builder.element_addr(alloc_result, index_val, ir_element_type);
                    builder.store(init_val, elem_addr);
                }
            }
        }
    }

    lowered[node] = {alloc_result, true};
}

void BoundToFLIR::visit(BoundCastExpression* node) {
    auto expr = emit_rvalue(node->expression);
    if (!expr) {
        lowered[node] = {nullptr, false};
        return;
    }

    auto result = builder.convert(expr, convert(node->type));
    lowered[node] = {result, false};
}

void BoundToFLIR::visit(BoundThisExpression* node) {
    auto this_ptr = get_this_param();
    if (!this_ptr) {
        error("'this' used in static or non-member context", node->location);
        lowered[node] = {nullptr, false};
        return;
    }

    bool is_value_type = node->type->as<NamedType>() && node->type->is_value_type();
    lowered[node] = {this_ptr, is_value_type};
}

void BoundToFLIR::visit(BoundConversionExpression* node) {
    auto expr_val = emit_rvalue(node->expression);
    if (!expr_val) {
        lowered[node] = {nullptr, false};
        return;
    }

    if (node->conversionKind != ConversionKind::Identity && node->type) {
        lowered[node] = {builder.convert(expr_val, convert(node->type)), false};
    } else {
        lowered[node] = {expr_val, false};
    }
}

void BoundToFLIR::visit(BoundTypeExpression* node) {
    lowered[node] = {nullptr, false};
}

#pragma endregion

#pragma region Statement Visitors

void BoundToFLIR::visit(BoundBlockStatement* node) {
    for (auto stmt : node->statements) {
        stmt->accept(this);
    }
}

void BoundToFLIR::visit(BoundExpressionStatement* node) {
    emit_rvalue(node->expression);
}

void BoundToFLIR::visit(BoundIfStatement* node) {
    auto cond = emit_rvalue(node->condition);
    if (!cond) return;

    // Create structured IfInst
    auto if_inst = builder.create_if(cond);

    // Emit then-body
    builder.push_target(&if_inst->then_body);
    node->thenStatement->accept(this);
    builder.pop_target();

    // Emit else-body (if present)
    if (node->elseStatement) {
        builder.push_target(&if_inst->else_body);
        node->elseStatement->accept(this);
        builder.pop_target();
    }

    // Add IfInst to current emission target
    builder.emit_if(std::move(if_inst));
}

void BoundToFLIR::visit(BoundWhileStatement* node) {
    auto block_inst = builder.create_block();
    auto loop_inst = builder.create_loop();

    builder.push_target(&block_inst->body);
    builder.push_target(&loop_inst->body);

    auto cond = emit_rvalue(node->condition);
    if (!cond) {
        std::string msg = "While condition evaluated to null";
        if (node->condition && node->condition->type) {
            msg += " (condition type: " + node->condition->type->get_name() + ")";
        }
        error(msg, node->location);
        builder.pop_target();
        builder.pop_target();
        return;
    }

    auto not_cond = builder.unary(Opcode::Not, cond);
    builder.br_if(not_cond, 1);

    int saved_for_depth = for_loop_body_depth;
    for_loop_body_depth = 0;
    node->body->accept(this);
    for_loop_body_depth = saved_for_depth;

    builder.emit_continue();

    builder.pop_target();
    builder.emit_loop(std::move(loop_inst));
    builder.pop_target();
    builder.emit_block(std::move(block_inst));
}

void BoundToFLIR::visit(BoundForStatement* node) {
    if (node->initializer) {
        node->initializer->accept(this);
    }

    auto loop_inst = builder.create_loop();
    auto body_block = builder.create_block();

    builder.push_target(&loop_inst->body);

    if (node->condition) {
        auto cond = emit_rvalue(node->condition);
        if (cond) {
            auto not_cond = builder.unary(Opcode::Not, cond);
            builder.br_if(not_cond, 0);
        }
    }

    builder.push_target(&body_block->body);
    for_loop_body_depth++;
    node->body->accept(this);
    for_loop_body_depth--;
    builder.pop_target();
    builder.emit_block(std::move(body_block));

    for (auto inc : node->incrementors) {
        emit_rvalue(inc);
    }
    builder.emit_continue();

    builder.pop_target();
    builder.emit_loop(std::move(loop_inst));
}

void BoundToFLIR::visit(BoundBreakStatement* node) {
    builder.br(1);
}

void BoundToFLIR::visit(BoundContinueStatement* node) {
    if (for_loop_body_depth > 0) {
        builder.br(0);
    } else {
        builder.emit_continue();
    }
}

void BoundToFLIR::visit(BoundReturnStatement* node) {
    if (node->value) {
        auto val = emit_rvalue(node->value);
        builder.emit_return(val);
    } else {
        builder.emit_return();
    }
}

void BoundToFLIR::visit(BoundUsingStatement* node) {
}

#pragma endregion

#pragma region Declaration Visitors

void BoundToFLIR::visit(BoundVariableDeclaration* node)
{
    auto var_sym = node->symbol->as<VariableSymbol>();
    if (!var_sym) return;

    if (var_sym->is_field())
    {
        return;
    }

    auto var_ir_type = convert(var_sym->type);

    if (var_sym->type->is_reference_type())
    {
        auto ref_ptr = builder.stack_alloc(var_ir_type, node->name);
        variable_addresses[node->symbol] = ref_ptr;

        if (node->initializer) {
            auto init_value = emit_rvalue(node->initializer);
            if (init_value) {
                builder.store(init_value, ref_ptr);
            }
        } else {
            auto null_ptr = builder.const_null(var_ir_type);
            builder.store(null_ptr, ref_ptr);
        }
    } else {
        auto var_addr = builder.stack_alloc(var_ir_type, node->name);
        variable_addresses[node->symbol] = var_addr;

        if (node->initializer) {
            if (auto new_expr = node->initializer->as<BoundNewExpression>()) {
                bool is_new_value_type = new_expr->type &&
                    new_expr->type->as<NamedType>() &&
                    new_expr->type->is_value_type();

                if (is_new_value_type && new_expr->constructor) {
                    auto ctor_func = module->find_function(new_expr->constructor);
                    if (ctor_func) {
                        std::vector<FLIR::Value*> args;
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
                emit_store(var_addr, init_value, var_ir_type);
            }
        } else {
            auto default_value = builder.const_null(var_ir_type);
            builder.store(default_value, var_addr);
        }
    }
}

void BoundToFLIR::visit(BoundFunctionDeclaration* node) {
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
        auto struct_ir_type = module->ir_types.get_struct(parent_type);
        auto this_ptr_type = module->ir_types.get_pointer(struct_ir_type);
        auto this_param = func->create_value(this_ptr_type, "this");
        func->params.push_back(this_param);
        func->this_param = this_param;
    }

    for (size_t i = 0; i < node->parameters.size(); i++) {
        auto param_sym = node->parameters[i]->symbol->as<ParameterSymbol>();
        auto param_ir_type = convert(param_sym->type);
        auto param = func->create_value(param_ir_type, node->parameters[i]->name);
        func->params.push_back(param);
    }

    if (func_sym->is_extern()) {
        current_function = nullptr;
        return;
    }

    // Set up builder with function body as emission target
    builder.set_function(func);

    for (size_t i = 0; i < node->parameters.size(); i++) {
        auto param_sym = node->parameters[i]->symbol->as<ParameterSymbol>();
        size_t param_idx = is_member_function && !func_sym->is_static() ? i + 1 : i;
        auto param_value = func->params[param_idx];

        IRTypePtr alloc_ir_type;
        if (auto array_type = param_sym->type->as<ArrayType>()) {
            alloc_ir_type = module->ir_types.get_pointer(convert(array_type->element));
        } else {
            alloc_ir_type = convert(param_sym->type);
        }

        auto param_addr = builder.stack_alloc(alloc_ir_type, param_sym->name + ".addr");
        builder.store(param_value, param_addr);
        variable_addresses[param_sym] = param_addr;
    }

    if (node->body) {
        node->body->accept(this);
    }

    if (!definitely_terminates(func->body)) {
        builder.emit_return();
    }

    current_function = nullptr;
}

void BoundToFLIR::visit(BoundPropertyDeclaration* node) {
    // Properties not yet implemented
}

void BoundToFLIR::generate_property_accessor(
    BoundPropertyDeclaration* prop_decl,
    BoundPropertyAccessor* accessor,
    bool is_getter)
{
    // Property accessors not yet implemented
}

void BoundToFLIR::visit(BoundTypeDeclaration* node) {
    auto type_sym = node->symbol->as<TypeSymbol>();
    if (!type_sym) return;

    for (auto member : node->members) {
        member->accept(this);
    }
}

void BoundToFLIR::visit(BoundNamespaceDeclaration* node) {
}

void BoundToFLIR::visit(BoundCompilationUnit* node) {
    for (auto stmt : node->statements) {
        stmt->accept(this);
    }
}

#pragma endregion

}
