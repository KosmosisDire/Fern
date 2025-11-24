// bound_to_hlir.cpp
#include "bound_to_hlir.hpp"
#include <cassert>
#include <iostream>
namespace Fern::HLIR
{
#pragma region Public Methods
void BoundToHLIR::build(BoundCompilationUnit* unit) {
    visit(unit);
}

size_t BoundToHLIR::get_field_index(TypeSymbol* type_sym, Symbol* field_sym) {
    if (!type_sym || !field_sym) return 0;

    size_t index = 0;
    for (auto* member : type_sym->member_order) {
        if (member->is<FieldSymbol>() || member->is<VariableSymbol>()) {
            if (member == field_sym) {
                return index;
            }
            index++;
        }
    }
    return 0;
}

#pragma region Core Expressions

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
    else if (std::holds_alternative<std::string>(node->constantValue)) {
        result = builder.const_string(std::get<std::string>(node->constantValue), node->type);
    }
    else if (std::holds_alternative<std::monostate>(node->constantValue)) {
        error("Literal expression has no constant value (type: " +
              (node->type ? node->type->get_name() : "null") + ")", node->location);
    }
    else {
        error("Literal expression has unknown variant type (index: " +
              std::to_string(node->constantValue.index()) + ", type: " +
              (node->type ? node->type->get_name() : "null") + ")", node->location);
    }

    expression_values[node] = result;
}

void BoundToHLIR::visit(BoundNameExpression* node) {
    if (!node->symbol) {
        expression_values[node] = nullptr;
        return;
    }

    // Field member access
    if (node->symbol->is<FieldSymbol>() || node->symbol->is<VariableSymbol>()) {
        auto parent = node->symbol->parent;
        if (parent && parent->is<TypeSymbol>()) {
            // Field of a type - need 'this' pointer
            if (current_function && !current_function->is_static &&
                current_function->params.size() > 0) {

                auto this_param = current_function->params[0];
                size_t field_index = get_field_index(parent->as<TypeSymbol>(), node->symbol);
                auto field_addr = builder.field_addr(this_param, field_index, node->type, node->symbol->name);
                auto field_value = builder.load(field_addr, node->type, node->symbol->name);
                expression_values[node] = field_value;
                return;
            }
        }
    }

    // Regular local variable/parameter - just load it
    auto var_addr = variable_addresses[node->symbol];
    if (!var_addr) {
        error("Undefined variable: " + node->symbol->name, node->location);
        expression_values[node] = nullptr;
        return;
    }

    auto value = builder.load(var_addr, node->type, node->symbol->name);
    expression_values[node] = value;
}

void BoundToHLIR::visit(BoundBinaryExpression* node) {
    auto left = evaluate_expression(node->left);
    auto right = evaluate_expression(node->right);

    if (!left || !right) {
        std::string msg = "Null operand in binary expression";
        if (!left && node->left && node->left->type) {
            msg += " (left operand type: " + node->left->type->get_name() + ")";
        }
        if (!right && node->right && node->right->type) {
            msg += " (right operand type: " + node->right->type->get_name() + ")";
        }
        error(msg, node->location);
        expression_values[node] = nullptr;
        return;
    }

    // Handle pointer arithmetic specially
    if ((node->operatorKind == BinaryOperatorKind::Add ||
         node->operatorKind == BinaryOperatorKind::Subtract) &&
        node->left->type && node->right->type) {

        // Check for pointer + integer
        if (node->left->type->is<PointerType>() && node->right->type->is<PrimitiveType>()) {
            auto ptr_type = node->left->type->as<PointerType>();
            auto element_type = ptr_type->pointee;

            if (node->operatorKind == BinaryOperatorKind::Add) {
                // pointer + integer: use ElementAddr
                auto result = builder.element_addr(left, right, element_type);
                expression_values[node] = result;
                return;
            } else {
                // pointer - integer: negate the index and use ElementAddr
                auto neg_right = builder.unary(Opcode::Neg, right);
                auto result = builder.element_addr(left, neg_right, element_type);
                expression_values[node] = result;
                return;
            }
        }

        // Check for integer + pointer (only for addition)
        if (node->operatorKind == BinaryOperatorKind::Add &&
            node->right->type->is<PointerType>() && node->left->type->is<PrimitiveType>()) {
            auto ptr_type = node->right->type->as<PointerType>();
            auto element_type = ptr_type->pointee;

            // integer + pointer: use ElementAddr (swap operands)
            auto result = builder.element_addr(right, left, element_type);
            expression_values[node] = result;
            return;
        }

        // Check for pointer - pointer
        if (node->operatorKind == BinaryOperatorKind::Subtract &&
            node->left->type->is<PointerType>() && node->right->type->is<PointerType>()) {
            // pointer - pointer: convert pointers to integers, subtract, divide by element size
            // For now, we'll use a simple subtraction - proper implementation would need ptrtoint
            // This is a placeholder that will need proper implementation in codegen
            auto opcode = get_binary_opcode(node->operatorKind);
            auto result = builder.binary(opcode, left, right);
            expression_values[node] = result;
            return;
        }
    }

    // Regular binary operation
    auto opcode = get_binary_opcode(node->operatorKind);
    auto result = builder.binary(opcode, left, right);
    expression_values[node] = result;
}

void BoundToHLIR::visit(BoundUnaryExpression* node) {
    auto operand = evaluate_expression(node->operand);
    if (!operand) {
        expression_values[node] = nullptr;
        return;
    }
    
    auto opcode = get_unary_opcode(node->operatorKind);
    auto result = builder.unary(opcode, operand);
    expression_values[node] = result;
}

void BoundToHLIR::visit(BoundAssignmentExpression* node) {
    auto rhs_value = evaluate_expression(node->value);
    if (!rhs_value) {
        expression_values[node] = nullptr;
        return;
    }

    // Get target address (lvalue)
    auto target_addr = get_lvalue_address(node->target);
    if (!target_addr) {
        error("Invalid assignment target", node->location);
        expression_values[node] = nullptr;
        return;
    }

    // Handle compound assignments
    HLIR::Value* final_value = rhs_value;
    if (node->operatorKind != AssignmentOperatorKind::Assign) {
        auto current_value = builder.load(target_addr, node->target->type);
        auto opcode = get_compound_opcode(node->operatorKind);
        final_value = builder.binary(opcode, current_value, rhs_value);
    }

    // Store result
    builder.store(final_value, target_addr);
    expression_values[node] = final_value;
}

void BoundToHLIR::visit(BoundCallExpression* node) {
    std::vector<HLIR::Value*> args;

    // For method calls, add 'this' as first argument
    if (auto member_expr = node->callee->as<BoundMemberAccessExpression>()) {
        // Get the address of the object for 'this'
        auto this_addr = get_lvalue_address(member_expr->object);
        
        if (!this_addr) {
            // Fallback: evaluate as rvalue
            auto this_val = evaluate_expression(member_expr->object);
            if (this_val) {
                args.push_back(this_val);
            }
        } else {
            // For value types, pass the address
            // For reference types, load the pointer first
            bool is_value_type = member_expr->object->type->as<NamedType>() && 
                                member_expr->object->type->is_value_type();
            
            if (is_value_type) {
                // Pass address directly
                args.push_back(this_addr);
            } else {
                // Load pointer and pass it
                auto this_ptr = builder.load(this_addr, member_expr->object->type);
                args.push_back(this_ptr);
            }
        }
    }

    // Add regular arguments
    for (auto arg : node->arguments) {
        auto arg_val = evaluate_expression(arg);
        if (arg_val) {
            args.push_back(arg_val);
        }
    }

    if (!node->method || !node->method->as<FunctionSymbol>()) {
        expression_values[node] = nullptr;
        return;
    }

    auto func_sym = static_cast<FunctionSymbol*>(node->method);
    HLIR::Function* func = module->find_function(func_sym);
    if (!func) {
        expression_values[node] = nullptr;
        return;
    }

    auto result = builder.call(func, args);
    expression_values[node] = result;
}

void BoundToHLIR::visit(BoundMemberAccessExpression* node) {
    if (!node->member) {
        expression_values[node] = nullptr;
        return;
    }

    // Evaluate object - get its address
    auto obj_addr = get_lvalue_address(node->object);
    
    if (!obj_addr) {
        // Fallback: evaluate as rvalue
        auto obj_val = evaluate_expression(node->object);
        if (!obj_val) {
            expression_values[node] = nullptr;
            return;
        }
        
        // For rvalue, we need to materialize it
        bool is_value_type = node->object->type->as<NamedType>() && 
                            node->object->type->is_value_type();
        if (is_value_type) {
            // Materialize into temporary storage
            auto temp = builder.alloc(node->object->type, /*stack=*/true);
            builder.store(obj_val, temp);
            obj_addr = temp;
        } else {
            // For reference types, obj_val is already a pointer
            obj_addr = obj_val;
        }
    } else {
        // We have the address - for reference types, load the pointer
        bool is_value_type = node->object->type->as<NamedType>() && 
                            node->object->type->is_value_type();
        if (!is_value_type) {
            // Load the pointer for reference types
            obj_addr = builder.load(obj_addr, node->object->type);
        }
    }

    // Field access - get address and load
    if (auto field = node->member->as<FieldSymbol>()) {
        size_t field_index = get_field_index(field->parent->as<TypeSymbol>(), field);
        auto field_addr = builder.field_addr(obj_addr, field_index, field->type, field->name);
        auto field_value = builder.load(field_addr, field->type, field->name);
        expression_values[node] = field_value;
        return;
    }

    // Variable member
    if (auto var = node->member->as<VariableSymbol>()) {
        if (obj_addr) {
            size_t field_index = get_field_index(var->parent->as<TypeSymbol>(), var);
            auto field_addr = builder.field_addr(obj_addr, field_index, var->type, var->name);
            auto field_value = builder.load(field_addr, var->type, var->name);
            expression_values[node] = field_value;
        } else {
            // Static member
            auto var_addr = variable_addresses[var];
            if (var_addr) {
                auto value = builder.load(var_addr, var->type, var->name);
                expression_values[node] = value;
            } else {
                expression_values[node] = nullptr;
            }
        }
        return;
    }
    
    // Method reference - store object address for call site
    if (node->member->as<FunctionSymbol>()) {
        expression_values[node] = obj_addr;
        return;
    }
    
    // Property - TODO
    expression_values[node] = nullptr;
}

void BoundToHLIR::visit(BoundIndexExpression* node) {
    auto obj_val = evaluate_expression(node->object);
    auto index_val = evaluate_expression(node->index);

    if (!obj_val || !index_val) {
        expression_values[node] = nullptr;
        return;
    }

    // Get element type
    TypePtr element_type = nullptr;
    if (auto array_type = node->object->type->as<ArrayType>()) {
        element_type = array_type->element;
    } else if (auto ptr_type = node->object->type->as<PointerType>()) {
        element_type = ptr_type->pointee;
    }

    if (!element_type) {
        error("Cannot index non-array/non-pointer type", node->location);
        expression_values[node] = nullptr;
        return;
    }

    // Get element address and load value
    auto elem_addr = builder.element_addr(obj_val, index_val, element_type);
    auto elem_value = builder.load(elem_addr, element_type);
    expression_values[node] = elem_value;
}

void BoundToHLIR::visit(BoundNewExpression* node) {
    if (!node->type) {
        expression_values[node] = nullptr;
        return;
    }

    bool is_value_type = node->type->as<NamedType>() && node->type->is_value_type();
    
    // Allocate storage
    auto storage = builder.alloc(node->type, /*stack=*/is_value_type);
    
    // Call constructor if present
    if (node->constructor) {
        auto ctor_func = module->find_function(node->constructor);
        if (ctor_func) {
            std::vector<HLIR::Value*> args;
            args.push_back(storage);  // 'this'
            
            for (auto arg : node->arguments) {
                auto arg_val = evaluate_expression(arg);
                if (arg_val) {
                    args.push_back(arg_val);
                }
            }
            
            builder.call(ctor_func, args);
        }
    }
    
    // Return appropriate value
    if (is_value_type) {
        // For value types: load and return the value
        auto value = builder.load(storage, node->type);
        expression_values[node] = value;
    } else {
        // For reference types: return the pointer
        expression_values[node] = storage;
    }
}

void BoundToHLIR::visit(BoundArrayCreationExpression* node) {
    auto alloc_result = builder.alloc(node->type, /*stack=*/true);

    // Initialize elements
    if (!node->initializers.empty()) {
        TypePtr element_type = nullptr;
        if (auto array_type = node->type->as<ArrayType>()) {
            element_type = array_type->element;
        }

        if (element_type) {
            auto i32_type = type_system->get_primitive("i32");
            
            for (size_t i = 0; i < node->initializers.size(); i++) {
                auto init_val = evaluate_expression(node->initializers[i]);
                if (init_val) {
                    auto index_val = builder.const_int(static_cast<int64_t>(i), i32_type);
                    auto elem_addr = builder.element_addr(alloc_result, index_val, element_type);
                    builder.store(init_val, elem_addr);
                }
            }
        }
    }

    expression_values[node] = alloc_result;
}

void BoundToHLIR::visit(BoundCastExpression* node) {
    auto expr = evaluate_expression(node->expression);
    if (!expr) {
        expression_values[node] = nullptr;
        return;
    }
    
    auto result = builder.cast(expr, node->type);
    expression_values[node] = result;
}

void BoundToHLIR::visit(BoundThisExpression* node) {
    if (!current_function || current_function->is_static) {
        error("'this' used in static or non-member context", node->location);
        expression_values[node] = nullptr;
        return;
    }

    if (current_function->params.empty()) {
        error("'this' parameter missing from function", node->location);
        expression_values[node] = nullptr;
        return;
    }

    expression_values[node] = current_function->params[0];
}

void BoundToHLIR::visit(BoundParenthesizedExpression* node) {
    expression_values[node] = evaluate_expression(node->expression);
}

void BoundToHLIR::visit(BoundConversionExpression* node) {
    auto expr_val = evaluate_expression(node->expression);
    if (!expr_val) {
        expression_values[node] = nullptr;
        return;
    }

    // Only emit cast if conversion is not identity
    if (node->conversionKind != ConversionKind::Identity && node->type) {
        expression_values[node] = builder.cast(expr_val, node->type);
    } else {
        expression_values[node] = expr_val;
    }
}

void BoundToHLIR::visit(BoundTypeExpression* node) {
    expression_values[node] = nullptr;
}

#pragma region Core Statements

void BoundToHLIR::visit(BoundBlockStatement* node) {
    for (auto stmt : node->statements) {
        stmt->accept(this);
    }
}

void BoundToHLIR::visit(BoundExpressionStatement* node) {
    evaluate_expression(node->expression);
}

void BoundToHLIR::visit(BoundIfStatement* node) {
    auto cond = evaluate_expression(node->condition);
    if (!cond) return;

    auto then_block = create_block("if.then");
    auto else_block = create_block("if.else");
    HLIR::BasicBlock* merge_block = nullptr;

    builder.cond_br(cond, then_block, else_block);

    // Then branch
    builder.set_block(then_block);
    current_block = then_block;
    node->thenStatement->accept(this);
    // Terminated if: current_block is null OR has a terminator
    bool then_terminated = !current_block || current_block->terminator();
    if (!then_terminated) {
        if (!merge_block) merge_block = create_block("if.merge");
        builder.set_block(current_block);
        builder.br(merge_block);
    }

    // Else branch
    builder.set_block(else_block);
    current_block = else_block;
    bool else_terminated = false;
    if (node->elseStatement) {
        node->elseStatement->accept(this);
        // Terminated if: current_block is null OR has a terminator
        else_terminated = !current_block || current_block->terminator();
    }
    if (!else_terminated) {
        if (!merge_block) merge_block = create_block("if.merge");
        builder.set_block(current_block);
        builder.br(merge_block);
    }

    // Continue to merge if it was created
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

    auto cond = evaluate_expression(node->condition);
    if (!cond) {
        std::string msg = "While condition evaluated to null";
        if (node->condition && node->condition->type) {
            msg += " (condition type: " + node->condition->type->get_name() + ")";
        }
        error(msg, node->location);
        // Early exit: branch to exit and skip body generation
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
    if (current_block && !current_block->terminator()) {
        builder.set_block(current_block);
        builder.br(header);
    }

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
        auto cond = evaluate_expression(node->condition);
        if (cond) {
            builder.cond_br(cond, body, exit);
        }
    } else {
        builder.br(body);
    }

    builder.set_block(body);
    current_block = body;
    node->body->accept(this);
    if (current_block && !current_block->terminator()) {
        builder.set_block(current_block);
        builder.br(update);
    }

    builder.set_block(update);
    current_block = update;
    for (auto inc : node->incrementors) {
        evaluate_expression(inc);
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
        auto val = evaluate_expression(node->value);
        builder.ret(val);
    } else {
        builder.ret(nullptr);
    }
}

void BoundToHLIR::visit(BoundUsingStatement* node) {
    // TODO
}

#pragma region Declarations

void BoundToHLIR::visit(BoundVariableDeclaration* node) {
    if (node->symbol && node->symbol->is<FieldSymbol>()) {
        return;
    }

    auto var_sym = node->symbol->as<VariableSymbol>();
    if (!var_sym) return;

    if (var_sym->type->is_reference_type()) {
        auto ref_ptr = builder.alloc_nested(var_sym->type, true, node->name);
        variable_addresses[node->symbol] = ref_ptr;

        if (node->initializer) {
            auto init_value = evaluate_expression(node->initializer);
            if (init_value && init_value->type->as<PointerType>()) {
                builder.store(init_value, ref_ptr);
            } else {
                throw std::runtime_error("Reference variable must be initialized to a pointer");
            }
        } else {
            auto null_ptr = builder.const_null(var_sym->type);
            builder.store(null_ptr, ref_ptr);
        }
    } else {
        // Value type
        auto var_addr = builder.alloc(var_sym->type, true, node->name);
        variable_addresses[node->symbol] = var_addr;

        if (node->initializer) {
            // Optimization: if initializer is a new expression for a value type,
            // call the constructor directly with var_addr as 'this' to avoid
            // creating a temporary allocation and copying
            if (auto new_expr = node->initializer->as<BoundNewExpression>()) {
                bool is_new_value_type = new_expr->type &&
                    new_expr->type->as<NamedType>() &&
                    new_expr->type->is_value_type();

                if (is_new_value_type) {
                    emit_constructor_to_address(new_expr, var_addr);
                    return;
                }
            }

            // Default path: evaluate initializer and store
            auto init_value = evaluate_expression(node->initializer);
            if (init_value) {
                builder.store(init_value, var_addr);
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

    func->is_external = func_sym->isExtern;

    // Clear state
    variable_addresses.clear();
    expression_values.clear();
    current_function = func;

    bool is_member_function = func_sym->parent && func_sym->parent->is<TypeSymbol>();

    // Add 'this' parameter for member functions
    if (is_member_function && !func_sym->isStatic) {
        auto parent_type = func_sym->parent->as<TypeSymbol>();
        auto this_ptr_type = type_system->get_pointer(parent_type->type);
        auto this_param = func->create_value(this_ptr_type, "this");
        func->params.push_back(this_param);
    }

    // Add parameters
    for (size_t i = 0; i < node->parameters.size(); i++) {
        auto param_sym = node->parameters[i]->symbol->as<ParameterSymbol>();
        auto param = func->create_value(param_sym->type, node->parameters[i]->name);
        func->params.push_back(param);
    }

    // Skip body for external functions
    if (func_sym->isExtern) {
        current_function = nullptr;
        return;
    }

    // Create entry block
    auto entry = func->create_block("entry");
    func->entry = entry;
    current_block = entry;
    builder.set_function(func);
    builder.set_block(entry);

    // Allocate stack space for parameters (mem2reg will promote to SSA)
    for (size_t i = 0; i < node->parameters.size(); i++) {
        auto param_sym = node->parameters[i]->symbol->as<ParameterSymbol>();
        size_t param_idx = is_member_function && !func_sym->isStatic ? i + 1 : i;
        auto param_value = func->params[param_idx];

        // For array parameters (which are really pointers), allocate space for the pointer
        TypePtr alloc_type = param_sym->type;
        if (auto array_type = param_sym->type->as<ArrayType>()) {
            alloc_type = type_system->get_pointer(array_type->element);
        }

        auto param_addr = builder.alloc(alloc_type, /*stack=*/true, param_sym->name + ".addr");
        builder.store(param_value, param_addr);
        variable_addresses[param_sym] = param_addr;
    }

    // Process body
    if (node->body) {
        node->body->accept(this);
    }

    // Add implicit return
    if (current_block && !current_block->terminator()) {
        builder.ret(nullptr);
    }

    current_function = nullptr;
    current_block = nullptr;
}

void BoundToHLIR::visit(BoundPropertyDeclaration* node) {
    auto prop_sym = node->symbol->as<PropertySymbol>();
    if (!prop_sym) return;
    
    if (node->getter && prop_sym->has_getter) {
        generate_property_getter(node, node->getter);
    }
    
    if (node->setter && prop_sym->has_setter) {
        generate_property_setter(node, node->setter);
    }
}

void BoundToHLIR::visit(BoundTypeDeclaration* node) {
    auto type_sym = node->symbol->as<TypeSymbol>();
    if (!type_sym) return;
    
    for (auto member : node->members) {
        member->accept(this);
    }
}

void BoundToHLIR::visit(BoundNamespaceDeclaration* node) {
    for (auto member : node->members) {
        member->accept(this);
    }
}

void BoundToHLIR::visit(BoundCompilationUnit* node) {
    for (auto stmt : node->statements) {
        stmt->accept(this);
    }
}

#pragma region Helper Methods

HLIR::Value* BoundToHLIR::evaluate_expression(BoundExpression* expr) {
    if (!expr) return nullptr;
    expr->accept(this);
    return expression_values[expr];
}

HLIR::Value* BoundToHLIR::get_lvalue_address(BoundExpression* expr)
{
    if (auto this_expr = expr->as<BoundThisExpression>()) {
        if (current_function && !current_function->is_static && 
            !current_function->params.empty()) {
            return current_function->params[0];  // 'this' is already a pointer
        }
        return nullptr;
    }

    if (auto name = expr->as<BoundNameExpression>()) {
        // Check for field access first
        if (name->symbol && (name->symbol->is<FieldSymbol>() || name->symbol->is<VariableSymbol>())) {
            auto parent = name->symbol->parent;
            if (parent && parent->is<TypeSymbol>()) {
                // Field of a type - need 'this' pointer
                if (current_function && !current_function->is_static && 
                    current_function->params.size() > 0) {
                    auto this_param = current_function->params[0];
                    size_t field_index = get_field_index(parent->as<TypeSymbol>(), name->symbol);
                    return builder.field_addr(this_param, field_index, name->type);
                }
            }
        }
        
        // Regular local variable
        return variable_addresses[name->symbol];
    }
    
    if (auto member = expr->as<BoundMemberAccessExpression>()) {
        // Get object address first
        auto obj_addr = get_lvalue_address(member->object);
        if (!obj_addr) {
            // Try evaluating as rvalue
            auto obj_val = evaluate_expression(member->object);
            if (!obj_val) return nullptr;
            
            // For value types, materialize
            bool is_value_type = member->object->type->as<NamedType>() && 
                                member->object->type->is_value_type();
            if (is_value_type) {
                auto temp = builder.alloc(member->object->type, /*stack=*/true);
                builder.store(obj_val, temp);
                obj_addr = temp;
            } else {
                obj_addr = obj_val;
            }
        } else {
            // For reference types, load the pointer
            bool is_value_type = member->object->type->as<NamedType>() && 
                                member->object->type->is_value_type();
            if (!is_value_type) {
                obj_addr = builder.load(obj_addr, member->object->type);
            }
        }
        
        if (!member->member) return nullptr;
        
        size_t field_index = get_field_index(
            member->member->parent->as<TypeSymbol>(),
            member->member
        );
        return builder.field_addr(obj_addr, field_index, member->type);
    }
    
    if (auto index = expr->as<BoundIndexExpression>()) {
        auto obj_val = evaluate_expression(index->object);
        auto index_val = evaluate_expression(index->index);
        if (!obj_val || !index_val) return nullptr;
        
        TypePtr element_type = nullptr;
        if (auto array_type = index->object->type->as<ArrayType>()) {
            element_type = array_type->element;
        } else if (auto ptr_type = index->object->type->as<PointerType>()) {
            element_type = ptr_type->pointee;
        }
        
        if (!element_type) return nullptr;
        return builder.element_addr(obj_val, index_val, element_type);
    }
    
    return nullptr;
}

HLIR::BasicBlock* BoundToHLIR::create_block(const std::string& name) {
    auto block = current_function->create_block(name);
    return block;
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
        case BinaryOperatorKind::LeftShift: return HLIR::Opcode::Shl;
        case BinaryOperatorKind::RightShift: return HLIR::Opcode::Shr;
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
        case AssignmentOperatorKind::LeftShift: return HLIR::Opcode::Shl;
        case AssignmentOperatorKind::RightShift: return HLIR::Opcode::Shr;
        default: return HLIR::Opcode::Add;
    }
}

void BoundToHLIR::emit_constructor_to_address(BoundNewExpression* new_expr, HLIR::Value* dest_addr) {
    // Call constructor directly with dest_addr as 'this', avoiding temporary allocation
    if (new_expr->constructor) {
        auto ctor_func = module->find_function(new_expr->constructor);
        if (ctor_func) {
            std::vector<HLIR::Value*> args;
            args.push_back(dest_addr);  // 'this' pointer

            for (auto arg : new_expr->arguments) {
                auto arg_val = evaluate_expression(arg);
                if (arg_val) {
                    args.push_back(arg_val);
                }
            }

            builder.call(ctor_func, args);
        }
    }
    // No need to load/return - the constructor writes directly to dest_addr
}

void BoundToHLIR::generate_property_getter(BoundPropertyDeclaration* prop_decl, BoundPropertyAccessor* getter) {
    auto prop_sym = prop_decl->symbol->as<PropertySymbol>();
    if (!prop_sym || !getter->function_symbol) return;
    
    auto getter_func = module->find_function(getter->function_symbol);
    if (!getter_func) return;
    
    // Add 'this' parameter
    if (prop_sym->parent && prop_sym->parent->is<TypeSymbol>()) {
        auto parent_type = prop_sym->parent->as<TypeSymbol>();
        auto this_ptr_type = type_system->get_pointer(parent_type->type);
        auto this_param = getter_func->create_value(this_ptr_type, "this");
        getter_func->params.push_back(this_param);
    }
    
    auto entry_block = getter_func->create_block("entry");
    getter_func->entry = entry_block;
    
    // Save context
    auto prev_function = current_function;
    auto prev_block = current_block;
    current_function = getter_func;
    current_block = entry_block;
    builder.set_function(getter_func);
    builder.set_block(entry_block);
    
    // Generate body
    if (getter->expression) {
        auto result = evaluate_expression(getter->expression);
        builder.ret(result);
    } else if (getter->body) {
        getter->body->accept(this);
        if (current_block && !current_block->terminator()) {
            builder.ret(nullptr);
        }
    } else {
        auto default_val = builder.const_null(prop_sym->type);
        builder.ret(default_val);
    }
    
    // Restore context
    current_function = prev_function;
    current_block = prev_block;
    if (prev_function) {
        builder.set_function(prev_function);
        if (prev_block) {
            builder.set_block(prev_block);
        }
    }
}

void BoundToHLIR::generate_property_setter(BoundPropertyDeclaration* prop_decl, BoundPropertyAccessor* setter) {
    auto prop_sym = prop_decl->symbol->as<PropertySymbol>();
    if (!prop_sym || !setter->function_symbol) return;
    
    auto setter_func = module->find_function(setter->function_symbol);
    if (!setter_func) return;
    
    // Add 'this' parameter
    if (prop_sym->parent && prop_sym->parent->is<TypeSymbol>()) {
        auto parent_type = prop_sym->parent->as<TypeSymbol>();
        auto this_ptr_type = type_system->get_pointer(parent_type->type);
        auto this_param = setter_func->create_value(this_ptr_type, "this");
        setter_func->params.push_back(this_param);
    }
    
    // Add 'value' parameter
    auto value_param = setter_func->create_value(prop_sym->type, "value");
    setter_func->params.push_back(value_param);
    
    auto entry_block = setter_func->create_block("entry");
    setter_func->entry = entry_block;
    
    // Save context
    auto prev_function = current_function;
    auto prev_block = current_block;
    current_function = setter_func;
    current_block = entry_block;
    builder.set_function(setter_func);
    builder.set_block(entry_block);
    
    // Generate body
    if (setter->expression) {
        evaluate_expression(setter->expression);
    } else if (setter->body) {
        setter->body->accept(this);
    }
    
    if (current_block && !current_block->terminator()) {
        builder.ret(nullptr);
    }
    
    // Restore context
    current_function = prev_function;
    current_block = prev_block;
    if (prev_function) {
        builder.set_function(prev_function);
        if (prev_block) {
            builder.set_block(prev_block);
        }
    }
}
}
