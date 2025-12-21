#include "bound_tree_builder.hpp"
#include <iostream>

namespace Fern
{
    BoundTreeBuilder::BoundTreeBuilder(BindingArena& arena, SymbolTable &symbol_table)
        : DiagnosticSystem("Binder"), arena_(arena), symbol_table_(symbol_table) {}

    BoundCompilationUnit *BoundTreeBuilder::bind(CompilationUnitSyntax *syntax)
    {
        auto unit = arena_.make<BoundCompilationUnit>();
        unit->location = syntax->location;

        ScopeGuard scope(symbol_table_, symbol_table_.get_global_namespace());

        for (auto stmt : syntax->topLevelStatements)
        {
            if (stmt)
            {
                if (auto bound = bind_statement(stmt))
                {
                    unit->statements.push_back(bound);
                }
            }
        }

        return unit;
    }

#pragma region Stmnts and Decls

    BoundStatement *BoundTreeBuilder::bind_statement(BaseStmtSyntax *syntax)
    {
        if (!syntax)
            return nullptr;

        // Declarations
        if (auto func_decl = syntax->as<FunctionDeclSyntax>())
            return bind_function_declaration(func_decl);
        if (auto ctor_decl = syntax->as<ConstructorDeclSyntax>())
            return bind_constructor_declaration(ctor_decl);
        if (auto type_decl = syntax->as<TypeDeclSyntax>())
            return bind_type_declaration(type_decl);
        if (auto var_decl = syntax->as<VariableDeclSyntax>())
            return bind_variable_declaration(var_decl);
        if (auto namespace_decl = syntax->as<NamespaceDeclSyntax>())
            return bind_namespace_declaration(namespace_decl);
        if (auto prop_decl = syntax->as<PropertyDeclSyntax>())
            return bind_property_declaration(prop_decl);
        if (auto using_decl = syntax->as<UsingDirectiveSyntax>())
            return bind_using_statement(using_decl);

        // Statements
        if (auto block = syntax->as<BlockSyntax>())
            return bind_block(block);
        if (auto if_stmt = syntax->as<IfStmtSyntax>())
            return bind_if_statement(if_stmt);
        if (auto while_stmt = syntax->as<WhileStmtSyntax>())
            return bind_while_statement(while_stmt);
        if (auto for_stmt = syntax->as<ForStmtSyntax>())
            return bind_for_statement(for_stmt);
        if (auto return_stmt = syntax->as<ReturnStmtSyntax>())
            return bind_return_statement(return_stmt);
        if (auto break_stmt = syntax->as<BreakStmtSyntax>())
            return bind_break_statement(break_stmt);
        if (auto continue_stmt = syntax->as<ContinueStmtSyntax>())
            return bind_continue_statement(continue_stmt);
        if (auto expr_stmt = syntax->as<ExpressionStmtSyntax>())
            return bind_expression_statement(expr_stmt);

        return nullptr;
    }

    BoundBlockStatement *BoundTreeBuilder::bind_block(BlockSyntax *syntax)
    {
        auto bound = arena_.make<BoundBlockStatement>();
        bound->location = syntax->location;
        bound->symbol = symbol_table_.get_symbol_for_ast(syntax);

        ScopeGuard scope(symbol_table_, bound->symbol);

        for (auto stmt : syntax->statements)
        {
            if (auto bound_stmt = bind_statement(stmt))
            {
                bound->statements.push_back(bound_stmt);
            }
        }

        return bound;
    }

    BoundVariableDeclaration *BoundTreeBuilder::bind_variable_declaration(VariableDeclSyntax *syntax)
    {
        auto bound = arena_.make<BoundVariableDeclaration>();
        bound->location = syntax->location;
        bound->modifiers = syntax->modifiers;

        if (syntax->variable && syntax->variable->name)
        {
            bound->name = syntax->variable->name->get_name();
        }

        if (syntax->variable && syntax->variable->type)
        {
            bound->typeExpression = bind_type_expression(syntax->variable->type);
        }

        bound->symbol = symbol_table_.resolve_local(bound->name);

        ScopeGuard scope(symbol_table_, bound->symbol);

        if (syntax->initializer)
        {
            bound->initializer = bind_expression(syntax->initializer);
        }

        bound->isField = has_flag(bound->modifiers, ModifierKindFlags::Static) ||
                         has_flag(bound->modifiers, ModifierKindFlags::Private) ||
                         has_flag(bound->modifiers, ModifierKindFlags::Public);
        bound->isLocal = !bound->isField && !bound->isParameter;

        return bound;
    }

    BoundFunctionDeclaration *BoundTreeBuilder::bind_function_declaration(FunctionDeclSyntax *syntax)
    {
        auto bound = arena_.make<BoundFunctionDeclaration>();
        bound->location = syntax->location;
        bound->modifiers = syntax->modifiers;

        if (syntax->name)
        {
            bound->name = syntax->name->get_name();
        }

        bound->symbol = resolve_symbol({bound->name});

        ScopeGuard scope(symbol_table_, bound->symbol);

        if (syntax->returnType)
        {
            bound->returnTypeExpression = bind_type_expression(syntax->returnType);
        }

        for (auto param : syntax->parameters)
        {
            if (auto param_syntax = param->as<ParameterDeclSyntax>())
            {
                auto bound_param = arena_.make<BoundVariableDeclaration>();
                bound_param->location = param_syntax->location;
                bound_param->name = param_syntax->param->name ? param_syntax->param->name->get_name() : "";
                bound_param->typeExpression = param_syntax->param->type ? bind_type_expression(param_syntax->param->type) : nullptr;
                bound_param->isParameter = true;
                bound_param->symbol = symbol_table_.resolve_local(bound_param->name);

                bound->parameters.push_back(bound_param);
            }
        }

        if (syntax->body)
        {
            bound->body = bind_block(syntax->body);
        }

        return bound;
    }

    BoundFunctionDeclaration *BoundTreeBuilder::bind_constructor_declaration(ConstructorDeclSyntax *syntax)
    {
        auto bound = arena_.make<BoundFunctionDeclaration>();
        bound->location = syntax->location;
        bound->modifiers = syntax->modifiers;

        auto containing_type = get_containing_type();
        if (containing_type)
        {
            bound->name = containing_type->name;
            bound->symbol = symbol_table_.get_symbol_for_ast(syntax);
        }

        ScopeGuard scope(symbol_table_, bound->symbol);

        bound->returnTypeExpression = nullptr;

        for (auto param : syntax->parameters)
        {
            if (auto param_syntax = param->as<ParameterDeclSyntax>())
            {
                auto bound_param = arena_.make<BoundVariableDeclaration>();
                bound_param->location = param_syntax->location;
                bound_param->name = param_syntax->param->name ? param_syntax->param->name->get_name() : "";
                bound_param->typeExpression = param_syntax->param->type ? bind_type_expression(param_syntax->param->type) : nullptr;
                bound_param->isParameter = true;
                bound_param->symbol = symbol_table_.resolve_local(bound_param->name);

                bound->parameters.push_back(bound_param);
            }
        }

        if (syntax->body)
        {
            bound->body = bind_block(syntax->body);
        }

        return bound;
    }

    BoundTypeDeclaration *BoundTreeBuilder::bind_type_declaration(TypeDeclSyntax *syntax)
    {
        auto bound = arena_.make<BoundTypeDeclaration>();
        bound->location = syntax->location;
        bound->modifiers = syntax->modifiers;

        if (syntax->name)
        {
            bound->name = syntax->name->get_name();
        }

        bound->symbol = resolve_symbol({bound->name});

        ScopeGuard scope(symbol_table_, bound->symbol);

        for (auto member : syntax->members)
        {
            if (auto bound_member = bind_statement(member))
            {
                bound->members.push_back(bound_member);
            }
        }

        return bound;
    }

    BoundNamespaceDeclaration *BoundTreeBuilder::bind_namespace_declaration(NamespaceDeclSyntax *syntax)
    {
        auto bound = arena_.make<BoundNamespaceDeclaration>();
        bound->location = syntax->location;

        if (syntax->name)
        {
            bound->name = syntax->name->get_name();
        }

        bound->symbol = resolve_symbol({bound->name});

        ScopeGuard scope(symbol_table_, bound->symbol);

        if (syntax->body.has_value())
        {
            for (auto member : syntax->body.value())
            {
                if (auto bound_member = bind_statement(member))
                {
                    bound->members.push_back(bound_member);
                }
            }
        }

        return bound;
    }

    BoundPropertyDeclaration *BoundTreeBuilder::bind_property_declaration(PropertyDeclSyntax *syntax)
    {
        auto bound = arena_.make<BoundPropertyDeclaration>();
        bound->location = syntax->location;
        bound->modifiers = syntax->modifiers;

        if (syntax->variable && syntax->variable->variable)
        {
            if (syntax->variable->variable->name)
            {
                bound->name = syntax->variable->variable->name->get_name();
            }
            if (syntax->variable->variable->type)
            {
                bound->typeExpression = bind_type_expression(syntax->variable->variable->type);
            }
        }

        if (syntax->variable && syntax->variable->initializer)
        {
            bound->initializer = bind_expression(syntax->variable->initializer);
        }

        bound->symbol = resolve_symbol({bound->name});

        if (syntax->getter)
        {
            auto accessor = arena_.make<BoundPropertyAccessor>();
            accessor->kind = BoundPropertyAccessor::Kind::Get;

            if (auto prop_sym = bound->symbol->as<PropertySymbol>()) {
                auto getter_members = prop_sym->get_member("get");
                if (!getter_members.empty()) {
                    if (auto func_sym = getter_members[0]->as<FunctionSymbol>()) {
                        accessor->function_symbol = func_sym;

                        ScopeGuard scope(symbol_table_, func_sym);

                        if (auto expr_ptr = std::get_if<BaseExprSyntax *>(&syntax->getter->body))
                        {
                            accessor->expression = bind_expression(*expr_ptr);
                        }
                        else if (auto block_ptr = std::get_if<BlockSyntax *>(&syntax->getter->body))
                        {
                            accessor->body = bind_statement(*block_ptr);
                        }
                    }
                }
            }

            bound->getter = accessor;
        }

        if (syntax->setter)
        {
            auto accessor = arena_.make<BoundPropertyAccessor>();
            accessor->kind = BoundPropertyAccessor::Kind::Set;

            if (auto prop_sym = bound->symbol->as<PropertySymbol>()) {
                auto setter_members = prop_sym->get_member("set");
                if (!setter_members.empty()) {
                    if (auto func_sym = setter_members[0]->as<FunctionSymbol>()) {
                        accessor->function_symbol = func_sym;

                        ScopeGuard scope(symbol_table_, func_sym);

                        if (auto expr_ptr = std::get_if<BaseExprSyntax *>(&syntax->setter->body))
                        {
                            accessor->expression = bind_expression(*expr_ptr);
                        }
                        else if (auto block_ptr = std::get_if<BlockSyntax *>(&syntax->setter->body))
                        {
                            accessor->body = bind_statement(*block_ptr);
                        }
                    }
                }
            }

            bound->setter = accessor;
        }

        return bound;
    }

    BoundIfStatement *BoundTreeBuilder::bind_if_statement(IfStmtSyntax *syntax)
    {
        auto bound = arena_.make<BoundIfStatement>();
        bound->location = syntax->location;
        bound->condition = bind_expression(syntax->condition);
        bound->thenStatement = bind_statement(syntax->thenBranch);

        if (syntax->elseBranch)
        {
            bound->elseStatement = bind_statement(syntax->elseBranch);
        }

        return bound;
    }

    BoundWhileStatement *BoundTreeBuilder::bind_while_statement(WhileStmtSyntax *syntax)
    {
        auto bound = arena_.make<BoundWhileStatement>();
        bound->location = syntax->location;
        bound->condition = bind_expression(syntax->condition);
        bound->body = bind_statement(syntax->body);
        return bound;
    }

    BoundForStatement *BoundTreeBuilder::bind_for_statement(ForStmtSyntax *syntax)
    {
        auto bound = arena_.make<BoundForStatement>();
        bound->location = syntax->location;

        auto for_scope = symbol_table_.get_symbol_for_ast(syntax);
        ScopeGuard scope(symbol_table_, for_scope);

        if (syntax->initializer)
        {
            bound->initializer = bind_statement(syntax->initializer);
        }

        if (syntax->condition)
        {
            bound->condition = bind_expression(syntax->condition);
        }

        for (auto update : syntax->updates)
        {
            if (auto bound_update = bind_expression(update))
            {
                bound->incrementors.push_back(bound_update);
            }
        }

        bound->body = bind_statement(syntax->body);

        return bound;
    }

    BoundReturnStatement *BoundTreeBuilder::bind_return_statement(ReturnStmtSyntax *syntax)
    {
        auto bound = arena_.make<BoundReturnStatement>();
        bound->location = syntax->location;

        if (syntax->value)
        {
            bound->value = bind_expression(syntax->value);
        }

        return bound;
    }

    BoundBreakStatement *BoundTreeBuilder::bind_break_statement(BreakStmtSyntax *syntax)
    {
        auto bound = arena_.make<BoundBreakStatement>();
        bound->location = syntax->location;
        return bound;
    }

    BoundContinueStatement *BoundTreeBuilder::bind_continue_statement(ContinueStmtSyntax *syntax)
    {
        auto bound = arena_.make<BoundContinueStatement>();
        bound->location = syntax->location;
        return bound;
    }

    BoundExpressionStatement *BoundTreeBuilder::bind_expression_statement(ExpressionStmtSyntax *syntax)
    {
        auto bound = arena_.make<BoundExpressionStatement>();
        bound->location = syntax->location;
        bound->expression = bind_expression(syntax->expression);
        return bound;
    }

    BoundUsingStatement *BoundTreeBuilder::bind_using_statement(UsingDirectiveSyntax *syntax)
    {
        auto bound = arena_.make<BoundUsingStatement>();
        bound->location = syntax->location;

        if (syntax->target)
        {
            bound->namespaceParts = syntax->target->get_parts();
        }

        if (auto symbol = resolve_symbol(bound->namespaceParts))
        {
            bound->targetNamespace = symbol->as<NamespaceSymbol>();
        }

        return bound;
    }

#pragma endregion

#pragma region Expressions

    BoundExpression *BoundTreeBuilder::bind_expression(BaseExprSyntax *syntax)
    {
        if (!syntax)
            return nullptr;

        if (auto literal = syntax->as<LiteralExprSyntax>())
            return bind_literal(literal);
        if (auto name = syntax->as<BaseNameExprSyntax>())
            return bind_name(name);
        if (auto binary = syntax->as<BinaryExprSyntax>())
            return bind_binary_expression(binary);
        if (auto unary = syntax->as<UnaryExprSyntax>())
            return bind_unary_expression(unary);
        if (auto assignment = syntax->as<AssignmentExprSyntax>())
            return bind_assignment_expression(assignment);
        if (auto call = syntax->as<CallExprSyntax>())
            return bind_call_expression(call);
        if (auto member = syntax->as<MemberAccessExprSyntax>())
            return bind_member_access(member);
        if (auto indexer = syntax->as<IndexerExprSyntax>())
            return bind_index_expression(indexer);
        if (auto cast = syntax->as<CastExprSyntax>())
            return bind_cast_expression(cast);
        if (auto new_expr = syntax->as<NewExprSyntax>())
            return bind_new_expression(new_expr);
        if (auto this_expr = syntax->as<ThisExprSyntax>())
            return bind_this_expression(this_expr);
        if (auto array = syntax->as<ArrayLiteralExprSyntax>())
            return bind_array_creation(array);
        if (auto paren = syntax->as<ParenthesizedExprSyntax>())
            return bind_expression(paren->expression);

        return nullptr;
    }

    BoundLiteralExpression *BoundTreeBuilder::bind_literal(LiteralExprSyntax *syntax)
    {
        auto bound = arena_.make<BoundLiteralExpression>();
        bound->location = syntax->location;
        bound->literalKind = syntax->kind;

        switch (syntax->kind)
        {
        case LiteralKind::I32:
            bound->constantValue = static_cast<int64_t>(std::stoll(std::string(syntax->value)));
            break;
        case LiteralKind::F32:
            bound->constantValue = std::stod(std::string(syntax->value));
            break;
        case LiteralKind::Bool:
            bound->constantValue = (syntax->value == "true");
            break;
        case LiteralKind::Char:
            if (syntax->value.length() >= 1)
            {
                bound->constantValue = static_cast<int64_t>(static_cast<unsigned char>(syntax->value[0]));
            }
            else
            {
                error("Empty character literal", syntax->location);
                bound->constantValue = static_cast<int64_t>(0);
            }
            break;
        case LiteralKind::String:
            bound->constantValue = std::string(syntax->value);
            break;
        case LiteralKind::Null:
            bound->constantValue = std::monostate{};
            break;
        default:
            break;
        }

        return bound;
    }

    BoundExpression *BoundTreeBuilder::bind_name(BaseNameExprSyntax *syntax)
    {
        // Handle non-name left side of qualified name (e.g., array[index].field)
        if (auto qualified = syntax->as<QualifiedNameSyntax>())
        {
            if (!qualified->left->as<BaseNameExprSyntax>())
            {
                auto object = bind_expression(qualified->left);
                auto member_access = arena_.make<BoundMemberAccessExpression>();
                member_access->location = syntax->location;
                member_access->object = object;
                member_access->memberName = qualified->right->get_name();
                return member_access;
            }
        }

        auto parts = syntax->get_parts();

        // Convert qualified variable access to member access chain
        if (parts.size() > 1)
        {
            std::vector<std::string> first_part = {parts[0]};
            auto first_symbol = resolve_symbol(first_part);

            if (first_symbol && (first_symbol->is<VariableSymbol>() || first_symbol->is<ParameterSymbol>()))
            {
                auto object = arena_.make<BoundNameExpression>();
                object->location = syntax->location;
                object->parts = first_part;
                object->symbol = first_symbol;

                BoundExpression* current = object;

                for (size_t i = 1; i < parts.size(); ++i)
                {
                    auto member_access = arena_.make<BoundMemberAccessExpression>();
                    member_access->location = syntax->location;
                    member_access->object = current;
                    member_access->memberName = parts[i];
                    current = member_access;
                }

                return current;
            }
        }

        auto symbol = resolve_symbol(parts);

        // Add implicit 'this' for unqualified member access
        if (symbol && parts.size() == 1)
        {
            Symbol *member_of = nullptr;
            bool is_static = false;

            if (VariableSymbol *variable = symbol->as<VariableSymbol>(); variable && variable->is_field())
            {
                member_of = variable->parent;
            }
            else if (auto prop = symbol->as<PropertySymbol>())
            {
                member_of = prop->parent;
            }
            else if (auto func = symbol->as<FunctionSymbol>())
            {
                member_of = func->parent;
            }

            if (member_of && member_of->is<TypeSymbol>() && !is_static)
            {
                auto containing_type = get_containing_type();

                if (containing_type)
                {
                    TypeSymbol *current = containing_type;
                    bool is_accessible = false;

                    while (current)
                    {
                        if (current == member_of)
                        {
                            is_accessible = true;
                            break;
                        }
                        break;
                    }

                    if (is_accessible)
                    {
                        auto this_expr = arena_.make<BoundThisExpression>();
                        this_expr->location = syntax->location;
                        this_expr->containingType = containing_type;

                        auto member_access = arena_.make<BoundMemberAccessExpression>();
                        member_access->location = syntax->location;
                        member_access->object = this_expr;
                        member_access->memberName = parts[0];
                        member_access->member = symbol;

                        return member_access;
                    }
                }
            }
        }

        auto bound = arena_.make<BoundNameExpression>();
        bound->location = syntax->location;
        bound->parts = parts;
        bound->symbol = symbol;

        return bound;
    }

    BoundBinaryExpression *BoundTreeBuilder::bind_binary_expression(BinaryExprSyntax *syntax)
    {
        auto bound = arena_.make<BoundBinaryExpression>();
        bound->location = syntax->location;
        bound->left = bind_expression(syntax->left);
        bound->right = bind_expression(syntax->right);
        bound->operatorKind = syntax->op;
        return bound;
    }

    BoundUnaryExpression *BoundTreeBuilder::bind_unary_expression(UnaryExprSyntax *syntax)
    {
        auto bound = arena_.make<BoundUnaryExpression>();
        bound->location = syntax->location;
        bound->operand = bind_expression(syntax->operand);
        bound->operatorKind = syntax->op;
        return bound;
    }

    BoundAssignmentExpression *BoundTreeBuilder::bind_assignment_expression(AssignmentExprSyntax *syntax)
    {
        auto bound = arena_.make<BoundAssignmentExpression>();
        bound->location = syntax->location;
        bound->target = bind_expression(syntax->target);
        bound->value = bind_expression(syntax->value);
        bound->operatorKind = syntax->op;
        return bound;
    }

    BoundCallExpression *BoundTreeBuilder::bind_call_expression(CallExprSyntax *syntax)
    {
        auto bound = arena_.make<BoundCallExpression>();
        bound->location = syntax->location;
        bound->callee = bind_expression(syntax->callee);

        for (auto arg : syntax->arguments)
        {
            if (auto bound_arg = bind_expression(arg))
            {
                bound->arguments.push_back(bound_arg);
            }
        }

        if (auto name_expr = bound->callee->as<BoundNameExpression>())
        {
            std::string func_name = name_expr->parts.empty() ? "" : name_expr->parts.back();
            bound->method = resolve_function(func_name, bound);
        }
        else if (auto member_expr = bound->callee->as<BoundMemberAccessExpression>())
        {
            if (member_expr->member && member_expr->member->is<FunctionSymbol>())
            {
                bound->method = member_expr->member->as<FunctionSymbol>();
            }
        }

        return bound;
    }

    BoundMemberAccessExpression *BoundTreeBuilder::bind_member_access(MemberAccessExprSyntax *syntax)
    {
        auto bound = arena_.make<BoundMemberAccessExpression>();
        bound->location = syntax->location;
        bound->object = bind_expression(syntax->object);

        if (syntax->member)
        {
            bound->memberName = syntax->member->get_name();
        }

        if (bound->object && bound->object->type)
        {
            bound->member = resolve_member(bound->object->type, bound->memberName);
        }

        return bound;
    }

    BoundIndexExpression *BoundTreeBuilder::bind_index_expression(IndexerExprSyntax *syntax)
    {
        auto bound = arena_.make<BoundIndexExpression>();
        bound->location = syntax->location;
        bound->object = bind_expression(syntax->object);
        bound->index = bind_expression(syntax->index);
        return bound;
    }

    BoundCastExpression *BoundTreeBuilder::bind_cast_expression(CastExprSyntax *syntax)
    {
        auto bound = arena_.make<BoundCastExpression>();
        bound->location = syntax->location;
        bound->expression = bind_expression(syntax->expression);
        bound->targetTypeExpression = bind_type_expression(syntax->targetType);
        return bound;
    }

    BoundNewExpression *BoundTreeBuilder::bind_new_expression(NewExprSyntax *syntax)
    {
        auto bound = arena_.make<BoundNewExpression>();
        bound->location = syntax->location;
        bound->typeExpression = bind_type_expression(syntax->type);

        for (auto arg : syntax->arguments)
        {
            if (auto bound_arg = bind_expression(arg))
            {
                bound->arguments.push_back(bound_arg);
            }
        }

        bound->constructor = nullptr;

        return bound;
    }

    BoundThisExpression *BoundTreeBuilder::bind_this_expression(ThisExprSyntax *syntax)
    {
        auto bound = arena_.make<BoundThisExpression>();
        bound->location = syntax->location;
        bound->containingType = get_containing_type();
        return bound;
    }

    BoundArrayCreationExpression *BoundTreeBuilder::bind_array_creation(ArrayLiteralExprSyntax *syntax)
    {
        auto bound = arena_.make<BoundArrayCreationExpression>();
        bound->location = syntax->location;

        for (auto elem : syntax->elements)
        {
            if (auto bound_elem = bind_expression(elem))
            {
                bound->initializers.push_back(bound_elem);
            }
        }

        return bound;
    }

#pragma endregion

#pragma region Type Binding

    BoundTypeExpression *BoundTreeBuilder::bind_type_expression(BaseExprSyntax *syntax)
    {
        if (!syntax)
            return nullptr;

        auto bound = arena_.make<BoundTypeExpression>();
        bound->location = syntax->location;

        if (auto name = syntax->as<BaseNameExprSyntax>())
        {
            bound->parts = name->get_parts();

            if (auto symbol = resolve_symbol(bound->parts))
            {
                if (auto type_symbol = symbol->as<TypeSymbol>())
                {
                    bound->resolvedTypeReference = type_symbol->type;
                }
            }
        }
        else if (auto array_type = syntax->as<ArrayTypeSyntax>())
        {
            if (auto element_type = bind_type_expression(array_type->baseType))
            {
                bound->parts.push_back("[]");
                bound->typeArguments.push_back(element_type);

                if (array_type->size)
                {
                    bound->arraySize = bind_expression(array_type->size);
                }
            }
        }
        else if (auto ptr_type = syntax->as<PointerTypeSyntax>())
        {
            if (auto pointee = bind_type_expression(ptr_type->baseType))
            {
                bound->parts.push_back("*");
                bound->typeArguments.push_back(pointee);
            }
        }

        return bound;
    }

#pragma endregion

} // namespace Fern
