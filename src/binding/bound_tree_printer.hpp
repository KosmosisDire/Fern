#include "bound_tree.hpp"
#include <iostream>
#include <string>
#include <sstream>

namespace Fern
{
    class BoundTreePrinter : public DefaultBoundVisitor
    {
    private:
        int indentLevel = 0;
        const std::string indentStr = "  ";
        
        void printIndent()
        {
            for (int i = 0; i < indentLevel; ++i)
                std::cout << indentStr;
        }
        
        std::string typeToString(TypePtr type)
        {
            if (!type) return "null";
            
            if (type->is<UnresolvedType>()) {
                return "?" + std::to_string(type->as<UnresolvedType>()->id);
            }
            return type->get_name();
        }
        
        std::string symbolToString(Symbol* sym)
        {
            if (!sym) return "null";
            return sym->get_qualified_name();
        }
        
        std::string valueCategoryToString(ValueCategory cat)
        {
            return cat == ValueCategory::LValue ? "lvalue" : "rvalue";
        }
        
        void printNode(const std::string& nodeName, const std::string& props = "")
        {
            printIndent();
            std::cout << nodeName;
            if (!props.empty())
                std::cout << " [" << props << "]";
            std::cout << "\n";
        }
        
        std::string buildExpressionProps(BoundExpression* expr, const std::string& extra = "")
        {
            std::stringstream ss;
            
            // Add type
            ss << "type:" << typeToString(expr->type);
            
            // Add value category
            ss << " cat:" << valueCategoryToString(expr->valueCategory);
            
            // Add constant value if present
            if (expr->is_constant()) {
                ss << " const:";
                if (std::holds_alternative<int64_t>(expr->constantValue))
                    ss << std::get<int64_t>(expr->constantValue);
                else if (std::holds_alternative<uint64_t>(expr->constantValue))
                    ss << std::get<uint64_t>(expr->constantValue);
                else if (std::holds_alternative<bool>(expr->constantValue))
                    ss << (std::get<bool>(expr->constantValue) ? "true" : "false");
                else if (std::holds_alternative<std::string>(expr->constantValue))
                    ss << "\"" << std::get<std::string>(expr->constantValue) << "\"";
                else if (std::holds_alternative<double>(expr->constantValue))
                    ss << std::get<double>(expr->constantValue);
            }
            
            // Add extra properties
            if (!extra.empty()) {
                ss << " " << extra;
            }
            
            return ss.str();
        }
        
        class IndentGuard
        {
            BoundTreePrinter* printer;
        public:
            IndentGuard(BoundTreePrinter* p) : printer(p) { printer->indentLevel++; }
            ~IndentGuard() { printer->indentLevel--; }
        };
        
    public:
        // Expressions
        void visit(BoundLiteralExpression* node) override
        {
            std::stringstream extra;
            extra << "kind:" << static_cast<int>(node->literalKind);
            printNode("Literal", buildExpressionProps(node, extra.str()));
        }
        
        void visit(BoundNameExpression* node) override
        {
            std::stringstream extra;
            extra << "name:";
            for (size_t i = 0; i < node->parts.size(); ++i) {
                if (i > 0) extra << ".";
                extra << node->parts[i];
            }
            extra << " sym:" << symbolToString(node->symbol);
            printNode("Name", buildExpressionProps(node, extra.str()));
        }
        
        void visit(BoundBinaryExpression* node) override
        {
            std::stringstream extra;
            extra << "op:" << to_string(node->operatorKind);
            extra << " method:" << symbolToString(node->operatorMethod);
            printNode("Binary", buildExpressionProps(node, extra.str()));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "left:\n";
            {
                IndentGuard leftGuard(this);
                if (node->left) node->left->accept(this);
                else printNode("null");
            }
            
            printIndent();
            std::cout << "right:\n";
            {
                IndentGuard rightGuard(this);
                if (node->right) node->right->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundUnaryExpression* node) override
        {
            std::stringstream extra;
            extra << "op:" << to_string(node->operatorKind);
            extra << " method:" << symbolToString(node->operatorMethod);
            printNode("Unary", buildExpressionProps(node, extra.str()));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "operand:\n";
            {
                IndentGuard opGuard(this);
                if (node->operand) node->operand->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundAssignmentExpression* node) override
        {
            std::stringstream extra;
            extra << "op:" << to_string(node->operatorKind);
            printNode("Assignment", buildExpressionProps(node, extra.str()));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "target:\n";
            {
                IndentGuard targetGuard(this);
                if (node->target) node->target->accept(this);
                else printNode("null");
            }
            
            printIndent();
            std::cout << "value:\n";
            {
                IndentGuard valueGuard(this);
                if (node->value) node->value->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundCallExpression* node) override
        {
            std::stringstream extra;
            extra << "method:" << symbolToString(node->method);
            extra << " args:" << node->arguments.size();
            printNode("Call", buildExpressionProps(node, extra.str()));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "callee:\n";
            {
                IndentGuard calleeGuard(this);
                if (node->callee) node->callee->accept(this);
                else printNode("null");
            }
            
            if (!node->arguments.empty()) {
                printIndent();
                std::cout << "arguments:\n";
                IndentGuard argsGuard(this);
                for (size_t i = 0; i < node->arguments.size(); ++i) {
                    printIndent();
                    std::cout << "[" << i << "]:\n";
                    IndentGuard argGuard(this);
                    if (node->arguments[i]) node->arguments[i]->accept(this);
                    else printNode("null");
                }
            }
        }
        
        void visit(BoundMemberAccessExpression* node) override
        {
            std::stringstream extra;
            extra << "member:" << node->memberName;
            extra << " sym:" << symbolToString(node->member);
            printNode("MemberAccess", buildExpressionProps(node, extra.str()));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "object:\n";
            {
                IndentGuard objGuard(this);
                if (node->object) node->object->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundIndexExpression* node) override
        {
            std::stringstream extra;
            extra << "indexer:" << symbolToString(node->indexerProperty);
            printNode("Index", buildExpressionProps(node, extra.str()));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "object:\n";
            {
                IndentGuard objGuard(this);
                if (node->object) node->object->accept(this);
                else printNode("null");
            }
            
            printIndent();
            std::cout << "index:\n";
            {
                IndentGuard indexGuard(this);
                if (node->index) node->index->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundNewExpression* node) override
        {
            std::stringstream extra;
            extra << "ctor:" << symbolToString(node->constructor);
            extra << " args:" << node->arguments.size();
            printNode("New", buildExpressionProps(node, extra.str()));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "typeExpr:\n";
            {
                IndentGuard typeGuard(this);
                if (node->typeExpression) node->typeExpression->accept(this);
                else printNode("null");
            }
            
            if (!node->arguments.empty()) {
                printIndent();
                std::cout << "arguments:\n";
                IndentGuard argsGuard(this);
                for (size_t i = 0; i < node->arguments.size(); ++i) {
                    printIndent();
                    std::cout << "[" << i << "]:\n";
                    IndentGuard argGuard(this);
                    if (node->arguments[i]) node->arguments[i]->accept(this);
                    else printNode("null");
                }
            }
        }
        
        void visit(BoundArrayCreationExpression* node) override
        {
            std::stringstream extra;
            extra << "inits:" << node->initializers.size();
            printNode("ArrayCreation", buildExpressionProps(node, extra.str()));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "elementType:\n";
            {
                IndentGuard typeGuard(this);
                if (node->elementTypeExpression) node->elementTypeExpression->accept(this);
                else printNode("null");
            }
            
            if (node->size) {
                printIndent();
                std::cout << "size:\n";
                IndentGuard sizeGuard(this);
                node->size->accept(this);
            }
            
            if (!node->initializers.empty()) {
                printIndent();
                std::cout << "initializers:\n";
                IndentGuard initsGuard(this);
                for (size_t i = 0; i < node->initializers.size(); ++i) {
                    printIndent();
                    std::cout << "[" << i << "]:\n";
                    IndentGuard initGuard(this);
                    if (node->initializers[i]) node->initializers[i]->accept(this);
                    else printNode("null");
                }
            }
        }
        
        void visit(BoundCastExpression* node) override
        {
            std::stringstream extra;
            extra << "conversion:" << to_string(node->conversionKind);
            printNode("Cast", buildExpressionProps(node, extra.str()));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "expression:\n";
            {
                IndentGuard exprGuard(this);
                if (node->expression) node->expression->accept(this);
                else printNode("null");
            }
            
            printIndent();
            std::cout << "targetType:\n";
            {
                IndentGuard typeGuard(this);
                if (node->targetTypeExpression) node->targetTypeExpression->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundConditionalExpression* node) override
        {
            printNode("Conditional", buildExpressionProps(node));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "condition:\n";
            {
                IndentGuard condGuard(this);
                if (node->condition) node->condition->accept(this);
                else printNode("null");
            }
            
            printIndent();
            std::cout << "then:\n";
            {
                IndentGuard thenGuard(this);
                if (node->thenExpression) node->thenExpression->accept(this);
                else printNode("null");
            }
            
            printIndent();
            std::cout << "else:\n";
            {
                IndentGuard elseGuard(this);
                if (node->elseExpression) node->elseExpression->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundThisExpression* node) override
        {
            std::stringstream extra;
            extra << "containingType:" << symbolToString(node->containingType);
            printNode("This", buildExpressionProps(node, extra.str()));
        }
        
        void visit(BoundParenthesizedExpression* node) override
        {
            printNode("Parenthesized", buildExpressionProps(node));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "expression:\n";
            {
                IndentGuard exprGuard(this);
                if (node->expression) node->expression->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundConversionExpression* node) override
        {
            std::stringstream extra;
            extra << "conversion:" << to_string(node->conversionKind);
            printNode("Conversion", buildExpressionProps(node, extra.str()));
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "expression:\n";
            {
                IndentGuard exprGuard(this);
                if (node->expression) node->expression->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundTypeExpression* node) override
        {
            std::stringstream extra;
            extra << "name:";
            for (size_t i = 0; i < node->parts.size(); ++i) {
                if (i > 0) extra << ".";
                extra << node->parts[i];
            }
            extra << " resolved:" << typeToString(node->resolvedTypeReference);
            printNode("TypeExpr", buildExpressionProps(node, extra.str()));
            
            if (!node->typeArguments.empty()) {
                IndentGuard guard(this);
                printIndent();
                std::cout << "typeArgs:\n";
                IndentGuard argsGuard(this);
                for (size_t i = 0; i < node->typeArguments.size(); ++i) {
                    printIndent();
                    std::cout << "[" << i << "]:\n";
                    IndentGuard argGuard(this);
                    if (node->typeArguments[i]) node->typeArguments[i]->accept(this);
                    else printNode("null");
                }
            }
        }
        
        // Statements
        void visit(BoundBlockStatement* node) override
        {
            printNode("Block", "stmts:" + std::to_string(node->statements.size()));
            IndentGuard guard(this);
            for (size_t i = 0; i < node->statements.size(); ++i) {
                if (node->statements[i]) node->statements[i]->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundExpressionStatement* node) override
        {
            printNode("ExpressionStmt");
            IndentGuard guard(this);
            if (node->expression) node->expression->accept(this);
            else printNode("null");
        }
        
        void visit(BoundIfStatement* node) override
        {
            printNode("If");
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "condition:\n";
            {
                IndentGuard condGuard(this);
                if (node->condition) node->condition->accept(this);
                else printNode("null");
            }
            
            printIndent();
            std::cout << "then:\n";
            {
                IndentGuard thenGuard(this);
                if (node->thenStatement) node->thenStatement->accept(this);
                else printNode("null");
            }
            
            if (node->elseStatement) {
                printIndent();
                std::cout << "else:\n";
                IndentGuard elseGuard(this);
                node->elseStatement->accept(this);
            }
        }
        
        void visit(BoundWhileStatement* node) override
        {
            printNode("While");
            IndentGuard guard(this);
            
            printIndent();
            std::cout << "condition:\n";
            {
                IndentGuard condGuard(this);
                if (node->condition) node->condition->accept(this);
                else printNode("null");
            }
            
            printIndent();
            std::cout << "body:\n";
            {
                IndentGuard bodyGuard(this);
                if (node->body) node->body->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundForStatement* node) override
        {
            printNode("For", "incs:" + std::to_string(node->incrementors.size()));
            IndentGuard guard(this);
            
            if (node->initializer) {
                printIndent();
                std::cout << "init:\n";
                IndentGuard initGuard(this);
                node->initializer->accept(this);
            }
            
            if (node->condition) {
                printIndent();
                std::cout << "condition:\n";
                IndentGuard condGuard(this);
                node->condition->accept(this);
            }
            
            if (!node->incrementors.empty()) {
                printIndent();
                std::cout << "incrementors:\n";
                IndentGuard incsGuard(this);
                for (size_t i = 0; i < node->incrementors.size(); ++i) {
                    printIndent();
                    std::cout << "[" << i << "]:\n";
                    IndentGuard incGuard(this);
                    if (node->incrementors[i]) node->incrementors[i]->accept(this);
                    else printNode("null");
                }
            }
            
            printIndent();
            std::cout << "body:\n";
            {
                IndentGuard bodyGuard(this);
                if (node->body) node->body->accept(this);
                else printNode("null");
            }
        }
        
        void visit(BoundBreakStatement* node) override
        {
            printNode("Break");
        }
        
        void visit(BoundContinueStatement* node) override
        {
            printNode("Continue");
        }
        
        void visit(BoundReturnStatement* node) override
        {
            printNode("Return", node->value ? "hasValue" : "void");
            if (node->value) {
                IndentGuard guard(this);
                node->value->accept(this);
            }
        }
        
        void visit(BoundUsingStatement* node) override
        {
            std::stringstream extra;
            extra << "namespace:";
            for (size_t i = 0; i < node->namespaceParts.size(); ++i) {
                if (i > 0) extra << ".";
                extra << node->namespaceParts[i];
            }
            extra << " target:" << symbolToString(node->targetNamespace);
            printNode("Using", extra.str());
        }
        
        // Declarations
        void visit(BoundVariableDeclaration* node) override
        {
            std::stringstream extra;
            extra << "name:" << node->name;
            extra << " sym:" << symbolToString(node->symbol);
            extra << " mods:" << static_cast<int>(node->modifiers);
            if (node->isParameter) extra << " param";
            if (node->isLocal) extra << " local";
            if (node->isField) extra << " field";
            printNode("VarDecl", extra.str());
            IndentGuard guard(this);
            
            if (node->typeExpression) {
                printIndent();
                std::cout << "type:\n";
                IndentGuard typeGuard(this);
                node->typeExpression->accept(this);
            }
            
            if (node->initializer) {
                printIndent();
                std::cout << "init:\n";
                IndentGuard initGuard(this);
                node->initializer->accept(this);
            }
        }
        
        void visit(BoundFunctionDeclaration* node) override
        {
            std::stringstream extra;
            extra << "name:" << node->name;
            extra << " sym:" << symbolToString(node->symbol);
            extra << " mods:" << static_cast<int>(node->modifiers);
            if (node->isConstructor) extra << " ctor";
            extra << " params:" << node->parameters.size();
            printNode("FunctionDecl", extra.str());
            IndentGuard guard(this);
            
            if (node->returnTypeExpression) {
                printIndent();
                std::cout << "returnType:\n";
                IndentGuard typeGuard(this);
                node->returnTypeExpression->accept(this);
            }
            
            if (!node->parameters.empty()) {
                printIndent();
                std::cout << "parameters:\n";
                IndentGuard paramsGuard(this);
                for (auto* param : node->parameters) {
                    if (param) param->accept(this);
                    else printNode("null");
                }
            }
            
            if (node->body) {
                printIndent();
                std::cout << "body:\n";
                IndentGuard bodyGuard(this);
                node->body->accept(this);
            }
        }
        
        void visit(BoundPropertyDeclaration* node) override
        {
            std::stringstream extra;
            extra << "name:" << node->name;
            extra << " sym:" << symbolToString(node->symbol);
            extra << " mods:" << static_cast<int>(node->modifiers);
            printNode("PropertyDecl", extra.str());
            IndentGuard guard(this);
            
            if (node->typeExpression) {
            printIndent();
            std::cout << "type:\n";
            IndentGuard typeGuard(this);
            node->typeExpression->accept(this);
            }
            
            if (node->getter) {
            printIndent();
            std::cout << "getter:\n";
            IndentGuard getterGuard(this);
            
            std::stringstream getterExtra;
            getterExtra << "kind:get";
            printNode("PropertyAccessor", getterExtra.str());
            IndentGuard accessorGuard(this);
            
            if (node->getter->expression) {
                printIndent();
                std::cout << "expression:\n";
                IndentGuard exprGuard(this);
                node->getter->expression->accept(this);
            }
            
            if (node->getter->body) {
                printIndent();
                std::cout << "body:\n";
                IndentGuard bodyGuard(this);
                node->getter->body->accept(this);
            }
            }
            
            if (node->setter) {
            printIndent();
            std::cout << "setter:\n";
            IndentGuard setterGuard(this);
            
            std::stringstream setterExtra;
            setterExtra << "kind:set";
            printNode("PropertyAccessor", setterExtra.str());
            IndentGuard accessorGuard(this);
            
            if (node->setter->expression) {
                printIndent();
                std::cout << "expression:\n";
                IndentGuard exprGuard(this);
                node->setter->expression->accept(this);
            }
            
            if (node->setter->body) {
                printIndent();
                std::cout << "body:\n";
                IndentGuard bodyGuard(this);
                node->setter->body->accept(this);
            }
            }
        }
        
        void visit(BoundTypeDeclaration* node) override
        {
            std::stringstream extra;
            extra << "name:" << node->name;
            extra << " sym:" << symbolToString(node->symbol);
            extra << " mods:" << static_cast<int>(node->modifiers);
            extra << " members:" << node->members.size();
            printNode("TypeDecl", extra.str());
            IndentGuard guard(this);
            
            if (node->baseTypeExpression) {
                printIndent();
                std::cout << "base:\n";
                IndentGuard baseGuard(this);
                node->baseTypeExpression->accept(this);
            }
            
            if (!node->members.empty()) {
                printIndent();
                std::cout << "members:\n";
                IndentGuard membersGuard(this);
                for (auto* member : node->members) {
                    if (member) member->accept(this);
                    else printNode("null");
                }
            }
        }
        
        void visit(BoundNamespaceDeclaration* node) override
        {
            std::stringstream extra;
            extra << "name:" << node->name;
            extra << " sym:" << symbolToString(node->symbol);
            extra << " members:" << node->members.size();
            printNode("NamespaceDecl", extra.str());
            IndentGuard guard(this);
            
            if (!node->members.empty()) {
                printIndent();
                std::cout << "members:\n";
                IndentGuard membersGuard(this);
                for (auto* member : node->members) {
                    if (member) member->accept(this);
                    else printNode("null");
                }
            }
        }
        
        // Top-level
        void visit(BoundCompilationUnit* node) override
        {
            printNode("CompilationUnit", "stmts:" + std::to_string(node->statements.size()));
            IndentGuard guard(this);
            for (auto* stmt : node->statements) {
                if (stmt) stmt->accept(this);
                else printNode("null");
            }
        }
    };
    
    // Helper function to print a bound tree
    void printBoundTree(BoundNode* root)
    {
        if (!root) {
            std::cout << "null\n";
            return;
        }
        BoundTreePrinter printer;
        root->accept(&printer);
    }
    
} // namespace Fern