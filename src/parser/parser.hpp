#pragma once

#include "ast/ast.hpp"
#include "ast/arena.hpp"
#include "token_stream.hpp"
#include "common/token_kind.hpp"
#include <vector>
#include <optional>
#include <string>
#include <initializer_list>
#include <functional> // For std::invoke_result_t
#include "common/error.hpp"

namespace Fern {

class Parser : public DiagnosticSystem
{
public:
    Parser(TokenStream& tokens);
    ~Parser();

    // Main entry point
    CompilationUnitSyntax* parse();

private:
    // Data Members
    Arena arena;
    TokenStream& tokens;

    // Context tracking
    enum class Context {
        TOP_LEVEL,
        TYPE_BODY,
        NAMESPACE,
        FUNCTION,
        LOOP,
        PROPERTY_GETTER,
        PROPERTY_SETTER
    };
    std::vector<Context> contextStack;

    #pragma region Error Handling
    void parse_error(const std::string& msg);
    void parse_warning(const std::string& msg);
    MissingExprSyntax* errorExpr(const std::string& msg);
    MissingStmtSyntax* errorStmt(const std::string& msg);
    void synchronize();

    #pragma region Context Management
    bool inLoop() const;
    bool inFunction() const;
    bool inGetter() const;
    bool inSetter() const;
    bool inTypeBody() const;

    // Template function must be defined in the header.
    template <typename F>
    auto withContext(Context type, F &&func)
    {
        contextStack.push_back(type);
        if constexpr (std::is_void_v<std::invoke_result_t<F>>)
        {
            func();
            contextStack.pop_back();
        }
        else
        {
            auto result = func();
            contextStack.pop_back();
            return result;
        }
    }

    #pragma region Utility Helpers
    bool check(TokenKind kind);
    bool checkAny(std::initializer_list<TokenKind> kinds);
    bool consume(TokenKind kind);
    bool expect(TokenKind kind, const std::string& msg);
    Token previous();
    TokenKind peekNext();
    bool isExpressionTerminator();
    bool isPatternTerminator();
    bool isOnSameLine(const Token& prev, const Token& curr) const;
    bool requireSemicolonIfSameLine();

    #pragma region Top Level Parsing
    BaseStmtSyntax* parseTopLevelStatement();

    #pragma region Declarations
    bool checkDeclarationStart();
    BaseDeclSyntax* parseDeclaration();
    ModifierKindFlags parseModifiers();
    TypeDeclSyntax* parseTypeDecl(ModifierKindFlags modifiers, const Token& startToken);
    EnumCaseDeclSyntax* parseEnumCase();
    FunctionDeclSyntax* parseFunctionDecl(ModifierKindFlags modifiers, const Token& startToken);
    ConstructorDeclSyntax* parseConstructorDecl(ModifierKindFlags modifiers, const Token& startToken);
    BaseDeclSyntax* parseVarDeclaration(ModifierKindFlags modifiers, const Token& startToken);
    BaseExprSyntax* convertToArrayTypeIfNeeded(BaseExprSyntax* expr);
    std::vector<BaseDeclSyntax*> parseTypedMemberDeclarations(ModifierKindFlags modifiers, BaseExprSyntax* type, const Token& startToken);
    void parsePropertyAccessorSyntaxs(PropertyDeclSyntax* prop);
    NamespaceDeclSyntax* parseNamespaceDecl(const Token& startToken);

    #pragma region Statements
    BaseStmtSyntax* parseStatement();
    BlockSyntax* parseBlock();
    BaseStmtSyntax* parseIfStatement();
    WhileStmtSyntax* parseWhileStatement();
    BaseStmtSyntax* parseForStatement();
    ForStmtSyntax* parseTraditionalForStatement();
    ReturnStmtSyntax* parseReturnStatement();
    BreakStmtSyntax* parseBreakStatement();
    ContinueStmtSyntax* parseContinueStatement();
    ExpressionStmtSyntax* parseExpressionStatement();
    UsingDirectiveSyntax* parseUsingDirective();

    #pragma region Expressions
    BaseExprSyntax* parseExpression(int minPrecedence = 0);
    BaseExprSyntax* parseBinaryExpression(BaseExprSyntax* left, int minPrecedence);
    BaseExprSyntax* parsePrimaryExpression();
    BaseExprSyntax* parsePostfixExpression(BaseExprSyntax* expr);
    BaseExprSyntax* parseUnaryExpression();
    LiteralExprSyntax* parseLiteral();
    BaseNameExprSyntax* parseNameExpression();
    List<BaseExprSyntax *> parseGenericArgs();
    BaseExprSyntax* parseTypeExpression();
    BaseExprSyntax* parseParenthesizedOrLambda();
    BaseExprSyntax* parseCastExpression();
    BaseExprSyntax* parseArrayLiteral();
    BaseExprSyntax* parseNewExpression();
    BaseExprSyntax* parseLambdaExpression();

    BaseNameExprSyntax* parseIdentifier();
    TypedIdentifier* parseTypedIdentifier();
    List<ParameterDeclSyntax*> parseParameterList();
    List<TypeParameterDeclSyntax*> parseTypeParameterList();
    List<BaseExprSyntax*> parseBaseTypeList();  // Uses parseExpression() internally
};

} // namespace Fern