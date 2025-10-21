#include "parser/parser.hpp"

// #define REQUIRE_SEMI
#ifdef REQUIRE_SEMI
#define HANDLE_SEMI \
    expect(TokenKind::Semicolon, "Expected ';' to end statement");
#else
#define HANDLE_SEMI \
    requireSemicolonIfSameLine();
#endif

namespace Fern
{

    #pragma region Public API

    Parser::Parser(TokenStream &tokens) : tokens(tokens)
    {
        contextStack.push_back(Context::TOP_LEVEL);
    }

    Parser::~Parser() = default;


    CompilationUnitSyntax *Parser::parse()
    {
        auto unit = arena.make<CompilationUnitSyntax>();
        if (tokens.at_end())
        {
            unit->location = {{0, 1, 1}, 0};
            unit->topLevelStatements = arena.emptyList<BaseStmtSyntax *>();
            return unit;
        }

        auto startToken = tokens.current();
        std::vector<BaseStmtSyntax *> statements;

        while (!tokens.at_end())
        {
            auto stmt = parseTopLevelStatement();
            if (stmt)
            {
                statements.push_back(stmt);
            }
        }

        unit->topLevelStatements = arena.makeList(statements);
        // Use previous token unless the file was empty to begin with.
        unit->location = SourceRange(startToken.location.start, tokens.previous().location.end());
        return unit;
    }

    const std::vector<Parser::ParseError> &Parser::getErrors() const
    {
        return errors;
    }

    bool Parser::hasErrors() const
    {
        return !errors.empty();
    }

    #pragma endregion
    
    #pragma region Error Handling

    void Parser::error(const std::string &msg)
    {
        static int lastPos = 0;
        if (lastPos == tokens.position())
        {
            synchronize();
            return;
        }
        lastPos = tokens.position();

        errors.push_back({msg, tokens.current().location, ParseError::ERROR});
    }

    void Parser::warning(const std::string &msg)
    {
        errors.push_back({msg, tokens.current().location, ParseError::WARNING});
    }

    MissingExprSyntax *Parser::errorExpr(const std::string &msg)
    {
        error(msg);
        auto err = arena.make<MissingExprSyntax>();
        err->message = msg;
        err->location = tokens.previous().location;
        return err;
    }

    MissingStmtSyntax *Parser::errorStmt(const std::string &msg)
    {
        error(msg);
        auto err = arena.make<MissingStmtSyntax>();
        err->message = msg;
        err->location = tokens.previous().location;
        return err;
    }

    void Parser::synchronize()
    {
        while (!tokens.at_end())
        {
            tokens.advance();

            if (consume(TokenKind::Semicolon))
            {
                return;
            }

            if (tokens.current().starts_declaration() || tokens.current().starts_statement())
            {
                return;
            }
        }
    }

    #pragma endregion
    
    #pragma region Context Management

    bool Parser::inLoop() const
    {
        for (auto it = contextStack.rbegin(); it != contextStack.rend(); ++it)
        {
            if (*it == Context::LOOP)
                return true;
        }
        return false;
    }

    bool Parser::inFunction() const
    {
        for (auto it = contextStack.rbegin(); it != contextStack.rend(); ++it)
        {
            if (*it == Context::FUNCTION)
                return true;
        }
        return false;
    }

    bool Parser::inGetter() const
    {
        for (auto it = contextStack.rbegin(); it != contextStack.rend(); ++it)
        {
            if (*it == Context::PROPERTY_GETTER)
                return true;
        }
        return false;
    }

    bool Parser::inSetter() const
    {
        for (auto it = contextStack.rbegin(); it != contextStack.rend(); ++it)
        {
            if (*it == Context::PROPERTY_SETTER)
                return true;
        }
        return false;
    }

    bool Parser::inTypeBody() const
    {
        if (contextStack.back() == Context::TYPE_BODY)
            return true;
        return false;
    }

    #pragma endregion
    
    #pragma region Utility Helpers

    bool Parser::check(TokenKind kind)
    {
        return tokens.check(kind);
    }

    bool Parser::checkAny(std::initializer_list<TokenKind> kinds)
    {
        return tokens.check_any(kinds);
    }

    bool Parser::consume(TokenKind kind)
    {
        return tokens.consume(kind);
    }

    bool Parser::expect(TokenKind kind, const std::string &msg)
    {
        if (!consume(kind))
        {
            error(msg);
            return false;
        }
        return true;
    }

    Token Parser::previous()
    {
        return tokens.previous();
    }

    TokenKind Parser::peekNext()
    {
        auto checkpoint = tokens.checkpoint();
        tokens.advance();
        TokenKind next = tokens.current().kind;
        tokens.restore(checkpoint);
        return next;
    }

    #pragma endregion
    
    #pragma region Top Level Parsing

    BaseStmtSyntax *Parser::parseTopLevelStatement()
    {
        if (check(TokenKind::Using))
        {
            return parseUsingDirective();
        }
        if (check(TokenKind::Namespace))
        {
            return parseNamespaceDecl(tokens.current());
        }
        if (tokens.current().is_modifier() || checkDeclarationStart())
        {
            return parseDeclaration();
        }
        if (tokens.current().starts_expression())
        {
            return parseExpressionStatement();
        }
        error("Unexpected token at top level");
        synchronize();
        return errorStmt("Invalid top level construct");
    }

    #pragma endregion
    
    #pragma region Declarations

    bool Parser::checkDeclarationStart()
    {
        // 'new' should only be treated as a declaration start inside type bodies
        if (check(TokenKind::New))
        {
            return inTypeBody();
        }

        return checkAny({TokenKind::Type, TokenKind::Enum, TokenKind::Fn,
                         TokenKind::Var, TokenKind::Ref, TokenKind::Namespace});
    }

    BaseDeclSyntax *Parser::parseDeclaration()
    {
        auto startToken = tokens.current();
        ModifierKindFlags modifiers = parseModifiers();

        if (check(TokenKind::Namespace))
        {
            auto ns = parseNamespaceDecl(startToken);
            ns->modifiers = modifiers;
            return ns;
        }
        if (checkAny({TokenKind::Type, TokenKind::Ref, TokenKind::Enum, TokenKind::Static}))
        {
            return parseTypeDecl(modifiers, startToken);
        }
        if (check(TokenKind::Fn))
        {
            return parseFunctionDecl(modifiers, startToken);
        }
        if (check(TokenKind::New))
        {
            return parseConstructorDecl(modifiers, startToken);
        }
        if (check(TokenKind::Var))
        {
            return parseVarDeclaration(modifiers, startToken);
        }

        auto checkpoint = tokens.checkpoint();
        auto type = parseTypeExpression();
        if (type && check(TokenKind::Identifier))
        {
            // Parse all comma-separated declarations with the same type
            auto allDeclarations = parseTypedMemberDeclarations(modifiers, type, startToken);
            // TODO: Return the first one for now (caller will need to handle multiple)
            return allDeclarations.empty() ? nullptr : allDeclarations[0];
        }

        tokens.restore(checkpoint);
        error("Expected declaration");
        return nullptr;
    }

    ModifierKindFlags Parser::parseModifiers()
    {
        ModifierKindFlags mods = ModifierKindFlags::None;
        while (tokens.current().is_modifier())
        {
            mods |= tokens.current().to_modifier_kind();
            tokens.advance();
        }
        return mods;
    }

    TypeDeclSyntax *Parser::parseTypeDecl(ModifierKindFlags modifiers, const Token &startToken)
    {
        auto decl = arena.make<TypeDeclSyntax>();
        decl->modifiers = modifiers;

        if (consume(TokenKind::Static))
        {
            expect(TokenKind::Type, "Expected 'type' after 'static'");
            decl->kind = TypeDeclSyntax::Kind::StaticType;
        }
        else if (consume(TokenKind::Ref))
        {
            expect(TokenKind::Type, "Expected 'type' after 'ref'");
            decl->kind = TypeDeclSyntax::Kind::RefType;
        }
        else if (consume(TokenKind::Enum))
        {
            decl->kind = TypeDeclSyntax::Kind::Enum;
        }
        else if (consume(TokenKind::Type))
        {
            decl->kind = TypeDeclSyntax::Kind::Type;
        }
        else
        {
            return nullptr;
        }

        if (check(TokenKind::Identifier))
        {
            decl->name = parseIdentifier();
        }
        else
        {
            error("Expected type name");
            decl->name = arena.makeIdentifier(Token::invalid_token(tokens.current()));
        }

        // Parse generic type parameters: <T, U, V>
        if (check(TokenKind::Less))
        {
            decl->typeParameters = parseTypeParameterList();
        }
        else
        {
            decl->typeParameters = arena.emptyList<TypeParameterDeclSyntax *>();
        }

        if (consume(TokenKind::Colon))
        {
            decl->baseTypes = parseBaseTypeList();
        }
        else
        {
            decl->baseTypes = arena.emptyList<BaseExprSyntax *>();
        }

        expect(TokenKind::LeftBrace, "Expected '{' after type declaration");

        std::vector<BaseDeclSyntax *> members;
        withContext(Context::TYPE_BODY, [&]()
                    {
        while (!check(TokenKind::RightBrace) && !tokens.at_end()) {
            if (decl->kind == TypeDeclSyntax::Kind::Enum) {
                if (tokens.current().is_modifier() || check(TokenKind::Fn)) {
                    if (auto member = parseDeclaration()) members.push_back(member);
                } else if (check(TokenKind::Identifier)) {
                    if (auto enumCase = parseEnumCase()) members.push_back(enumCase);
                } else {
                    error("Unexpected token in enum body");
                    tokens.advance();
                }
            } else {
                // Check if this is a typed declaration that could have multiple comma-separated variables
                auto checkpoint = tokens.checkpoint();
                auto type = parseTypeExpression();
                if (type && check(TokenKind::Identifier))
                {
                    // Parse all comma-separated declarations with the same type
                    auto declarations = parseTypedMemberDeclarations(ModifierKindFlags::None, type, previous());
                    for (auto decl : declarations) {
                        members.push_back(decl);
                    }
                } else {
                    tokens.restore(checkpoint);
                    if (auto member = parseDeclaration()) {
                        members.push_back(member);
                    }
                }
            }
            consume(TokenKind::Comma);
            consume(TokenKind::Semicolon);
        } });

        decl->members = arena.makeList(members);
        expect(TokenKind::RightBrace, "Expected '}' to close type declaration");

        decl->location = SourceRange(startToken.location.start, previous().location.end());
        return decl;
    }

    EnumCaseDeclSyntax *Parser::parseEnumCase()
    {
        auto startToken = tokens.current();
        auto decl = arena.make<EnumCaseDeclSyntax>();
        decl->name = parseIdentifier();

        if (check(TokenKind::LeftParen))
        {
            decl->associatedData = parseParameterList();
        }
        else
        {
            decl->associatedData = arena.emptyList<ParameterDeclSyntax *>();
        }

        decl->location = SourceRange(startToken.location.start, previous().location.end());
        return decl;
    }

    FunctionDeclSyntax *Parser::parseFunctionDecl(ModifierKindFlags modifiers, const Token &startToken)
    {
        auto decl = arena.make<FunctionDeclSyntax>();
        decl->modifiers = modifiers;

        consume(TokenKind::Fn);
        decl->name = parseIdentifier();

        // Check if we have parentheses for parameters
        if (check(TokenKind::LeftParen))
        {
            // Traditional parameter list with parentheses
            decl->parameters = parseParameterList();
        }
        else if (check(TokenKind::ThinArrow))
        {
            // No parentheses, but we have a return type - assume no parameters
            decl->parameters = arena.emptyList<ParameterDeclSyntax *>();
        }
        else if (check(TokenKind::LeftBrace))
        {
            // No parentheses, no return type - assume no parameters
            decl->parameters = arena.emptyList<ParameterDeclSyntax *>();
        }
        else
        {
            // Ambiguous case - could be parameters or return type
            // Look ahead to disambiguate
            auto checkpoint = tokens.checkpoint();
            
            // Try to parse as type expression (for return type)
            auto potentialReturnType = parseTypeExpression();
            
            if (potentialReturnType && (check(TokenKind::LeftBrace) || check(TokenKind::Semicolon)))
            {
                // Successfully parsed a type and found function body/end
                // This is a return type, not parameters
                tokens.restore(checkpoint);
                decl->parameters = arena.emptyList<ParameterDeclSyntax *>();
            }
            else
            {
                // Couldn't parse as return type, or ambiguous
                // Default to requiring parentheses for clarity
                tokens.restore(checkpoint);
                error("Expected '(' for parameter list, ':' for return type, or '{' for function body");
                decl->parameters = arena.emptyList<ParameterDeclSyntax *>();
            }
        }

        // Parse return type if present
        if (consume(TokenKind::ThinArrow))
        {
            decl->returnType = parseTypeExpression();
            if (!decl->returnType)
            {
                decl->returnType = errorExpr("Expected return type");
            }
        }
        else
        {
            decl->returnType = nullptr; // void
        }

        // Parse function body
        if (check(TokenKind::LeftBrace))
        {
            // External functions should not have a body
            if (has_flag(modifiers, ModifierKindFlags::Extern))
            {
                error("External function cannot have a body");
                decl->body = nullptr;
                // Skip the block to recover
                parseBlock();
            }
            else
            {
                decl->body = withContext(Context::FUNCTION, [this]()
                                        { return parseBlock(); });
            }
        }
        else if (check(TokenKind::FatArrow))
        {
            // Expression body not allowed for external functions
            if (has_flag(modifiers, ModifierKindFlags::Extern))
            {
                error("External function cannot have a body");
                decl->body = nullptr;
            }
            else
            {
                error("Expression-bodied functions not yet supported");
                decl->body = nullptr;
            }
        }
        else
        {
            // No body - this is abstract, interface, or external
            // Semicolon is optional
            consume(TokenKind::Semicolon);
            decl->body = nullptr;
        }

        decl->location = SourceRange(startToken.location.start, previous().location.end());
        return decl;
    }

    ConstructorDeclSyntax *Parser::parseConstructorDecl(ModifierKindFlags modifiers, const Token &startToken)
    {
        auto decl = arena.make<ConstructorDeclSyntax>();
        decl->modifiers = modifiers;

        consume(TokenKind::New);
        decl->parameters = parseParameterList();

        expect(TokenKind::LeftBrace, "Expected '{' after constructor parameters");
        decl->body = withContext(Context::FUNCTION, [this]()
                                 { return parseBlock(); });

        if (!decl->body)
        {
            auto block = arena.make<BlockSyntax>();
            block->location = previous().location;
            block->statements = arena.emptyList<BaseStmtSyntax *>();
            decl->body = block;
        }

        decl->location = SourceRange(startToken.location.start, previous().location.end());
        return decl;
    }

    BaseDeclSyntax *Parser::parseVarDeclaration(ModifierKindFlags modifiers, const Token &startToken)
    {
        consume(TokenKind::Var);

        if (!check(TokenKind::Identifier))
        {
            // Check if the user used a keyword instead of an identifier
            auto currentToken = tokens.current();
            if (currentToken.is_keyword())
            {
                error("Cannot use keyword '" + std::string(currentToken.text) + "' as an identifier");
            }
            else
            {
                error("Expected identifier after 'var', found '" + std::string(currentToken.text) + "' instead");
            }
            // Skip the invalid token to avoid infinite loop
            tokens.advance();
            return nullptr;
        }
        auto name = parseIdentifier();

        // Check if this is a property (has arrow syntax or braces after optional initializer)
        bool isProperty = false;
        BaseExprSyntax *initializer = nullptr;

        if (checkAny({TokenKind::FatArrow, TokenKind::LeftBrace}))
        {
            isProperty = true;
        }
        else if (check(TokenKind::Assign))
        {
            auto initCheckpoint = tokens.checkpoint();
            tokens.advance();
            parseExpression();
            if (check(TokenKind::LeftBrace))
            {
                isProperty = true;
                tokens.restore(initCheckpoint);
                consume(TokenKind::Assign);
                initializer = parseExpression();
            }
            else
            {
                tokens.restore(initCheckpoint);
            }
        }

        if (isProperty)
        {
            // Create PropertyDeclSyntax with embedded VariableDeclSyntax
            auto prop = arena.make<PropertyDeclSyntax>();
            prop->modifiers = modifiers;

            // Create the underlying variable
            auto varDecl = arena.make<VariableDeclSyntax>();
            varDecl->modifiers = modifiers;

            auto ti = arena.make<TypedIdentifier>();
            ti->name = name;
            ti->type = nullptr; // Type inference for var
            ti->location = SourceRange(startToken.location.start, name->location.end());

            varDecl->variable = ti;
            varDecl->initializer = initializer;
            varDecl->location = SourceRange(startToken.location.start, previous().location.end());

            prop->variable = varDecl;

            if (consume(TokenKind::FatArrow))
            {
                auto getter = arena.make<PropertyAccessorSyntax>();
                getter->kind = PropertyAccessorSyntax::Kind::Get;
                getter->body = parseExpression();
                prop->getter = getter;
                prop->setter = nullptr;
                HANDLE_SEMI
            }
            else if (check(TokenKind::LeftBrace))
            {
                parsePropertyAccessorSyntaxs(prop);
            }

            prop->location = SourceRange(startToken.location.start, previous().location.end());
            return prop;
        }

        // Always create VariableDeclSyntax - the context doesn't matter for the AST structure
        auto decl = arena.make<VariableDeclSyntax>();
        decl->modifiers = modifiers;

        auto ti = arena.make<TypedIdentifier>();
        ti->name = name;

        // Only type inference allowed for var declarations
        ti->type = nullptr; // Type inference

        ti->location = SourceRange(startToken.location.start, previous().location.end());
        decl->variable = ti;

        if (consume(TokenKind::Assign))
        {
            decl->initializer = parseExpression();
            if (!decl->initializer)
            {
                decl->initializer = errorExpr("Expected initializer");
            }
        }
        else
        {
            decl->initializer = nullptr;
        }

        HANDLE_SEMI
        decl->location = SourceRange(startToken.location.start, previous().location.end());
        return decl;
    }

    BaseExprSyntax *Parser::convertToArrayTypeIfNeeded(BaseExprSyntax *expr)
    {
        if (auto indexer = expr->as<IndexerExprSyntax>())
        {
            // Convert IndexerExprSyntax to ArrayTypeSyntax for type declarations
            // Check if the index is a literal (array size)
            if (auto literal = indexer->index->as<LiteralExprSyntax>())
            {
                if (literal->kind == LiteralKind::I32 ||
                    literal->kind == LiteralKind::I64 ||
                    literal->kind == LiteralKind::I8)
                {
                    auto arrayType = arena.make<ArrayTypeSyntax>();
                    arrayType->baseType = indexer->object;
                    arrayType->size = literal;
                    arrayType->location = indexer->location;
                    return arrayType;
                }
            }
            // For non-literal indices, still convert but without size
            auto arrayType = arena.make<ArrayTypeSyntax>();
            arrayType->baseType = indexer->object;
            arrayType->size = nullptr;
            arrayType->location = indexer->location;
            return arrayType;
        }
        return expr;
    }

    std::vector<BaseDeclSyntax *> Parser::parseTypedMemberDeclarations(ModifierKindFlags modifiers, BaseExprSyntax *type, const Token &startToken)
    {
        type = convertToArrayTypeIfNeeded(type);
        std::vector<BaseDeclSyntax *> declarations;
        bool hasProperties = false;

        do
        {
            auto fieldStartToken = tokens.current();
            auto name = parseIdentifier();
            BaseExprSyntax *initializer = nullptr;

            if (consume(TokenKind::Assign))
            {
                initializer = parseExpression();
            }

            if (checkAny({TokenKind::FatArrow, TokenKind::LeftBrace}))
            {
                // Create PropertyDeclSyntax with embedded VariableDeclSyntax
                auto prop = arena.make<PropertyDeclSyntax>();
                prop->modifiers = modifiers;

                // Create the underlying variable
                auto varDecl = arena.make<VariableDeclSyntax>();
                varDecl->modifiers = modifiers;

                auto ti = arena.make<TypedIdentifier>();
                ti->name = name;
                ti->type = type;
                ti->location = SourceRange(fieldStartToken.location.start, name->location.end());

                varDecl->variable = ti;
                varDecl->initializer = initializer;
                varDecl->location = SourceRange(fieldStartToken.location.start, previous().location.end());

                prop->variable = varDecl;

                if (consume(TokenKind::FatArrow))
                {
                    auto getter = arena.make<PropertyAccessorSyntax>();
                    getter->kind = PropertyAccessorSyntax::Kind::Get;
                    getter->body = parseExpression();
                    prop->getter = getter;
                    prop->setter = nullptr;
                    HANDLE_SEMI
                }
                else if (check(TokenKind::LeftBrace))
                {
                    parsePropertyAccessorSyntaxs(prop);
                }
                prop->location = SourceRange(fieldStartToken.location.start, previous().location.end());
                declarations.push_back(prop);
                hasProperties = true;
            }
            else
            {
                // Create regular VariableDeclSyntax for fields
                auto field = arena.make<VariableDeclSyntax>();
                field->modifiers = modifiers;

                auto ti = arena.make<TypedIdentifier>();
                ti->name = name;
                ti->type = type;
                ti->location = SourceRange(fieldStartToken.location.start, name->location.end());

                field->variable = ti;
                field->initializer = initializer;
                field->location = SourceRange(fieldStartToken.location.start, previous().location.end());
                declarations.push_back(field);
            }
        } while (consume(TokenKind::Comma));

        // Only expect semicolon for regular fields, not properties
        if (!hasProperties)
        {
            HANDLE_SEMI
        }

        // Set location for all declarations to span from the type to the end
        for (auto decl : declarations)
        {
            decl->location = SourceRange(startToken.location.start, previous().location.end());
        }

        return declarations;
    }

    void Parser::parsePropertyAccessorSyntaxs(PropertyDeclSyntax *prop)
    {
        consume(TokenKind::LeftBrace);
        while (!check(TokenKind::RightBrace) && !tokens.at_end())
        {
            auto accessorStartToken = tokens.current();
            ModifierKindFlags accessorMods = parseModifiers();

            if (consume(TokenKind::Get))
            {
                auto getter = arena.make<PropertyAccessorSyntax>();
                getter->kind = PropertyAccessorSyntax::Kind::Get;
                getter->modifiers = accessorMods;

                if (consume(TokenKind::FatArrow))
                {
                    if (check(TokenKind::LeftBrace))
                    {
                        error("Unexpected '{' after '=>' in property getter. Use either '=> expression' or '{ statements }'");
                        getter->body = withContext(Context::PROPERTY_GETTER, [this]()
                                                   { return parseBlock(); });
                    }
                    else
                    {
                        getter->body = withContext(Context::PROPERTY_GETTER, [this]()
                                                   { return parseExpression(); });
                    }
                }
                else if (check(TokenKind::LeftBrace))
                {
                    getter->body = withContext(Context::PROPERTY_GETTER, [this]()
                                               { return parseBlock(); });
                }
                else
                {
                    getter->body = std::monostate{};
                }
                getter->location = SourceRange(accessorStartToken.location.start, previous().location.end());
                prop->getter = getter;
            }
            else if (consume(TokenKind::Set))
            {
                auto setter = arena.make<PropertyAccessorSyntax>();
                setter->kind = PropertyAccessorSyntax::Kind::Set;
                setter->modifiers = accessorMods;

                if (consume(TokenKind::FatArrow))
                {
                    if (check(TokenKind::LeftBrace))
                    {
                        error("Unexpected '{' after '=>' in property setter. Use either '=> expression' or '{ statements }'");
                        setter->body = withContext(Context::PROPERTY_SETTER, [this]()
                                                   { return parseBlock(); });
                    }
                    else
                    {
                        setter->body = withContext(Context::PROPERTY_SETTER, [this]()
                                                   { return parseExpression(); });
                    }
                }
                else if (check(TokenKind::LeftBrace))
                {
                    setter->body = withContext(Context::PROPERTY_SETTER, [this]()
                                               { return parseBlock(); });
                }
                else
                {
                    setter->body = std::monostate{};
                }
                setter->location = SourceRange(accessorStartToken.location.start, previous().location.end());
                prop->setter = setter;
            }
            else
            {
                error("Expected 'get' or 'set' in property accessor");
                while (!tokens.at_end() &&
                       !check(TokenKind::Semicolon) &&
                       !check(TokenKind::RightBrace) &&
                       !check(TokenKind::Get) &&
                       !check(TokenKind::Set))
                {
                    tokens.advance();
                }
            }
            consume(TokenKind::Semicolon);
        }
        expect(TokenKind::RightBrace, "Expected '}' after property accessors");
    }

    NamespaceDeclSyntax *Parser::parseNamespaceDecl(const Token &startToken)
    {
        auto decl = arena.make<NamespaceDeclSyntax>();
        consume(TokenKind::Namespace);

        // Parse the namespace name as an expression (can be qualified)
        decl->name = parseNameExpression(); // Single identifier for now
        if (consume(TokenKind::Dot))
        {
            // Handle qualified names like "System.Collections"
            auto memberAccess = arena.make<QualifiedNameSyntax>();
            memberAccess->left = decl->name;
            memberAccess->right = parseIdentifier();
            decl->name = memberAccess;
            // Continue parsing additional dots
            while (consume(TokenKind::Dot))
            {
                auto nextMember = arena.make<QualifiedNameSyntax>();
                nextMember->left = decl->name;
                nextMember->right = parseIdentifier();
                decl->name = nextMember;
            }
        }

        if (consume(TokenKind::Semicolon))
        {
            decl->isFileScoped = true;
            decl->body = std::nullopt;
        }
        else if (consume(TokenKind::LeftBrace))
        {
            decl->isFileScoped = false;
            std::vector<BaseStmtSyntax *> statements;
            withContext(Context::NAMESPACE, [&]()
                        {
            while (!check(TokenKind::RightBrace) && !tokens.at_end()) {
                if (auto stmt = parseTopLevelStatement()) statements.push_back(stmt);
            } });
            decl->body = arena.makeList(statements);
            expect(TokenKind::RightBrace, "Expected '}' after namespace body");
        }
        else
        {
            error("Expected ';' or '{' after namespace declaration");
            decl->isFileScoped = true;
            decl->body = std::nullopt;
        }
        decl->location = SourceRange(startToken.location.start, previous().location.end());
        return decl;
    }

    #pragma endregion
    
    #pragma region Statements

    BaseStmtSyntax *Parser::parseStatement()
    {
        auto startToken = tokens.current();
        if (check(TokenKind::If))
            return parseIfStatement();
        if (check(TokenKind::While))
            return parseWhileStatement();
        if (check(TokenKind::For))
            return parseForStatement();
        if (check(TokenKind::Return))
            return parseReturnStatement();
        if (check(TokenKind::Break))
            return parseBreakStatement();
        if (check(TokenKind::Continue))
            return parseContinueStatement();
        if (check(TokenKind::LeftBrace))
            return parseBlock();
        if (tokens.current().is_modifier() || checkDeclarationStart())
        {
            return parseDeclaration();
        }

        if (check(TokenKind::Identifier))
        {
            auto checkpoint = tokens.checkpoint();
            auto type = parseTypeExpression();
            if (type && check(TokenKind::Identifier))
            {
                // TODO: This could be a typed variable declaration with multiple comma-separated variables
                // For now, just return the first one, but we should handle this better
                tokens.restore(checkpoint);
                return parseDeclaration();
            }
            tokens.restore(checkpoint);
        }

        return parseExpressionStatement();
    }

    BlockSyntax *Parser::parseBlock()
    {
        auto startToken = tokens.current();
        auto block = arena.make<BlockSyntax>();
        consume(TokenKind::LeftBrace);

        std::vector<BaseStmtSyntax *> statements;
        while (!check(TokenKind::RightBrace) && !tokens.at_end())
        {
            // Check for typed declarations that might have multiple comma-separated variables
            if (check(TokenKind::Identifier))
            {
                auto checkpoint = tokens.checkpoint();
                auto type = parseTypeExpression();
                if (type && check(TokenKind::Identifier))
                {
                    // Parse all comma-separated declarations with the same type
                    auto declarations = parseTypedMemberDeclarations(ModifierKindFlags::None, type, previous());
                    for (auto decl : declarations)
                    {
                        statements.push_back(decl);
                    }
                }
                else
                {
                    tokens.restore(checkpoint);
                    if (auto stmt = parseStatement())
                    {
                        statements.push_back(stmt);
                    }
                }
            }
            else if (auto stmt = parseStatement())
            {
                statements.push_back(stmt);
            }
        }
        block->statements = arena.makeList(statements);

        expect(TokenKind::RightBrace, "Expected '}' to close block");
        block->location = SourceRange(startToken.location.start, previous().location.end());
        return block;
    }

    BaseStmtSyntax *Parser::parseIfStatement()
    {
        auto startToken = tokens.current();
        consume(TokenKind::If);

        // Check if parentheses are present
        bool hasParens = consume(TokenKind::LeftParen);

        auto condition = parseExpression();
        if (!condition)
            condition = errorExpr("Expected condition");

        // If we had an opening paren, expect a closing one
        if (hasParens)
        {
            expect(TokenKind::RightParen, "Expected ')' after condition");
        }

        auto thenStmt = parseStatement();
        if (!thenStmt)
            thenStmt = errorStmt("Expected then statement");

        BaseStmtSyntax *elseStmt = nullptr;
        if (consume(TokenKind::Else))
        {
            elseStmt = parseStatement();
            if (!elseStmt)
                elseStmt = errorStmt("Expected else statement");
        }

        auto ifExpr = arena.make<IfStmtSyntax>();
        ifExpr->condition = condition;
        ifExpr->thenBranch = thenStmt;
        ifExpr->elseBranch = elseStmt;
        ifExpr->location = SourceRange(startToken.location.start, previous().location.end());

        return ifExpr;
    }

    WhileStmtSyntax *Parser::parseWhileStatement()
    {
        auto startToken = tokens.current();
        auto stmt = arena.make<WhileStmtSyntax>();
        consume(TokenKind::While);

        // Check if parentheses are present
        bool hasParens = consume(TokenKind::LeftParen);

        stmt->condition = parseExpression();
        if (!stmt->condition)
            stmt->condition = errorExpr("Expected condition");

        // If we had an opening paren, expect a closing one
        if (hasParens)
        {
            expect(TokenKind::RightParen, "Expected ')' after condition");
        }

        stmt->body = withContext(Context::LOOP, [this]()
                                 { return parseStatement(); });
        if (!stmt->body)
            stmt->body = errorStmt("Expected loop body");

        stmt->location = SourceRange(startToken.location.start, previous().location.end());
        return stmt;
    }

    BaseStmtSyntax *Parser::parseForStatement()
    {
        return parseTraditionalForStatement();
    }

    ForStmtSyntax *Parser::parseTraditionalForStatement()
    {
        auto startToken = tokens.current();
        auto stmt = arena.make<ForStmtSyntax>();
        consume(TokenKind::For);

        // Check if parentheses are present
        bool hasParens = consume(TokenKind::LeftParen);

        // Parse initializer
        if (!check(TokenKind::Semicolon))
        {
            stmt->initializer = parseStatement();
        }

        // Parse condition
        if (!check(TokenKind::Semicolon))
        {
            stmt->condition = parseExpression();
        }
        else
        {
            stmt->condition = nullptr;
        }
        HANDLE_SEMI

        // Parse updates
        std::vector<BaseExprSyntax *> updates;

        // If we have parentheses, parse until we hit the closing paren
        // Otherwise, parse a single expression (or none if we hit the loop body)
        if (hasParens)
        {
            while (!check(TokenKind::RightParen) && !tokens.at_end())
            {
                if (auto update = parseExpression())
                {
                    updates.push_back(update);
                }
                if (!consume(TokenKind::Comma))
                    break;
            }
        }
        else
        {
            // Without parens, only parse updates if we don't immediately see a statement starter
            if (!check(TokenKind::LeftBrace) && !tokens.current().starts_statement())
            {
                if (auto update = parseExpression())
                {
                    updates.push_back(update);
                    while (consume(TokenKind::Comma))
                    {
                        if (auto nextUpdate = parseExpression())
                        {
                            updates.push_back(nextUpdate);
                        }
                    }
                }
            }
        }
        stmt->updates = arena.makeList(updates);

        // If we had an opening paren, expect a closing one
        if (hasParens)
        {
            expect(TokenKind::RightParen, "Expected ')' after for clauses");
        }

        stmt->body = withContext(Context::LOOP, [this]()
                                 { return parseStatement(); });
        if (!stmt->body)
            stmt->body = errorStmt("Expected loop body");

        stmt->location = SourceRange(startToken.location.start, previous().location.end());
        return stmt;
    }

    ReturnStmtSyntax *Parser::parseReturnStatement()
    {
        auto startToken = tokens.current();
        auto stmt = arena.make<ReturnStmtSyntax>();
        consume(TokenKind::Return);

        if (!check(TokenKind::Semicolon))
        {
            stmt->value = parseExpression();
        }
        else
        {
            stmt->value = nullptr;
        }

        HANDLE_SEMI
        if (!inFunction() && !inGetter() && !inSetter())
        {
            warning("Return statement outside function or property");
        }
        stmt->location = SourceRange(startToken.location.start, previous().location.end());
        return stmt;
    }

    BreakStmtSyntax *Parser::parseBreakStatement()
    {
        auto startToken = tokens.current();
        auto stmt = arena.make<BreakStmtSyntax>();
        consume(TokenKind::Break);
        HANDLE_SEMI
        if (!inLoop())
        {
            warning("Break statement outside loop");
        }
        stmt->location = SourceRange(startToken.location.start, previous().location.end());
        return stmt;
    }

    ContinueStmtSyntax *Parser::parseContinueStatement()
    {
        auto startToken = tokens.current();
        auto stmt = arena.make<ContinueStmtSyntax>();
        consume(TokenKind::Continue);
        HANDLE_SEMI
        if (!inLoop())
        {
            warning("Continue statement outside loop");
        }
        stmt->location = SourceRange(startToken.location.start, previous().location.end());
        return stmt;
    }

    ExpressionStmtSyntax *Parser::parseExpressionStatement()
    {
        auto stmt = arena.make<ExpressionStmtSyntax>();
        stmt->expression = parseExpression();
        if (!stmt->expression)
        {
            stmt->expression = errorExpr("Expected expression");
        }
        HANDLE_SEMI
        stmt->location = SourceRange(stmt->expression->location.start, previous().location.end());
        return stmt;
    }

    UsingDirectiveSyntax *Parser::parseUsingDirective()
    {
        auto startToken = tokens.current();
        auto directive = arena.make<UsingDirectiveSyntax>();
        consume(TokenKind::Using);

        if (check(TokenKind::Identifier))
        {
            directive->target = parseNameExpression(); // Parse the target namespace
            if (consume(TokenKind::Dot))
            {
                // Handle qualified names like "System.Collections"
                auto memberAccess = arena.make<QualifiedNameSyntax>();
                memberAccess->left = directive->target;
                memberAccess->right = parseIdentifier();
                directive->target = memberAccess;
                // Continue parsing additional dots
                while (consume(TokenKind::Dot))
                {
                    auto nextMember = arena.make<QualifiedNameSyntax>();
                    nextMember->left = directive->target;
                    nextMember->right = parseIdentifier();
                    directive->target = nextMember;
                }
            }
        }

        HANDLE_SEMI
        directive->location = SourceRange(startToken.location.start, previous().location.end());
        return directive;
    }

    #pragma endregion
    
    #pragma region Expressions

    BaseExprSyntax *Parser::parseExpression(int minPrecedence)
    {
        auto left = parsePrimaryExpression();
        if (!left)
        {
            return nullptr;
        }
        return parseBinaryExpression(left, minPrecedence);
    }

    BaseExprSyntax *Parser::parseBinaryExpression(BaseExprSyntax *left, int minPrecedence)
    {
        while (!tokens.at_end())
        {
            Token op = tokens.current();
            int precedence = op.get_binary_precedence();

            if (op.kind == TokenKind::Question)
            {
                if (minPrecedence > precedence)
                    break;

                tokens.advance();

                auto conditional = arena.make<ConditionalExprSyntax>();
                conditional->condition = left;

                conditional->thenExpr = parseExpression(precedence + 1);
                if (!conditional->thenExpr)
                    conditional->thenExpr = errorExpr("Expected expression after '?'");

                expect(TokenKind::Colon, "Expected ':' in conditional expression");

                conditional->elseExpr = parseExpression(precedence);
                if (!conditional->elseExpr)
                    conditional->elseExpr = errorExpr("Expected expression after ':'");

                conditional->location = SourceRange(left->location.start, conditional->elseExpr->location.end());
                return conditional;
            }

            if (precedence < minPrecedence || precedence == 0)
                break;

            if (op.is_assignment_operator())
            {
                tokens.advance();
                auto assign = arena.make<AssignmentExprSyntax>();
                assign->target = left;
                assign->op = op.to_assignment_operator_kind();
                assign->value = parseExpression(precedence);
                if (!assign->value)
                    assign->value = errorExpr("Expected value in assignment");

                assign->location = SourceRange(left->location.start, assign->value->location.end());
                return assign;
            }

            tokens.advance();
            int nextPrecedence = op.is_right_associative() ? precedence : precedence + 1;

            auto right = parseExpression(nextPrecedence);
            if (!right)
                right = errorExpr("Expected right operand");

            auto binary = arena.make<BinaryExprSyntax>();
            binary->left = left;
            binary->op = op.to_binary_operator_kind();
            binary->right = right;
            binary->location = SourceRange(left->location.start, right->location.end());
            left = binary;
        }
        return left;
    }

    BaseExprSyntax *Parser::parsePrimaryExpression()
    {
        BaseExprSyntax *expr = nullptr;
        if (tokens.current().is_unary_operator())
        {
            expr = parseUnaryExpression();
        }
        else if (tokens.current().is_literal())
        {
            expr = parseLiteral();
        }
        else if (check(TokenKind::Identifier))
        {
            auto checkpoint = tokens.checkpoint();
            tokens.advance();
            if (check(TokenKind::FatArrow))
            {
                tokens.restore(checkpoint);
                expr = parseLambdaExpression();
            }
            else
            {
                tokens.restore(checkpoint);
                expr = parseNameExpression();
            }
        }
        else if (check(TokenKind::This))
        {
            auto startToken = tokens.current();
            auto thisExpr = arena.make<ThisExprSyntax>();
            tokens.advance();
            thisExpr->location = startToken.location;
            expr = thisExpr;
        }
        else if (check(TokenKind::LeftParen))
        {
            expr = parseParenthesizedOrLambda();
        }
        else if (check(TokenKind::LeftBracket))
        {
            expr = parseArrayLiteral();
        }
        else if (check(TokenKind::New))
        {
            expr = parseNewExpression();
        }
        else if (check(TokenKind::Typeof))
        {
            expr = parseTypeOfExpression();
        }
        else if (check(TokenKind::Sizeof))
        {
            expr = parseSizeOfExpression();
        }

        if (!expr)
        {
            return nullptr;
        }

        return parsePostfixExpression(expr);
    }

    BaseExprSyntax *Parser::parsePostfixExpression(BaseExprSyntax *expr)
    {
        while (true)
        {
            if (check(TokenKind::LeftParen))
            {
                tokens.advance();
                auto call = arena.make<CallExprSyntax>();
                call->callee = expr;

                std::vector<BaseExprSyntax *> args;
                while (!check(TokenKind::RightParen) && !tokens.at_end())
                {
                    auto arg = parseExpression();
                    if (!arg)
                        break;
                    args.push_back(arg);
                    if (!consume(TokenKind::Comma))
                        break;
                    if (check(TokenKind::RightParen))
                        break;
                }
                call->arguments = arena.makeList(args);

                expect(TokenKind::RightParen, "Expected ')' after arguments");
                call->location = SourceRange(expr->location.start, previous().location.end());
                expr = call;
            }
            else if (check(TokenKind::Dot))
            {
                tokens.advance();
                auto member = arena.make<QualifiedNameSyntax>();
                member->left = expr; 
                member->right = parseIdentifier();
                member->location = SourceRange(expr->location.start, member->right->location.end());
                expr = member;
            }
            else if (check(TokenKind::LeftBracket))
            {
                tokens.advance();

                // Regular indexer expression (arr[index])
                auto indexer = arena.make<IndexerExprSyntax>();
                indexer->object = expr;
                indexer->index = parseExpression();
                if (!indexer->index)
                    indexer->index = errorExpr("Expected index expression");

                expect(TokenKind::RightBracket, "Expected ']' after index");
                indexer->location = SourceRange(expr->location.start, previous().location.end());
                expr = indexer;
            }
            else if (checkAny({TokenKind::Increment, TokenKind::Decrement}))
            {
                auto unary = arena.make<UnaryExprSyntax>();
                unary->operand = expr;
                unary->op = tokens.current().to_unary_operator_kind();
                unary->isPostfix = true;
                tokens.advance();
                unary->location = SourceRange(expr->location.start, previous().location.end());
                expr = unary;
            }
            else
            {
                break;
            }
        }
        return expr;
    }

    BaseExprSyntax *Parser::parseUnaryExpression()
    {
        auto startToken = tokens.current();
        auto unary = arena.make<UnaryExprSyntax>();
        Token op = tokens.current();
        tokens.advance();
        unary->op = op.to_unary_operator_kind();
        unary->isPostfix = false;
        unary->operand = parseExpression(op.get_unary_precedence());
        if (!unary->operand)
        {
            unary->operand = errorExpr("Expected operand after unary operator");
        }
        unary->location = SourceRange(startToken.location.start, unary->operand->location.end());
        return unary;
    }

    LiteralExprSyntax *Parser::parseLiteral()
    {
        auto lit = arena.make<LiteralExprSyntax>();
        Token tok = tokens.current();
        lit->location = tok.location;
        lit->value = tok.text;
        lit->kind = tok.to_literal_kind();
        tokens.advance();
        return lit;
    }

        BaseNameExprSyntax *Parser::parseNameExpression()
        {
            BaseNameExprSyntax *nameExpr = parseIdentifier();
            if (!nameExpr)
            {
                return nullptr;
            }

            // Check for generic arguments: identifier<T, U>
            if (check(TokenKind::Less))
            {
                auto genericArgs = parseGenericArgs();
                if (!genericArgs.empty())
                {
                    auto generic = arena.make<GenericNameSyntax>();
                    generic->identifier = nameExpr;
                    generic->typeArguments = genericArgs;
                    generic->location = SourceRange(nameExpr->location.start, previous().location.end());
                    nameExpr = generic;
                }
            }

            // Recursively handle qualified names: left.right.more...
            if (consume(TokenKind::Dot))
            {
                auto right = parseNameExpression(); // Recursive call
                if (!right)
                {
                    error("Expected identifier after '.'");
                    return nameExpr;
                }

                auto qualified = arena.make<QualifiedNameSyntax>();
                qualified->left = nameExpr;
                qualified->right = right;
                qualified->location = SourceRange(nameExpr->location.start, right->location.end());
                return qualified;
            }

            return nameExpr;
        }

    List<BaseExprSyntax *> Parser::parseGenericArgs()
    {
        auto cp = tokens.checkpoint();
        if (!check(TokenKind::Less))
            return {};

        consume(TokenKind::Less);
        std::vector<BaseExprSyntax *> typeArgs;

        while (!check(TokenKind::Greater) && !tokens.at_end())
        {
            auto typeArg = parseTypeExpression();
            if (!typeArg)
                break;
            typeArgs.push_back(typeArg);

            if (!consume(TokenKind::Comma))
                break;
        }

        if (!check(TokenKind::Greater))
        {
            tokens.restore(cp);
            return {};
        }
        
        consume(TokenKind::Greater);

        return arena.makeList(typeArgs);
    }

    BaseExprSyntax *Parser::parseTypeExpression()
    {
        if (!check(TokenKind::Identifier))
        {
            return nullptr; // Don't error, just return null
        }

        auto nameExpr = parseNameExpression();
        BaseExprSyntax *baseType = nameExpr;

        // Parse pointer and array type suffixes in a loop
        // This allows combinations like: void*[], int***, MyType[]*, etc.
        while (true)
        {
            // Check for pointer syntax: *
            if (consume(TokenKind::Asterisk))
            {
                auto pointerType = arena.make<PointerTypeSyntax>();
                pointerType->baseType = baseType;
                pointerType->location = SourceRange(baseType->location.start, previous().location.end());
                baseType = pointerType;
                continue; // Check for more suffixes
            }

            // Speculatively check for array type syntax
            if (check(TokenKind::LeftBracket))
            {
                auto checkpoint = tokens.checkpoint();
                tokens.advance(); // consume [

                // Check if this looks like array type syntax (empty or literal)
                bool isArrayType = false;
                if (check(TokenKind::RightBracket))
                {
                    isArrayType = true; // Empty brackets: Type[]
                }
                else if (check(TokenKind::LiteralI32) || check(TokenKind::LiteralI64) || check(TokenKind::LiteralI8))
                {
                    tokens.advance(); // consume literal
                    if (check(TokenKind::RightBracket))
                    {
                        isArrayType = true; // Literal size: Type[10]
                    }
                }

                if (!isArrayType)
                {
                    // Not array type syntax, restore and return base type
                    tokens.restore(checkpoint);
                    break; // Exit loop
                }

                // It is array type syntax, restore and parse properly
                tokens.restore(checkpoint);
                consume(TokenKind::LeftBracket);

                auto arrayType = arena.make<ArrayTypeSyntax>();
                arrayType->baseType = baseType;

                if (check(TokenKind::LiteralI32) || check(TokenKind::LiteralI64) || check(TokenKind::LiteralI8))
                {
                    arrayType->size = parseLiteral();
                }

                if (!consume(TokenKind::RightBracket))
                {
                    // If we determined it was array type syntax but can't parse it,
                    // return null to indicate parse failure
                    return nullptr;
                }

                arrayType->location = SourceRange(baseType->location.start, previous().location.end());
                baseType = arrayType;
                continue; // Check for more suffixes
            }

            // No more type suffixes
            break;
        }

        return baseType;
    }

    BaseExprSyntax *Parser::parseParenthesizedOrLambda()
    {
        auto checkpoint = tokens.checkpoint();
        consume(TokenKind::LeftParen);

        // Check if this might be a cast expression
        // A cast looks like (Type)expr where Type is an identifier that could be a type
        bool isCast = false;
        bool isLambda = false;
        int parenDepth = 1;

        // First, check if this could be a cast by looking for a type pattern
        if (check(TokenKind::Identifier))
        {
            auto typeCheckpoint = tokens.checkpoint();

            // Try to parse as type expression
            auto potentialType = parseTypeExpression();

            // If we successfully parsed a type and the next token is ')', it's likely a cast
            if (potentialType && check(TokenKind::RightParen))
            {
                tokens.advance(); // consume the ')'

                // Check what follows - if it's an expression-starting token, it's a cast
                if (tokens.current().starts_expression() &&
                    !check(TokenKind::Semicolon) &&
                    !checkAny({TokenKind::Comma, TokenKind::RightParen, TokenKind::RightBracket}))
                {
                    isCast = true;
                }
            }

            tokens.restore(typeCheckpoint);
        }

        if (!isCast)
        {
            // Original lambda detection logic
            while (!tokens.at_end() && parenDepth > 0)
            {
                if (check(TokenKind::LeftParen))
                    parenDepth++;
                else if (check(TokenKind::RightParen))
                {
                    parenDepth--;
                    if (parenDepth == 0)
                    {
                        tokens.advance();
                        if (check(TokenKind::FatArrow))
                            isLambda = true;
                        break;
                    }
                }
                else if (check(TokenKind::FatArrow) && parenDepth == 1)
                {
                    isLambda = true;
                    break;
                }
                tokens.advance();
            }
        }

        tokens.restore(checkpoint);

        if (isCast)
        {
            return parseCastExpression();
        }
        else if (isLambda)
        {
            return parseLambdaExpression();
        }
        else
        {
            consume(TokenKind::LeftParen);
            if (check(TokenKind::RightParen))
            {
                error("Empty parentheses are not a valid expression");
                consume(TokenKind::RightParen);
                return errorExpr("Expected expression in parentheses");
            }

            auto expr = parseExpression();
            if (!expr)
                return errorExpr("Expected expression in parentheses");

            expect(TokenKind::RightParen, "Expected ')' after expression");
            return expr;
        }
    }

    BaseExprSyntax *Parser::parseCastExpression()
    {
        auto startToken = tokens.current();
        consume(TokenKind::LeftParen);

        // Parse the target type
        auto targetType = parseTypeExpression();
        if (!targetType)
        {
            error("Expected type in cast expression");
            targetType = errorExpr("Expected cast type");
        }

        expect(TokenKind::RightParen, "Expected ')' after cast type");

        // Parse the expression to cast - use higher precedence to ensure we don't
        // accidentally grab binary operators that should apply after the cast
        auto expr = parseExpression(14); // Use unary precedence level
        if (!expr)
        {
            expr = errorExpr("Expected expression after cast");
        }

        auto cast = arena.make<CastExprSyntax>();
        cast->targetType = targetType;
        cast->expression = expr;
        cast->location = SourceRange(startToken.location.start, expr->location.end());
        return cast;
    }

    BaseExprSyntax *Parser::parseArrayLiteral()
    {
        auto startToken = tokens.current();
        auto array = arena.make<ArrayLiteralExprSyntax>();
        consume(TokenKind::LeftBracket);

        std::vector<BaseExprSyntax *> elements;
        while (!check(TokenKind::RightBracket) && !tokens.at_end())
        {
            if (auto elem = parseExpression())
            {
                elements.push_back(elem);
            }
            if (!consume(TokenKind::Comma))
                break;
        }
        array->elements = arena.makeList(elements);

        expect(TokenKind::RightBracket, "Expected ']' after array elements");
        array->location = SourceRange(startToken.location.start, previous().location.end());
        return array;
    }

    BaseExprSyntax *Parser::parseNewExpression()
    {
        auto startToken = tokens.current();
        auto newExpr = arena.make<NewExprSyntax>();
        consume(TokenKind::New);

        // Parse constructor type - use parseTypeExpression to handle generics properly
        if (check(TokenKind::Identifier))
        {
            newExpr->type = parseTypeExpression();
            if (!newExpr->type)
            {
                newExpr->type = errorExpr("Expected type name after 'new'");
            }
        }
        else
        {
            newExpr->type = errorExpr("Expected type name after 'new'");
        }

        if (check(TokenKind::LeftParen))
        {
            consume(TokenKind::LeftParen);
            std::vector<BaseExprSyntax *> args;
            while (!check(TokenKind::RightParen) && !tokens.at_end())
            {
                if (auto arg = parseExpression())
                    args.push_back(arg);
                if (!consume(TokenKind::Comma))
                    break;
            }
            newExpr->arguments = arena.makeList(args);
            expect(TokenKind::RightParen, "Expected ')' after constructor arguments");
        }
        else
        {
            newExpr->arguments = arena.emptyList<BaseExprSyntax *>();
        }
        newExpr->location = SourceRange(startToken.location.start, previous().location.end());
        return newExpr;
    }

    BaseExprSyntax *Parser::parseLambdaExpression()
    {
        auto startToken = tokens.current();
        auto lambda = arena.make<LambdaExprSyntax>();

        if (check(TokenKind::LeftParen))
        {
            consume(TokenKind::LeftParen);
            std::vector<ParameterDeclSyntax *> params;
            while (!check(TokenKind::RightParen) && !tokens.at_end())
            {
                auto param = arena.make<ParameterDeclSyntax>();
                auto paramStart = tokens.current();
                param->param = parseTypedIdentifier();
                if (!param->param)
                {
                    auto ti = arena.make<TypedIdentifier>();
                    ti->type = errorExpr("Expected parameter type");
                    ti->name = arena.makeIdentifier(Token::invalid_token(tokens.current()));
                    param->param = ti;
                }
                param->defaultValue = nullptr;
                param->location = SourceRange(paramStart.location.start, previous().location.end());
                params.push_back(param);
                if (!consume(TokenKind::Comma))
                    break;
            }
            lambda->parameters = arena.makeList(params);
            expect(TokenKind::RightParen, "Expected ')' after lambda parameters");
        }
        else
        {
            std::vector<ParameterDeclSyntax *> params;
            auto param = arena.make<ParameterDeclSyntax>();
            auto paramStart = tokens.current();
            auto ti = arena.make<TypedIdentifier>();
            ti->type = nullptr;
            ti->name = parseIdentifier();
            ti->location = ti->name->location;
            param->param = ti;
            param->defaultValue = nullptr;
            param->location = paramStart.location;
            params.push_back(param);
            lambda->parameters = arena.makeList(params);
        }

        expect(TokenKind::FatArrow, "Expected '=>' after lambda parameters");

        if (check(TokenKind::LeftBrace))
        {
            lambda->body = parseBlock();
        }
        else
        {
            auto expr = parseExpression();
            if (!expr)
                expr = errorExpr("Expected lambda body");
            auto exprStmt = arena.make<ExpressionStmtSyntax>();
            exprStmt->expression = expr;
            exprStmt->location = expr->location;
            lambda->body = exprStmt;
        }
        lambda->location = SourceRange(startToken.location.start, previous().location.end());
        return lambda;
    }

    BaseExprSyntax *Parser::parseTypeOfExpression()
    {
        auto startToken = tokens.current();
        auto typeOf = arena.make<TypeOfExprSyntax>();
        consume(TokenKind::Typeof);
        expect(TokenKind::LeftParen, "Expected '(' after 'typeof'");

        typeOf->type = parseTypeExpression();
        if (!typeOf->type)
            typeOf->type = errorExpr("Expected type");

        expect(TokenKind::RightParen, "Expected ')' after type");
        typeOf->location = SourceRange(startToken.location.start, previous().location.end());
        return typeOf;
    }

    BaseExprSyntax *Parser::parseSizeOfExpression()
    {
        auto startToken = tokens.current();
        auto sizeOf = arena.make<SizeOfExprSyntax>();
        consume(TokenKind::Sizeof);
        expect(TokenKind::LeftParen, "Expected '(' after 'sizeof'");

        sizeOf->type = parseTypeExpression();
        if (!sizeOf->type)
            sizeOf->type = errorExpr("Expected type");

        expect(TokenKind::RightParen, "Expected ')' after type");
        sizeOf->location = SourceRange(startToken.location.start, previous().location.end());
        return sizeOf;
    }

    #pragma endregion
    
    #pragma region Helper Functions

    BaseNameExprSyntax *Parser::parseIdentifier()
    {
        if (!check(TokenKind::Identifier))
        {
            return nullptr;
        }

        auto tok = tokens.current();
        auto id = arena.makeIdentifier(tok);
        id->location = tok.location;
        tokens.advance();

        return id;
    }

    TypedIdentifier *Parser::parseTypedIdentifier()
    {
        auto startToken = tokens.current();
        auto ti = arena.make<TypedIdentifier>();
        if (consume(TokenKind::Var))
        {
            ti->type = nullptr;
            ti->name = parseIdentifier();
        }
        else
        {
            ti->type = parseTypeExpression();
            if (!ti->type)
            {
                error("Expected type specification");
                ti->type = errorExpr("Expected type");
            }
            ti->name = parseIdentifier();
        }
        ti->location = SourceRange(startToken.location.start, previous().location.end());
        return ti;
    }

    List<ParameterDeclSyntax *> Parser::parseParameterList()
    {
        expect(TokenKind::LeftParen, "Expected '(' for parameter list");

        std::vector<ParameterDeclSyntax *> params;
        while (!check(TokenKind::RightParen) && !tokens.at_end())
        {
            auto startToken = tokens.current();
            auto param = arena.make<ParameterDeclSyntax>();
            param->param = parseTypedIdentifier();
            if (!param->param)
            {
                auto ti = arena.make<TypedIdentifier>();
                ti->type = errorExpr("Expected parameter type");
                ti->name = arena.makeIdentifier(Token::invalid_token(tokens.current()));
                param->param = ti;
            }

            if (consume(TokenKind::Assign))
            {
                param->defaultValue = parseExpression();
            }
            else
            {
                param->defaultValue = nullptr;
            }
            param->location = SourceRange(startToken.location.start, previous().location.end());
            params.push_back(param);
            if (!consume(TokenKind::Comma))
                break;
        }

        expect(TokenKind::RightParen, "Expected ')' after parameters");
        return arena.makeList(params);
    }

    List<TypeParameterDeclSyntax *> Parser::parseTypeParameterList()
    {
        std::vector<TypeParameterDeclSyntax *> typeParams;

        expect(TokenKind::Less, "Expected '<' to start type parameter list");

        while (!check(TokenKind::Greater) && !tokens.at_end())
        {
            auto startToken = tokens.current();
            auto typeParam = arena.make<TypeParameterDeclSyntax>();
            typeParam->name = parseIdentifier();
            typeParam->location = SourceRange(startToken.location.start, previous().location.end());
            typeParams.push_back(typeParam);

            if (!consume(TokenKind::Comma))
                break;
        }

        expect(TokenKind::Greater, "Expected '>' to close type parameter list");
        return arena.makeList(typeParams);
    }

    List<BaseExprSyntax *> Parser::parseBaseTypeList()
    {
        std::vector<BaseExprSyntax *> types;
        do
        {
            if (auto type = parseTypeExpression())
            {
                types.push_back(type);
            }
        } while (consume(TokenKind::Comma));
        return arena.makeList(types);
    }

    bool Parser::isExpressionTerminator()
    {
        return checkAny({TokenKind::Semicolon, TokenKind::RightParen,
                         TokenKind::RightBracket, TokenKind::RightBrace,
                         TokenKind::Comma, TokenKind::Colon, TokenKind::Assign, TokenKind::ThinArrow,
                         TokenKind::FatArrow, TokenKind::EndOfFile});
    }

    bool Parser::isPatternTerminator()
    {
        return checkAny({TokenKind::FatArrow, TokenKind::Comma,
                         TokenKind::RightParen, TokenKind::RightBrace});
    }

    bool Parser::isOnSameLine(const Token &prev, const Token &curr) const
    {
        // Check if two tokens are on the same line
        // Assuming SourceLocation has a line field or similar
        return prev.location.end().line == curr.location.start.line;
    }

    bool Parser::requireSemicolonIfSameLine()
    {
// This replaces the HANDLE_SEMI macro logic for line-aware semicolon handling
#ifdef REQUIRE_SEMI
        return expect(TokenKind::Semicolon, "Expected ';' to end statement");
#else
        // Check if we have a semicolon
        if (consume(TokenKind::Semicolon))
        {
            return true; // Semicolon was present and consumed
        }

        // No semicolon found - check if next token is on same line
        if (!tokens.at_end())
        {
            Token prev = tokens.previous();
            Token curr = tokens.current();

            // If the next token is on the same line as the end of the previous statement,
            // we require a semicolon
            if (isOnSameLine(prev, curr))
            {
                error("Expected ';' between statements on the same line");
                return false;
            }
        }
        return true; // No semicolon needed - statements on different lines
#endif
    }
    #pragma endregion

} // namespace Fern