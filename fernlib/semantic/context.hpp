#pragma once

#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include <common/diagnostic.hpp>
#include <symbol/table.hpp>
#include <token/token.hpp>

namespace Fern
{

class AllocArena;
class Binder;
class RootBinder;

struct FhirMethod;
struct NamespaceSymbol;
struct NamedTypeSymbol;
struct MethodSymbol;

struct SemanticContext
{
    SemanticContext(AllocArena& arena, DiagnosticSystem& diagnostics);
    ~SemanticContext();

    SemanticContext(const SemanticContext&) = delete;
    SemanticContext& operator=(const SemanticContext&) = delete;

    AllocArena& arena;
    DiagnosticSystem& diagnostics;

    SymbolTable symbols;
    std::vector<FhirMethod*> methods;

    std::unique_ptr<RootBinder> rootBinder;
    std::unordered_map<NamespaceSymbol*, std::unique_ptr<Binder>> nsBinders;
    std::unordered_map<NamedTypeSymbol*, std::unique_ptr<Binder>> typeBinders;
    std::unordered_map<MethodSymbol*, std::unique_ptr<Binder>> methodBinders;
    std::unordered_map<MethodSymbol*, FhirMethod*> boundMethods;

    Binder& namespace_binder(NamespaceSymbol* ns);
    Binder& type_binder(NamedTypeSymbol* type);
    Binder& method_binder(MethodSymbol* method);

    FhirMethod* bind_single_method(MethodSymbol* method);

    TypeSymbol* resolve_type_name(Token name);
    TypeSymbol* resolve_type_name(std::string_view alias);

    std::string format() const;

private:
    FhirMethod* bind_method(MethodSymbol* method);
    FhirMethod* lower_synthetic_constructor(MethodSymbol* method, NamedTypeSymbol* parentType);
};

}
