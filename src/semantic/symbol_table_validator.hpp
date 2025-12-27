#pragma once

#include "symbol_table.hpp"
#include "common/error.hpp"

namespace Fern
{

class SymbolTableValidator : public DiagnosticSystem
{
public:
    SymbolTableValidator() : DiagnosticSystem("SymbolTableValidator") {}

    // Validation before type resolution
    public:
    void validate_untyped(SymbolTable& symbols);

    private:
    void validate_container_untyped(ContainerSymbol* container);
    void validate_type_member_names(TypeSymbol* type_sym);

    // Validate after type resolution
    public:
    void validate_typed(SymbolTable& symbols);

    private:
    void validate_container_typed(ContainerSymbol* container);
};

} // namespace Fern
