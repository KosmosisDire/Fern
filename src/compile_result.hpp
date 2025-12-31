// compile_result.hpp - Compilation Result
#pragma once

#include "flir/flir.hpp"
#include "common/error.hpp"
#include "common/source_database.hpp"
#include <memory>
#include <string>
#include <vector>

namespace Fern
{
    class SymbolTable;
    class TypeSystem;

    struct CompileResult
    {
        std::unique_ptr<FLIR::Module> module;
        std::vector<Diagnostic> diagnostics;
        std::shared_ptr<SourceDatabase> source_db;

        // Keep symbol table alive - FLIR functions reference symbols
        std::unique_ptr<SymbolTable> symbols;
        std::unique_ptr<TypeSystem> type_system;

        bool has_errors() const
        {
            for (const auto& d : diagnostics)
            {
                if (d.severity == Diagnostic::Severity::Error ||
                    d.severity == Diagnostic::Severity::Fatal)
                    return true;
            }
            return false;
        }

        bool is_valid() const { return module != nullptr && !has_errors(); }

        size_t error_count() const
        {
            size_t count = 0;
            for (const auto& d : diagnostics)
            {
                if (d.severity == Diagnostic::Severity::Error ||
                    d.severity == Diagnostic::Severity::Fatal)
                    count++;
            }
            return count;
        }

        std::string format_diagnostic(const Diagnostic& diag) const
        {
            if (source_db)
            {
                auto filename = source_db->get_path(diag.location.start.file_id);
                if (!filename.empty())
                    return diag.to_string(filename);
            }
            return diag.to_string();
        }
    };

} // namespace Fern
