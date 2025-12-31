// llvm_backend.cpp - LLVM Backend Implementation
#include "llvm_backend.hpp"
#include "llvm_codegen.hpp"
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/TargetParser/Host.h>
#include <iostream>

namespace Fern
{

    LLVMBackend::LLVMBackend()
        : Backend("LLVMBackend")
    {
    }

    bool LLVMBackend::lower(FLIR::Module* flir_module)
    {
        if (!flir_module)
        {
            error("Cannot lower null FLIR module", SourceRange());
            return false;
        }

        try
        {
            context = std::make_unique<llvm::LLVMContext>();
            module_name = flir_module->name;

            FLIRCodeGen codegen(*context, module_name);
            module = codegen.lower(flir_module);

            for (const auto& diag : codegen.get_diagnostics())
            {
                report(diag);
            }

            return module != nullptr && !has_errors();
        }
        catch (const std::exception& e)
        {
            error("LLVM code generation error: " + std::string(e.what()), SourceRange());
            return false;
        }
    }

    void LLVMBackend::initialize_targets()
    {
        static bool initialized = false;
        if (initialized) return;

        LLVMInitializeX86TargetInfo();
        LLVMInitializeX86Target();
        LLVMInitializeX86TargetMC();
        LLVMInitializeX86AsmPrinter();
        LLVMInitializeX86AsmParser();

        initialized = true;
    }

    bool LLVMBackend::write_ir(const std::string& filename) const
    {
        if (!is_valid())
        {
            std::cerr << "Cannot write IR: module is invalid\n";
            return false;
        }

        std::error_code EC;
        llvm::raw_fd_ostream output(filename, EC, llvm::sys::fs::OF_None);

        if (EC)
        {
            std::cerr << "Could not open file " << filename << ": " << EC.message() << "\n";
            return false;
        }

        module->print(output, nullptr);
        return true;
    }

    std::string LLVMBackend::get_ir_string() const
    {
        if (!is_valid())
            return "";

        std::string ir_str;
        llvm::raw_string_ostream stream(ir_str);
        module->print(stream, nullptr);
        return stream.str();
    }

    void LLVMBackend::dump_ir() const
    {
        if (!is_valid())
        {
            std::cerr << "Cannot dump IR: module is invalid\n";
            return;
        }

        std::cout << "\n=== LLVM IR ===\n";
        module->print(llvm::outs(), nullptr);
        std::cout << "\n===============\n";
    }

    bool LLVMBackend::write_object_file(const std::string& filename) const
    {
        if (!is_valid())
        {
            std::cerr << "Cannot generate object file: module is invalid\n";
            return false;
        }

        initialize_targets();

        // Clone module since we need to modify it
        auto cloned_module = llvm::CloneModule(*module);

        // Get target triple
        auto target_triple = llvm::sys::getDefaultTargetTriple();
        cloned_module->setTargetTriple(target_triple);

        // Get target
        std::string error;
        auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);

        if (!target)
        {
            std::cerr << "Target lookup failed: " << error << "\n";
            return false;
        }

        // Create target machine
        auto CPU = "generic";
        auto features = "";
        llvm::TargetOptions opt;
        auto RM = std::optional<llvm::Reloc::Model>();
        auto target_machine = target->createTargetMachine(
            target_triple, CPU, features, opt, RM);

        cloned_module->setDataLayout(target_machine->createDataLayout());

        // Open output file
        std::error_code EC;
        llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);

        if (EC)
        {
            std::cerr << "Could not open file: " << EC.message() << "\n";
            return false;
        }

        // Generate object code
        llvm::legacy::PassManager pass;
        auto file_type = llvm::CodeGenFileType::ObjectFile;

        if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type))
        {
            std::cerr << "Target machine can't emit object file\n";
            return false;
        }

        pass.run(*cloned_module);
        dest.flush();

        return true;
    }

    bool LLVMBackend::write_assembly(const std::string& filename) const
    {
        if (!is_valid())
        {
            std::cerr << "Cannot generate assembly: module is invalid\n";
            return false;
        }

        initialize_targets();

        auto cloned_module = llvm::CloneModule(*module);
        auto target_triple = llvm::sys::getDefaultTargetTriple();
        cloned_module->setTargetTriple(target_triple);

        std::string error;
        auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);
        if (!target)
        {
            std::cerr << "Target lookup failed: " << error << "\n";
            return false;
        }

        auto CPU = "generic";
        auto features = "";
        llvm::TargetOptions opt;
        auto RM = std::optional<llvm::Reloc::Model>();
        auto target_machine = target->createTargetMachine(
            target_triple, CPU, features, opt, RM);

        cloned_module->setDataLayout(target_machine->createDataLayout());

        std::error_code EC;
        llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);
        if (EC)
        {
            std::cerr << "Could not open file: " << EC.message() << "\n";
            return false;
        }

        llvm::legacy::PassManager pass;
        auto file_type = llvm::CodeGenFileType::AssemblyFile;

        if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type))
        {
            std::cerr << "Target machine can't emit assembly file\n";
            return false;
        }

        pass.run(*cloned_module);
        dest.flush();

        return true;
    }

    #pragma region Backend Interface

    std::optional<float> LLVMBackend::execute(const std::string& function_name)
    {
        return execute_jit<float>(function_name);
    }

    void LLVMBackend::dump() const
    {
        dump_ir();
    }

    std::string LLVMBackend::get_dump_string() const
    {
        return get_ir_string();
    }

} // namespace Fern
