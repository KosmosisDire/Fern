// jit.cpp - LLVM JIT Execution Engine Implementation
#include "jit.hpp"
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/Support/TargetSelect.h>
#include <iostream>

namespace Fern
{

    JIT::JIT()
    {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();

        auto jit_expected = llvm::orc::LLJITBuilder().create();
        if (!jit_expected)
        {
            llvm::errs() << "Failed to create JIT: "
                         << llvm::toString(jit_expected.takeError()) << "\n";
            exit(1);
        }
        jit = std::move(*jit_expected);

        auto &main_dylib = jit->getMainJITDylib();

        auto generator = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
            jit->getDataLayout().getGlobalPrefix());
        if (!generator)
        {
            llvm::errs() << "Failed to create symbol generator: "
                         << llvm::toString(generator.takeError()) << "\n";
            exit(1);
        }
        main_dylib.addGenerator(std::move(*generator));
    }

    bool JIT::load_library(const std::string &path)
    {
        auto &main_dylib = jit->getMainJITDylib();

        auto generator = llvm::orc::DynamicLibrarySearchGenerator::Load(
            path.c_str(),
            jit->getDataLayout().getGlobalPrefix());

        if (!generator)
        {
            llvm::errs() << "Failed to load library '" << path << "': "
                         << llvm::toString(generator.takeError()) << "\n";
            return false;
        }

        main_dylib.addGenerator(std::move(*generator));
        return true;
    }

    bool JIT::load_libraries(const std::vector<std::string> &paths)
    {
        for (const auto &path : paths)
        {
            if (!load_library(path))
                return false;
        }
        return true;
    }

    bool JIT::add_module(std::unique_ptr<llvm::Module> module,
                         std::unique_ptr<llvm::LLVMContext> context)
    {
        auto err = jit->addIRModule(
            llvm::orc::ThreadSafeModule(std::move(module), std::move(context)));

        if (err)
        {
            llvm::errs() << "Failed to add module: "
                         << llvm::toString(std::move(err)) << "\n";
            return false;
        }
        return true;
    }

    llvm::Expected<llvm::orc::ExecutorAddr> JIT::lookup(const std::string &name)
    {
        return jit->lookup(name);
    }

} // namespace Fern
