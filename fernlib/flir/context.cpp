#include <flir/context.hpp>

#include <flir/lower.hpp>
#include <semantic/context.hpp>

namespace Fern
{

FlirMethod* FlirContext::lower_single_method(SemanticContext& semantic, MethodSymbol* method)
{
    if (!method) return nullptr;

    auto it = loweredMethods.find(method);
    if (it != loweredMethods.end()) return it->second;

    auto* fhir = semantic.bind_single_method(method);
    if (!fhir) return nullptr;

    FlirLowerer lowerer(semantic, *this);
    auto* result = lowerer.lower_method(fhir);

    loweredMethods[method] = result;
    if (result) methods.push_back(result);
    return result;
}

}
