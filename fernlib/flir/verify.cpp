#include <flir/verify.hpp>

#include <common/diagnostic.hpp>
#include <flir/builder.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

#pragma region Address Predicate

// True for nodes that produce an address, seen through a sequence that yields one. A call to a ref
// returning method yields the place it returns.
static bool is_address(FlirExpr* node)
{
    if (!node) return false;
    if (node->is<FlirLocalAddr>() || node->is<FlirFieldAddr>() || node->is<FlirElemAddr>()) return true;
    if (auto* seq = node->as<FlirSequence>()) return is_address(seq->value);
    if (auto* call = node->as<FlirCall>()) return call->method && call->method->returnsRef;
    return false;
}

#pragma region Verifier

namespace
{

class VerifyVisitor : public DefaultFlirVisitor
{
public:
    VerifyVisitor(FlirMethod* method, Diagnostics& diag) : method(method), diag(diag) {}

private:
    FlirMethod* method;
    Diagnostics& diag;

    std::string_view name() const
    {
        if (method && method->symbol) return method->symbol->name;
        return "?";
    }

    void fail(FlirNode* node, std::string_view what)
    {
        diag.report(DiagnosticCode::Err_FlirMalformed, node ? node->span : Span{}, name(), what);
    }

    void on_visit(FlirNode* node) override
    {
        if (auto* n = node->as<FlirLoad>())
        {
            if (!is_address(n->address)) fail(n, "load address is not an address");
            if (is_memory_value(n->type)) fail(n, "load of a value type");
        }
        else if (auto* n = node->as<FlirStore>())
        {
            if (!is_address(n->address)) fail(n, "store address is not an address");
            else if (is_memory_value(n->address->type)) fail(n, "store to a value type address");
        }
        else if (auto* n = node->as<FlirCopy>())
        {
            if (!is_address(n->dest)) fail(n, "copy dest is not an address");
            if (!is_address(n->src)) fail(n, "copy src is not an address");
            if (!is_memory_value(n->type)) fail(n, "copy of a type that moves as a value");
            auto* named = n->type ? n->type->as<NamedTypeSymbol>() : nullptr;
            if (named && named->layoutState != LayoutState::Computed) fail(n, "copy type has no computed layout");
        }
        else if (auto* n = node->as<FlirLocalAddr>())
        {
            if (!n->local) fail(n, "local address has no slot");
        }
        else if (auto* n = node->as<FlirFieldAddr>())
        {
            if (!n->field) fail(n, "field address has no field");
            else if (n->field->offset < 0) fail(n, "field has no offset");
        }
        else if (auto* n = node->as<FlirElemAddr>())
        {
            if (!n->base) fail(n, "element address has no base");
            if (!n->index) fail(n, "element address has no index");
            auto* elem = n->elemType ? n->elemType->as<NamedTypeSymbol>() : nullptr;
            if (!elem) fail(n, "element address has no element type");
            else if (elem->layoutState != LayoutState::Computed) fail(n, "element type has no computed layout");
        }
        else if (auto* n = node->as<FlirCall>())
        {
            if (n->method && n->method->is_intrinsic()) fail(n, "call to an intrinsic method");
            if (n->method && n->method->returnsRef && n->resultDest) fail(n, "ref returning call with a result destination");
        }
        else if (auto* n = node->as<FlirReturn>())
        {
            bool refMethod = method && method->symbol && method->symbol->returnsRef;
            if (refMethod && !is_address(n->value)) fail(n, "ref return of a non address");
        }
        else if (auto* n = node->as<FlirIntrinsic>())
        {
            if (!n->method) fail(n, "intrinsic has no method");
            else if (!n->method->is_intrinsic()) fail(n, "intrinsic node has a non intrinsic method");
            else if (n->method->is_constructor())
            {
                if (n->thisArg) fail(n, "intrinsic constructor with a this argument");
                if (!n->type) fail(n, "intrinsic constructor with no type");
            }
        }
        else if (auto* n = node->as<FlirCast>())
        {
            if (!n->method) fail(n, "cast has no method");
            else if (!n->method->is_intrinsic()) fail(n, "cast with a non intrinsic method");
        }
        else if (auto* n = node->as<FlirAlloc>())
        {
            auto* named = n->allocType ? n->allocType->as<NamedTypeSymbol>() : nullptr;
            if (!n->allocType) fail(n, "alloc of a null type");
            else if (named && named->is_builtin()) fail(n, "alloc of a builtin type");
            else if (!named || !named->is_ref()) fail(n, "alloc of a non ref type");
        }
    }
};

}

void FlirVerifier::verify(FlirMethod* method, Diagnostics& diag)
{
    if (!method) return;

    std::string_view mname = method->symbol ? std::string_view(method->symbol->name) : std::string_view("?");
    auto check_slot = [&](FlirLocal* slot)
    {
        if (slot && !slot->byAddress && slot->offset < 0)
            diag.report(DiagnosticCode::Err_FlirMalformed, Span{}, mname, "frame slot has no offset");
    };
    for (auto* param : method->parameters) check_slot(param);
    for (auto* local : method->locals) check_slot(local);

    if (!method->body) return;
    VerifyVisitor visitor(method, diag);
    method->body->accept(&visitor);
}

}
