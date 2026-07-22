#include <flir/verify.hpp>

#include <common/diagnostic.hpp>
#include <flir/builder.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

#pragma region Address Predicate

// True for nodes that produce an address, seen through a sequence that yields one.
static bool is_address(FlirExpr* node)
{
    if (!node) return false;
    if (node->is<FlirLocalAddr>() || node->is<FlirFieldAddr>() || node->is<FlirElemAddr>()) return true;
    if (auto* seq = node->as<FlirSequence>()) return is_address(seq->value);
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
            if (is_memory_class(n->type)) fail(n, "load of an aggregate type");
        }
        else if (auto* n = node->as<FlirStore>())
        {
            if (!is_address(n->address)) fail(n, "store address is not an address");
            else if (is_memory_class(n->address->type)) fail(n, "store to an aggregate address");
        }
        else if (auto* n = node->as<FlirCopy>())
        {
            if (!is_address(n->dest)) fail(n, "copy dest is not an address");
            if (!is_address(n->src)) fail(n, "copy src is not an address");
            if (!is_memory_class(n->type)) fail(n, "copy of a non aggregate type");
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
        }
        else if (auto* n = node->as<FlirElemAddr>())
        {
            if (!n->base) fail(n, "element address has no base");
            if (!n->index) fail(n, "element address has no index");
        }
        else if (auto* n = node->as<FlirCall>())
        {
            if (n->method && n->method->is_intrinsic()) fail(n, "call to an intrinsic method");
        }
        else if (auto* n = node->as<FlirIntrinsic>())
        {
            if (!n->method) fail(n, "intrinsic has no method");
            else if (!n->method->is_intrinsic()) fail(n, "intrinsic node has a non intrinsic method");
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
