#include <flir/frame.hpp>

#include <flir/flir.hpp>
#include <semantic/symbol/symbol.hpp>

namespace Fern
{

static int align_up(int offset, int align)
{
    if (align <= 1) return offset;
    return (offset + align - 1) / align * align;
}

// Places one slot at the next aligned offset and advances. A null or unlaid type falls back to one
// byte so distinct slots keep distinct addresses.
static void place(FlirLocal* slot, int& offset, int& frameAlign)
{
    if (!slot || slot->byAddress) return;

    auto* named = slot->type ? slot->type->as<NamedTypeSymbol>() : nullptr;
    int size = named && named->sizeInBytes > 0 ? named->sizeInBytes : 1;
    int align = named && named->alignment > 0 ? named->alignment : 1;

    offset = align_up(offset, align);
    slot->offset = offset;
    offset += size;
    if (align > frameAlign) frameAlign = align;
}

void FlirFramePass::run(FlirMethod* method)
{
    if (!method) return;

    int offset = 0;
    int frameAlign = 1;

    for (auto* param : method->parameters)
        place(param, offset, frameAlign);
    for (auto* local : method->locals)
        place(local, offset, frameAlign);

    method->frameSize = align_up(offset, frameAlign);
}

}
