#pragma once

namespace Fern
{

// Byte layout parameters for the target platform. The symbol table holds the one instance so the
// layout pass, the word sized types, and the VM all agree on the pointer width.
struct TargetInfo
{
    int pointerSize = 8;
    int pointerAlign = 8;
    // Heap blocks behind Array and String hold an i32 length at offset 0 then element data at this offset
    int blockHeaderSize = 8;
};

}
