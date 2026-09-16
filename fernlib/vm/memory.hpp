#pragma once

#include <cstdint>
#include <map>
#include <string>
#include <vector>

namespace Fern
{

struct TypeSymbol;

// A non resumable runtime error. Thrown at the fault site, caught once at the run boundary, where it
// becomes a diagnostic carrying the current node span.
struct VmError
{
    std::string message;
};

// A heap allocation, keyed by its start address in the block table. The type lets a future collector
// walk the block. A native block came from NativeMemory.Alloc, has no type, and is the only kind
// NativeMemory.Free accepts. A freed block stays in the table so a stale pointer into it still traps.
struct HeapBlock
{
    uint64_t size = 0;
    TypeSymbol* type = nullptr;
    bool native = false;
    bool freed = false;
};

// Interpreter memory on real host addresses, so a pointer can cross into C unchanged. The stack is one
// fixed buffer that never moves. Every heap block is its own host allocation recorded in a table, so a
// block never moves either. An access to the stack or a known block is checked, so null, a popped frame,
// a freed block, and an overrun trap. Any other address is memory C owns and is not checked.
class VmMemory
{
public:
    explicit VmMemory(uint64_t stackSize);
    ~VmMemory();

    VmMemory(const VmMemory&) = delete;
    VmMemory& operator=(const VmMemory&) = delete;

    // Allocates a zero filled heap block and records it. Returns the block address.
    uint64_t alloc(uint64_t size, TypeSymbol* type);
    // Allocates a native block with undefined contents and records it. Returns the block address.
    uint64_t native_alloc(uint64_t size);
    // Releases a native block. Null is ignored, anything else that is not a live native block errors.
    void native_free(uint64_t addr);

    // Reserves a zero filled frame in the stack region, 8 byte aligned. Errors on stack overflow.
    uint64_t stack_alloc(uint64_t size);
    // Pops the stack back to a base returned by an earlier stack_alloc.
    void stack_restore(uint64_t base);
    uint64_t stack_pointer() const { return sp; }

    uint8_t read_u8(uint64_t addr);
    uint16_t read_u16(uint64_t addr);
    uint32_t read_u32(uint64_t addr);
    uint64_t read_u64(uint64_t addr);
    void write_u8(uint64_t addr, uint8_t value);
    void write_u16(uint64_t addr, uint16_t value);
    void write_u32(uint64_t addr, uint32_t value);
    void write_u64(uint64_t addr, uint64_t value);

    // Moves size bytes between two validated ranges. Overlap is allowed.
    void copy(uint64_t dest, uint64_t src, uint64_t size);
    // Copies host bytes into a validated range.
    void write_bytes(uint64_t addr, const void* src, uint64_t size);

    // A validated pointer to the bytes at addr. Stays valid as long as the block or frame it is in.
    uint8_t* host_ptr(uint64_t addr, uint64_t size);

private:
    // Checks a range that starts in the stack or a known block. Any other address is left alone.
    void validate(uint64_t addr, uint64_t size);
    std::map<uint64_t, HeapBlock>::iterator find_block(uint64_t addr);
    // Records a fresh host allocation, erroring when the host returned null
    uint64_t record_block(void* ptr, HeapBlock block);

    std::vector<uint8_t> stack;
    std::map<uint64_t, HeapBlock> blocks;

    uint64_t stackBase = 0;
    uint64_t stackEnd = 0;
    uint64_t sp = 0;
};

}
