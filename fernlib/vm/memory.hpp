#pragma once

#include <cstdint>
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

// A live heap allocation. Blocks are kept in address order so a binary search maps any address to the
// block that owns it. The type lets a future collector walk the block.
struct HeapBlock
{
    uint64_t start = 0;
    uint64_t size = 0;
    TypeSymbol* type = nullptr;
};

// Flat virtual memory for the interpreter. One byte vector split into a null page, a stack region, and a
// heap that grows by resize. Addresses are byte indexes, so heap growth never invalidates them. Every
// access is validated against the live stack range or an owning heap block and errors otherwise.
class VmMemory
{
public:
    explicit VmMemory(uint64_t stackSize);

    // Bump allocates a zero filled heap block and records it. Returns the block address.
    uint64_t alloc(uint64_t size, TypeSymbol* type);

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

    // A validated raw pointer into flat memory. Invalidated by any alloc, so use it before allocating.
    uint8_t* host_ptr(uint64_t addr, uint64_t size);

private:
    // Errors unless the whole range lies in the live stack or inside a single heap block.
    void validate(uint64_t addr, uint64_t size);
    HeapBlock* find_block(uint64_t addr);

    std::vector<uint8_t> bytes;
    std::vector<HeapBlock> blocks;

    uint64_t stackBase = 0;
    uint64_t heapBase = 0;
    uint64_t sp = 0;
    uint64_t hp = 0;
};

}
