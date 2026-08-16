#include <vm/memory.hpp>

#include <algorithm>
#include <cstring>
#include <format>

namespace Fern
{

// Everything below the stack is an unmapped null page so a zero handle always traps.
static constexpr uint64_t nullPageSize = 4096;

static uint64_t align_up(uint64_t value, uint64_t align)
{
    return (value + align - 1) & ~(align - 1);
}

VmMemory::VmMemory(uint64_t stackSize)
{
    stackBase = nullPageSize;
    heapBase = nullPageSize + stackSize;
    sp = stackBase;
    hp = heapBase;
    bytes.resize(heapBase, 0);
}

#pragma region Allocation

uint64_t VmMemory::alloc(uint64_t size, TypeSymbol* type)
{
    hp = align_up(hp, 8);
    uint64_t addr = hp;
    hp += size;
    if (bytes.size() < hp)
        bytes.resize(hp, 0);
    blocks.push_back(HeapBlock{addr, size, type});
    return addr;
}

uint64_t VmMemory::stack_alloc(uint64_t size)
{
    uint64_t base = align_up(sp, 8);
    uint64_t next = base + size;
    if (next > heapBase)
        throw VmError{"stack overflow"};
    std::fill(bytes.begin() + base, bytes.begin() + next, uint8_t{0});
    sp = next;
    return base;
}

void VmMemory::stack_restore(uint64_t base)
{
    sp = base;
}

#pragma region Validation

HeapBlock* VmMemory::find_block(uint64_t addr)
{
    // The last block whose start is at or below addr is the only one that can contain it.
    auto it = std::upper_bound(blocks.begin(), blocks.end(), addr,
        [](uint64_t a, const HeapBlock& b) { return a < b.start; });
    if (it == blocks.begin()) return nullptr;
    return &*(it - 1);
}

void VmMemory::validate(uint64_t addr, uint64_t size)
{
    if (size == 0) return;

    uint64_t end = addr + size;
    if (end < addr)
        throw VmError{std::format("invalid memory access at address {} (size {})", addr, size)};

    if (addr >= stackBase && end <= sp) return;

    HeapBlock* block = find_block(addr);
    if (block && addr >= block->start && end <= block->start + block->size) return;

    throw VmError{std::format("invalid memory access at address {} (size {})", addr, size)};
}

#pragma region Access

uint8_t VmMemory::read_u8(uint64_t addr)
{
    validate(addr, 1);
    return bytes[addr];
}

uint16_t VmMemory::read_u16(uint64_t addr)
{
    validate(addr, 2);
    uint16_t value;
    std::memcpy(&value, bytes.data() + addr, 2);
    return value;
}

uint32_t VmMemory::read_u32(uint64_t addr)
{
    validate(addr, 4);
    uint32_t value;
    std::memcpy(&value, bytes.data() + addr, 4);
    return value;
}

uint64_t VmMemory::read_u64(uint64_t addr)
{
    validate(addr, 8);
    uint64_t value;
    std::memcpy(&value, bytes.data() + addr, 8);
    return value;
}

void VmMemory::write_u8(uint64_t addr, uint8_t value)
{
    validate(addr, 1);
    bytes[addr] = value;
}

void VmMemory::write_u16(uint64_t addr, uint16_t value)
{
    validate(addr, 2);
    std::memcpy(bytes.data() + addr, &value, 2);
}

void VmMemory::write_u32(uint64_t addr, uint32_t value)
{
    validate(addr, 4);
    std::memcpy(bytes.data() + addr, &value, 4);
}

void VmMemory::write_u64(uint64_t addr, uint64_t value)
{
    validate(addr, 8);
    std::memcpy(bytes.data() + addr, &value, 8);
}

void VmMemory::copy(uint64_t dest, uint64_t src, uint64_t size)
{
    validate(dest, size);
    validate(src, size);
    std::memmove(bytes.data() + dest, bytes.data() + src, size);
}

void VmMemory::write_bytes(uint64_t addr, const void* src, uint64_t size)
{
    validate(addr, size);
    std::memcpy(bytes.data() + addr, src, size);
}

uint8_t* VmMemory::host_ptr(uint64_t addr, uint64_t size)
{
    validate(addr, size);
    return bytes.data() + addr;
}

}
