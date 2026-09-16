#include <vm/memory.hpp>

#include <iterator>
#include <cstdlib>
#include <cstring>
#include <format>

namespace Fern
{

static uint64_t align_up(uint64_t value, uint64_t align)
{
    return (value + align - 1) & ~(align - 1);
}

static uint64_t to_addr(const void* ptr)
{
    return static_cast<uint64_t>(reinterpret_cast<uintptr_t>(ptr));
}

static uint8_t* to_ptr(uint64_t addr)
{
    return reinterpret_cast<uint8_t*>(static_cast<uintptr_t>(addr));
}

VmMemory::VmMemory(uint64_t stackSize)
    : stack(stackSize, 0)
{
    stackBase = to_addr(stack.data());
    stackEnd = stackBase + stackSize;
    sp = stackBase;
}

VmMemory::~VmMemory()
{
    for (auto& [start, block] : blocks)
    {
        if (!block.freed) std::free(to_ptr(start));
    }
}

#pragma region Allocation

uint64_t VmMemory::alloc(uint64_t size, TypeSymbol* type)
{
    // A zero sized block still gets its own address so it is never null and never shared
    return record_block(std::calloc(1, size == 0 ? 1 : size), HeapBlock{size, type, false});
}

uint64_t VmMemory::native_alloc(uint64_t size)
{
    return record_block(std::malloc(size == 0 ? 1 : size), HeapBlock{size, nullptr, true});
}

void VmMemory::native_free(uint64_t addr)
{
    if (addr == 0) return;

    auto it = blocks.find(addr);
    if (it == blocks.end())
        throw VmError{std::format("free of {:#x}, which is not the start of a native block", addr)};
    if (!it->second.native)
        throw VmError{std::format("free of {:#x}, which is a managed block", addr)};
    if (it->second.freed)
        throw VmError{std::format("free of {:#x}, which was already freed", addr)};

    std::free(to_ptr(addr));
    it->second.freed = true;
}

uint64_t VmMemory::record_block(void* ptr, HeapBlock block)
{
    if (!ptr)
        throw VmError{std::format("out of memory allocating {} bytes", block.size)};

    // The host may hand back the address of a freed block, which this new block then replaces
    uint64_t addr = to_addr(ptr);
    blocks.insert_or_assign(addr, block);
    return addr;
}

uint64_t VmMemory::stack_alloc(uint64_t size)
{
    uint64_t base = align_up(sp, 8);
    uint64_t next = base + size;
    if (next > stackEnd)
        throw VmError{"stack overflow"};
    std::memset(to_ptr(base), 0, size);
    sp = next;
    return base;
}

void VmMemory::stack_restore(uint64_t base)
{
    sp = base;
}

#pragma region Validation

std::map<uint64_t, HeapBlock>::iterator VmMemory::find_block(uint64_t addr)
{
    // The last block whose start is at or below addr is the only one that can contain it.
    auto it = blocks.upper_bound(addr);
    if (it == blocks.begin()) return blocks.end();
    return std::prev(it);
}

void VmMemory::validate(uint64_t addr, uint64_t size)
{
    if (size == 0) return;
    if (addr == 0)
        throw VmError{std::format("null memory access (size {})", size)};

    uint64_t end = addr + size;
    if (end < addr)
        throw VmError{std::format("invalid memory access at address {:#x} (size {})", addr, size)};

    if (addr >= stackBase && addr < stackEnd)
    {
        if (end <= sp) return;
        throw VmError{std::format("invalid memory access at address {:#x} (size {})", addr, size)};
    }

    auto it = find_block(addr);
    if (it == blocks.end() || addr >= it->first + it->second.size) return;
    if (it->second.freed)
        throw VmError{std::format("access to freed memory at address {:#x} (size {})", addr, size)};
    if (end <= it->first + it->second.size) return;

    throw VmError{std::format("invalid memory access at address {:#x} (size {})", addr, size)};
}

#pragma region Access

uint8_t VmMemory::read_u8(uint64_t addr)
{
    validate(addr, 1);
    return *to_ptr(addr);
}

uint16_t VmMemory::read_u16(uint64_t addr)
{
    validate(addr, 2);
    uint16_t value;
    std::memcpy(&value, to_ptr(addr), 2);
    return value;
}

uint32_t VmMemory::read_u32(uint64_t addr)
{
    validate(addr, 4);
    uint32_t value;
    std::memcpy(&value, to_ptr(addr), 4);
    return value;
}

uint64_t VmMemory::read_u64(uint64_t addr)
{
    validate(addr, 8);
    uint64_t value;
    std::memcpy(&value, to_ptr(addr), 8);
    return value;
}

void VmMemory::write_u8(uint64_t addr, uint8_t value)
{
    validate(addr, 1);
    *to_ptr(addr) = value;
}

void VmMemory::write_u16(uint64_t addr, uint16_t value)
{
    validate(addr, 2);
    std::memcpy(to_ptr(addr), &value, 2);
}

void VmMemory::write_u32(uint64_t addr, uint32_t value)
{
    validate(addr, 4);
    std::memcpy(to_ptr(addr), &value, 4);
}

void VmMemory::write_u64(uint64_t addr, uint64_t value)
{
    validate(addr, 8);
    std::memcpy(to_ptr(addr), &value, 8);
}

void VmMemory::copy(uint64_t dest, uint64_t src, uint64_t size)
{
    validate(dest, size);
    validate(src, size);
    std::memmove(to_ptr(dest), to_ptr(src), size);
}

void VmMemory::write_bytes(uint64_t addr, const void* src, uint64_t size)
{
    validate(addr, size);
    std::memcpy(to_ptr(addr), src, size);
}

uint8_t* VmMemory::host_ptr(uint64_t addr, uint64_t size)
{
    validate(addr, size);
    return to_ptr(addr);
}

}
