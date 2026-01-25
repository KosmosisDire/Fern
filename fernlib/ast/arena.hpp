#pragma once

#include <cstdint>
#include <cstddef>
#include <vector>
#include <memory>

namespace Fern
{

class AstArena
{
public:
    static constexpr size_t DefaultBlockSize = 4096;

    explicit AstArena(size_t blockSize = DefaultBlockSize)
        : blockSize(blockSize)
    {
        allocate_block();
    }

    ~AstArena() = default;

    // Non-copyable, non-movable
    AstArena(const AstArena&) = delete;
    AstArena& operator=(const AstArena&) = delete;

    template <typename T, typename... Args>
    T* alloc(Args&&... args)
    {
        void* mem = allocate(sizeof(T), alignof(T));
        return new (mem) T(std::forward<Args>(args)...);
    }

    template <typename T>
    T* alloc_array(size_t count)
    {
        if (count == 0) return nullptr;
        void* mem = allocate(sizeof(T) * count, alignof(T));
        return static_cast<T*>(mem);
    }

    void reset()
    {
        blocks.clear();
        current = nullptr;
        end = nullptr;
        allocate_block();
    }

private:
    void* allocate(size_t size, size_t align)
    {
        uintptr_t currentAddr = reinterpret_cast<uintptr_t>(current);
        uintptr_t aligned = (currentAddr + align - 1) & ~(align - 1);
        size_t padding = aligned - currentAddr;

        if (current + padding + size > end)
        {
            allocate_block(size + align);
            return allocate(size, align);
        }

        current += padding;
        void* result = current;
        current += size;
        return result;
    }

    void allocate_block(size_t minSize = 0)
    {
        size_t size = std::max(blockSize, minSize);
        blocks.push_back(std::make_unique<uint8_t[]>(size));
        current = blocks.back().get();
        end = current + size;
    }

    size_t blockSize;
    std::vector<std::unique_ptr<uint8_t[]>> blocks;
    uint8_t* current = nullptr;
    uint8_t* end = nullptr;
};

}
