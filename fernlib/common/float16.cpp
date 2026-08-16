#include <common/float16.hpp>

#include <cstring>

namespace Fern
{

// Rounds to nearest even. A float widens to double exactly, so this covers f32 and f64 sources
uint16_t f16_from_double(double value)
{
    uint64_t bits;
    std::memcpy(&bits, &value, 8);

    uint16_t sign = static_cast<uint16_t>((bits >> 48) & 0x8000);
    int exp = static_cast<int>((bits >> 52) & 0x7FF);
    uint64_t mant = bits & 0xFFFFFFFFFFFFFull;

    // Infinities stay infinite and every nan becomes the quiet nan
    if (exp == 0x7FF)
        return static_cast<uint16_t>(sign | (mant == 0 ? 0x7C00 : 0x7E00));

    int halfExp = exp - 1023 + 15;
    if (halfExp >= 0x1F)
        return static_cast<uint16_t>(sign | 0x7C00);

    // Subnormal results shift the implicit bit below the exponent field before rounding
    if (halfExp <= 0)
    {
        if (halfExp < -10) return sign;
        mant |= 1ull << 52;
        int shift = 43 - halfExp;
        uint16_t result = static_cast<uint16_t>(sign | (mant >> shift));
        uint64_t rem = mant & ((1ull << shift) - 1);
        uint64_t halfway = 1ull << (shift - 1);
        if (rem > halfway || (rem == halfway && (result & 1)))
            result++;
        return result;
    }

    // The round up carry can walk into the exponent and up to infinity, which is correct
    uint16_t result = static_cast<uint16_t>(sign | (halfExp << 10) | static_cast<uint16_t>(mant >> 42));
    uint64_t rem = mant & ((1ull << 42) - 1);
    uint64_t halfway = 1ull << 41;
    if (rem > halfway || (rem == halfway && (result & 1)))
        result++;
    return result;
}

// Exact, every binary16 value is representable as a float
float f16_to_float(uint16_t bits)
{
    uint32_t sign = static_cast<uint32_t>(bits & 0x8000) << 16;
    uint32_t exp = (bits >> 10) & 0x1F;
    uint32_t mant = bits & 0x3FF;

    uint32_t out;
    if (exp == 0x1F)
    {
        out = sign | 0x7F800000u | (mant << 13);
    }
    else if (exp == 0)
    {
        if (mant == 0)
        {
            out = sign;
        }
        else
        {
            // Renormalize the subnormal into the wider exponent range
            int shift = 0;
            while ((mant & 0x400) == 0)
            {
                mant <<= 1;
                shift++;
            }
            mant &= 0x3FF;
            out = sign | (static_cast<uint32_t>(113 - shift) << 23) | (mant << 13);
        }
    }
    else
    {
        out = sign | ((exp + 112) << 23) | (mant << 13);
    }

    float value;
    std::memcpy(&value, &out, 4);
    return value;
}

double f16_round(double value)
{
    return f16_to_float(f16_from_double(value));
}

}
