#pragma once

#include <cstdint>

namespace Fern
{

// IEEE 754 binary16 helpers shared by the constant folder and the VM. Bits live in a uint16.
uint16_t f16_from_double(double value);
float f16_to_float(uint16_t bits);

}
