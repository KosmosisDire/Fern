#pragma once

#include <flir/flir.hpp>

namespace Fern
{

class Diagnostics;

// Checks lowered FLIR against the address model invariants and reports violations. A clean method is
// silent. A violation means a lowering bug, so it reports through the shared diagnostics sink.
class FlirVerifier
{
public:
    static void verify(FlirMethod* method, Diagnostics& diag);
};

}
