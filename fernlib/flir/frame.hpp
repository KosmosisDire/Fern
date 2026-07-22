#pragma once

namespace Fern
{

struct FlirMethod;

// Assigns every parameter and local a byte offset in the frame with C alignment, and sets frameSize.
// Runs after lowering and any transforms, so it sees the final slot set. LocalAddr is then base plus
// a constant offset.
class FlirFramePass
{
public:
    static void run(FlirMethod* method);
};

}
