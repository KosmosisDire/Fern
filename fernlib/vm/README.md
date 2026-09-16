# VM

A tree walking interpreter that executes lowered FLIR directly as a memory machine. It runs `Program.Main` and prints the result. The aim is debugging and correctness, not speed. It uses a memory machine so it should be able to support pointers etc in the future, and behave basically the same as LLVM.

Memory is real host memory, so a pointer can go into C unchanged. Reads and writes to the VM stack and to blocks the VM handed out are checked, so null, a popped frame, a freed block, and an overrun trap. Any other address belongs to C and is not checked.

An `@Extern` function is called through libffi. Before Main runs, every extern the program calls is looked up in the running process, so the C runtime is always in reach and a missing symbol stops the program before it starts, the way a linker would.
