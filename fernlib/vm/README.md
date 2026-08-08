# VM

A tree walking interpreter that executes lowered FLIR directly as a memory machine. It runs `Program.Main` and prints the result. The aim is debugging and correctness, not speed. It uses a memory machine so it should be able to support pointers etc in the future, and behave basically the same as LLVM.
