libFuzzer is a library for coverage-guided fuzz testing which is part of the LLVM project. 
Fernfuzz implements a harness for libFuzzer to fuz test the Fern compiler.
The fuzzer code itself is very messy because it doesn't really matter how it works as long as it works since it is a completely independent debugging detail.
However, I had a lot of issues getting it to work reliably without crashing.
It would crash due to out of memory errors after slowly leaking memory on every single execution, but I could never reproduce a memory leak just with fernlib itself.
It would also crash due to some interaction with how it had to be compiled. Because it would crash and the crash would not be reproducable with the normal compiler, only with the fuzzer.
I would like to fix this eventually, but for now I can at least run the fuzzer until it runs out of memory. Or the fuzzer might crash with a stack overflow, but if I pass the last input through the fuzzer again it wouldnot trigger the stack overflow again.

=================================================================
==70676==ERROR: AddressSanitizer: stack-overflow on address 0x7ffd723c6377 (pc 0x7ffd723c6377 bp 0x00b6f5294340 sp 0x00b6f52942c8 T0)
    <empty stack>

SUMMARY: AddressSanitizer: stack-overflow
==70676==ABORTING
PS C:\Main\Projects\Coding\Fern> .\build\fuzz\bin\fernfuzz.exe .\last_input.bin                   
INFO: Running with entropic power schedule (0xFF, 100).
INFO: Seed: 1743175594
INFO: Loaded 1 modules   (16530 inline 8-bit counters): 16530 [00007FF600497888, 00007FF60049B91A), 
INFO: Loaded 1 PC tables (16530 PCs): 16530 [00007FF600404F98,00007FF6004458B8), 
C:\Main\Projects\Coding\Fern\build\fuzz\bin\fernfuzz.exe: Running 1 inputs 1 time(s) each.
Running: .\last_input.bin
Executed .\last_input.bin in 0 ms
***
*** NOTE: fuzzing was not performed, you have only
***       executed the target code on a fixed set of inputs.
***

Maybe if I run the fuzzer on linux I can track the memory leak more easily than on windows.