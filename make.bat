@echo off

set file=PROG_CODE_01

if exist "%file%".bin (
    del "%file%".bin
)

:asar "%file%".asm "%file%".bin
asar xxxxx.asm xxxxx.bin
:asar KAN.asm KAN.bin

:@fc /B "%file%".bak "%file%".bin | head -n5