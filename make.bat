@echo off

if exist PROG_CODE_00.bin del PROG_CODE_00.bin
if exist PROG_CODE_01.bin del PROG_CODE_01.bin
if exist PROG_CODE_02.bin del PROG_CODE_02.bin
if exist SGSOUND0.bin del SGSOUND0.bin

asar PROG_CODE_00.asm PROG_CODE_00.bin
asar PROG_CODE_01.asm PROG_CODE_01.bin
asar PROG_CODE_02.asm PROG_CODE_02.bin
asar SGSOUND0.asm SGSOUND0.bin

@fc /B SGSOUND0.bak SGSOUND0.bin | head -n5

del PROG_CODE_00.bin PROG_CODE_01.bin PROG_CODE_02.bin