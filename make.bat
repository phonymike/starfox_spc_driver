@echo off

if exist SGSOUND0.bin del SGSOUND0.bin

asar SGSOUND0.asm SGSOUND0.bin

@fc /B SGSOUND0.bak SGSOUND0.bin | head -n5
