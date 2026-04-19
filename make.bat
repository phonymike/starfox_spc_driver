@echo off

if exist SGSOUND0.BIN del SGSOUND0.BIN

asar --symbols=nocash SGSOUND0.asm SGSOUND0.BIN

::@fc /B SGSOUND0.BAK SGSOUND0.BIN
:: | head -n5
