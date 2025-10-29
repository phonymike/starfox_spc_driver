# Star Fox Sound Driver Disassembly
This repository contains a disassembly of the [N-SPC/Kankichi-kun](https://sneslab.net/wiki/N-SPC_Engine) sound driver variant used in Star Fox/Starwing (SNES).  
It builds the following SBN:  
- SGSOUND0.BIN ``sha1: 84a3411c5c6406b53716d40d64b59f1e55f0f27f``  
  
Comments were taken from the SNES SDK's ``KAN.ASM``, supplemented by comments from [loveemu's disassembly of YI's sound driver](https://github.com/KungFuFurby/vgm-disasm/blob/master/snes/NSPC/Nintendo/Koji%20Kondo/Super%20Mario%20World%202%20-%20Yoshi's%20Island.s) and [brunovalads' YI disassembly](https://github.com/brunovalads/yoshisisland-disassembly/blob/278b3a7a067a15bfcac0049a28d7077bb05886d8/disassembly/bank20.asm#L132) where applicable, as YI's sound driver appears to be very close to the variant used in Star Fox.  
[KungFuFurby's notes](https://github.com/KungFuFurby/SNESSoundDriverDocViewer/tree/raw-notes/Raw%20Notes) were also used as a reference for comments and documentation.  

## Assembly
Obtain Asar v1.91 and place the executable in the root directory.  
For matching builds, obtain the original SGSOUND0.BIN, rename it to ``SGSOUND0.BAK``, and place in the root directory. SGSOUND0.BIN can be found in every release of Star Fox/Starwing from 0xC0000-0xC28C1.  
Run ``make.bat`` to assemble the sound driver.  
If you wish to modify the sound driver, remove the last line of ``make.bat`` to skip checking for a 1:1 match.  

## Project Structure

- `SGSOUND0.asm` — Main driver code, SFX pointer table, SFX chain table, SFX patterns.
- `defines.asm` — Register/variable definitions and constants used by the driver.
- `KAN.asm` — Music constants used by the driver.
- `patches.asm` — Instrument patch table.
