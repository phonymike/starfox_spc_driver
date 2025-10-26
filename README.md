# Star Fox Sound Driver Disassembly
This repository contains a disassembly of the N-SPC/Kankichi-Kun sound driver variant used in Star Fox/Starwing (SNES).  
It builds the following SBN:  
- SGSOUND0.BIN ``sha1: 84a3411c5c6406b53716d40d64b59f1e55f0f27f``

## Assembly
Obtain Asar v1.91 and place the executable in the root directory.  
For matching builds, obtain the original SGSOUND0.BIN, rename it to ``SGSOUND0.BAK``, and place in the root directory. SGSOUND0.BIN can be found in every release of Star Fox/Starwing from 0xC0000-0xC28C1.  
Run ``make.bat`` to assemble the sound driver.  
If you wish to modify the sound driver, remove the last line of ``make.bat`` to skip checking for a 1:1 match.  

## Project Structure

- `SGSOUND0.asm` — Main driver code, SFX pointer table, SFX chain table, SFX patterns.
- `defines.asm` — Register/variable definitions and constants used by the driver.
- `KAN.asm` — Music constants used by the driver.
- `patches.asm` — Sound patch table
