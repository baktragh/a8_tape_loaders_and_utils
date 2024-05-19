@ECHO OFF
atasm -ouniturbo.xex uniturbo.asm
atasm -r -ouniturbo.bin uniturbo.asm
PAUSE