@ECHO OFF
set PATH=%PATH%;C:\utils\a8\atasm
atasm -r -ospeedy2700_core.bin speedy2700_core.asm
atasm -ospeedy2700.xex wrapper.asm
PAUSE

