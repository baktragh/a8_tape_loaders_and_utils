@ECHO OFF
SET PATH=%PATH%;C:\utils\a8\atasm

atasm -oturbo_rom_loader.bin -v -r turbo_rom_loader.asm
atasm -oturbo_rom_loader.xex wrapper.asm

PAUSE



