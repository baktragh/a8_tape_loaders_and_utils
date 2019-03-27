#!/bin/sh
atasm -oturbo_rom_loader.bin -v -r turbo_rom_loader.asm
atasm -oturbo_rom_diagnostic_loader.bin -v -r turbo_rom_diagnostic_loader.asm
atasm -oturbo_rom_loader.xex wrapper.asm
atasm -oturbo_rom_diagnostic_loader.xex turbo_rom_diagnostic_loader_wrapper.asm



