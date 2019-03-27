#!/bin/sh
atasm -oturbo_rom_binload.bin -s -r -v turbo_rom_binload.asm
atasm -oturbo_rom_binload.xex -s -v wrapper.asm
atasm -oturbo_rom_binload_simple.xex -s -v wrapper_simple.asm

