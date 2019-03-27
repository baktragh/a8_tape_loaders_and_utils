#!/bin/sh
#Raw binaries 
atasm -s -r -dUNDER_ROM=0 -onanobtape.bin    nanobtape.asm
atasm -s -r -dUNDER_ROM=1 -onanobtape_ur.bin nanobtape.asm
atasm -s -r -dUNDER_ROM=2 -onanobtape_u2.bin nanobtape.asm

#Wrapped binaries
atasm -dUNDER_ROM=0 -onanobtape.xex    wrapper.asm
atasm -dUNDER_ROM=1 -onanobtape_ur.xex wrapper.asm
atasm -dUNDER_ROM=2 -onanobtape_u2.xex wrapper.asm

#Delete raw binary files
rm nanobtape.bin
rm nanobtape_ur.bin
rm nanobtape_u2.bin

