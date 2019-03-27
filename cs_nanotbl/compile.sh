#!/bin/sh
#Raw binaries 
atasm -s -r -dUNDER_ROM=0 -onanotbl.bin    nanotbl.asm
atasm -s -r -dUNDER_ROM=1 -onanotbl_ur.bin nanotbl.asm
atasm -s -r -dUNDER_ROM=2 -onanotbl_u2.bin nanotbl.asm

#Wrapped binaries
atasm -s -dUNDER_ROM=0 -onanotbl.xex    wrapper.asm
atasm -s -dUNDER_ROM=1 -onanotbl_ur.xex wrapper.asm
atasm -s -dUNDER_ROM=2 -onanotbl_u2.xex wrapper.asm

#Delete raw binary files
rm nanotbl.bin
rm nanotbl_ur.bin
rm nanotbl_u2.bin

