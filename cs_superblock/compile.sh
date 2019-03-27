#!/bin/sh
atasm -s -r -osuperblock.bin superblock.asm
atasm -s -osuperblock.xex wrapper.asm

rm superblock.bin
