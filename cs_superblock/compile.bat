@ECHO OFF
atasm -s -r -osuperblock.bin superblock.asm
atasm -s -osuperblock.xex wrapper.asm

del superblock.bin
