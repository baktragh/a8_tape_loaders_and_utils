@ECHO OFF
atasm -s -r -osuperchain2.bin superchain2.asm
atasm -s -osuperchain2.xex wrapper.asm

DEL superchain2.bin
