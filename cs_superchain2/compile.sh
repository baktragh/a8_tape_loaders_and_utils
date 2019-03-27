#!/bin/sh
atasm -s -r -osuperchain2.bin superchain2.asm
atasm -s -osuperchain2.xex wrapper.asm

rm superchain2.bin
