#!/bin/sh
atasm -s -r -ochainloader2.bin chainloader2.asm
atasm  -ochainloader2.xex wrapper.asm

rm chainloader2.bin


