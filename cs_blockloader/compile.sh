#!/bin/sh
atasm -s -r -oblockloader.bin blockloader.asm 
atasm -oblockloader.xex wrapper.asm

rm blockloader.bin