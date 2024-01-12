#!/bin/sh
atasm -ochaincopy.bin -v -r chaincopy.asm
atasm -ochaincopy.xex wrapper.asm
