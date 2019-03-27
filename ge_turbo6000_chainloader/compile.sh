#!/bin/sh
atasm -s -r -ot6k_chainloader.bin t6k_chainloader.asm 
atasm -ot6k_chainloader.xex t6k_chainloader_wrapper.asm

rm t6k_chainloader.bin
