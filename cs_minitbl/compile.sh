#!/bin/sh

#Create raw binary
atasm -v -r -s -ominitbl.bin minitbl.asm

#Wrapped binary
atasm -ominitbl.xex wrapper.asm

#Delete raw binaries
rm minitbl.bin

