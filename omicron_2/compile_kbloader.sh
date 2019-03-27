#!/bin/sh

#Raw binaries 
atasm -s -r -oomicron_kbloader.binx  omicron_kbloader.asm
#Wrapped binaries
atasm -s -r -obuild/omicron_kbloader.bin omicron_kbloader_wrapper.asm
#Wrapped binaries
atasm -s -obuild/omicron_kbloader.xex omicron_kbloader_wrapper.asm

#Delete raw binary files
rm omicron_kbloader.binx

