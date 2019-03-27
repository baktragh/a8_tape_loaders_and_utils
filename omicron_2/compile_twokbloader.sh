#!/bin/sh

#Raw binaries 
atasm -s -r -oomicron_twokbloader.binx  omicron_twokbloader.asm
#Wrapped binaries
atasm -s -r -obuild/omicron_twokbloader.bin omicron_twokbloader_wrapper.asm
#Wrapped binaries
atasm -s -obuild/omicron_twokbloader.xex omicron_twokbloader_wrapper.asm

#Delete raw binary files
rm omicron_twokbloader.binx

