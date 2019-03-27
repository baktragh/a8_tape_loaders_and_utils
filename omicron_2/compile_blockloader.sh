#!/bin/sh
atasm -s -r -oomicron_blockloader.xbin omicron_blockloader.asm 
atasm -s -r -obuild/omicron_blockloader.bin omicron_blockloader_wrapper.asm

#Remove temporary object file
rm omicron_blockloader.xbin
