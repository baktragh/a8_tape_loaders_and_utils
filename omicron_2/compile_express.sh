#!/bin/sh
mads -o:omicron_express.xbin omicron_express.asm 
mads -o:build/omicron_express.bin omicron_express_wrapper.asm

#Remove temporary object file
rm omicron_express.xbin
