#!/bin/sh
echo Assembler...
#Assemble boot header and main part
atasm -dLDRTYPE=0 -r -s -obuild/omicron_stage0.bot omicron_stage0.asm
atasm -dLDRTYPE=1 -s -obuild/omicron_stage0.xex omicron_stage0.asm
atasm -dLDRTYPE=2 -r -s -oomicron_stage0.cbin omicron_stage0.asm

#Assemble cartridge
atasm -dCARTTYPE=0 -r -s -obuild/omicron_cart_8kphoenix.bin omicron_cart.asm
atasm -dCARTTYPE=1 -r -s -obuild/omicron_cart_2kbutton.bin omicron_cart.asm

#Remove temporary
rm omicron_stage0.cbin
