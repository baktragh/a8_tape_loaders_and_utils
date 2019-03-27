#!/bin/sh
xasm  -o chainloader2_low_profile.bin chainloader2_low_profile.asm
xasm  -o chainloader2_low_profile.xex wrapper_low_profile.asm
rm chainloader2_low_profile.bin


