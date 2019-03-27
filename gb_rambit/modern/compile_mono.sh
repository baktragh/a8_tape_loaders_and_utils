#!/bin/sh
xasm  -d LDRTYPE=0 -d COLORSYSTEM=0 -l listing/rambit_mono_modern_bot_c0.lst -o boot/rambit_mono_modern_c0.bot rambit_mono_modern.asm
xasm  -d LDRTYPE=0 -d COLORSYSTEM=1 -l listing/rambit_mono_modern_bot_c1.lst -o boot/rambit_mono_modern_c1.bot rambit_mono_modern.asm
xasm  -d LDRTYPE=1 -d COLORSYSTEM=0 -l listing/rambit_mono_modern_xex_c0.lst -o xex/rambit_mono_modern_c0.xex rambit_mono_modern.asm
xasm  -d LDRTYPE=1 -d COLORSYSTEM=1 -l listing/rambit_mono_modern_xex_c1.lst -o xex/rambit_mono_modern_c1.xex rambit_mono_modern.asm

