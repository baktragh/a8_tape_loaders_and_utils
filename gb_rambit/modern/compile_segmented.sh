#!/bin/sh
atasm -llisting/rambit_segmented_modern_xex_c0.lst -dLDRTYPE=1 -dCOLORSYSTEM=0 -v -oxex/rambit_segmented_modern_c0.xex rambit_segmented_modern.asm
atasm -llisting/rambit_segmented_modern_xex_c1.lst -dLDRTYPE=1 -dCOLORSYSTEM=1 -v -oxex/rambit_segmented_modern_c1.xex rambit_segmented_modern.asm
atasm -llisting/rambit_segmented_modern_bot_c0.lst -dLDRTYPE=0 -dCOLORSYSTEM=0 -v -r -oboot/rambit_segmented_modern_c0.bot rambit_segmented_modern.asm
atasm -llisting/rambit_segmented_modern_bot_c1.lst -dLDRTYPE=0 -dCOLORSYSTEM=1 -v -r -oboot/rambit_segmented_modern_c1.bot rambit_segmented_modern.asm

