#!/bin/sh
atasm -dLDRTYPE=1 -v -otscbl.xex tscbl.asm
atasm -dLDRTYPE=0 -v -r -otscbl.bot tscbl.asm
