#!/bin/sh
atasm -dLDRTYPE=1 -s -v -otscbl.xex tscbl.asm
atasm -dLDRTYPE=0 -s -v -r -otscbl.bot tscbl.asm
