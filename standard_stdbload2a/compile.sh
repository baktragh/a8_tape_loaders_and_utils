#!/bin/sh
atasm -dLDRTYPE=1 -v -ostdbload2a.xex stdbload2a.asm
atasm -dLDRTYPE=0 -v -r -ostdbload2a.bot stdbload2a.asm
