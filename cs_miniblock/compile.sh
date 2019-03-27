#!/bin/sh
atasm -s -ominiblock.xex miniblock.asm
atasm -s -r -ominiblock_bootable.bot miniblock_bootable.asm
