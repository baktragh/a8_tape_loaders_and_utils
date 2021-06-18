#!/bin/sh
atasm -s -v -r -ol3_loader.bin l3_loader.asm
atasm -ol3_loader.xex wrapper.asm


