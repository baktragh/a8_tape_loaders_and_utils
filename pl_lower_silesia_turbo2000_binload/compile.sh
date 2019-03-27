#/bin/sh
atasm -s -r -olower_silesia_2000_binload.bin lower_silesia_2000_binload.asm
atasm -olower_silesia_2000_binload.xex lower_silesia_2000_binload_wrapper.asm
