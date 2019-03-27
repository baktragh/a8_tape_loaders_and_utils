#/bin/sh
atasm -s -r  htblp.asm
atasm -v -s -ohtblp.xex wrapper.asm
atasm -v -r -ohtblpm.bot htblpm.asm