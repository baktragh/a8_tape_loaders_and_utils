@ECHO OFF
echo Assembler...
#Assemble boot header and main part
atasm -r -v -ominiblock_bootable_tricked.bot miniblock_bootable_tricked.asm
PAUSE

