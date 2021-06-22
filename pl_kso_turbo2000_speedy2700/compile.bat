@ECHO OFF
set PATH=%PATH%;C:\utils\a8\mads
mads -l:binary.lst -d:LDRTYPE=1 -o:speedy2700_core.bin speedy2700_core.asm
mads -l:boot.lst -d:LDRTYPE=2 -o:speedy2700_core.bot speedy2700_core.asm
mads -l:wrapper.lst -o:speedy2700.xex wrapper.asm
PAUSE

