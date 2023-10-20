@ECHO OFF
set PATH=%PATH%;C:\utils\a8\mads
mads -l:binary.lst -d:LDRTYPE=1 -d:TITLE=0 -o:speedy2700_core.bin speedy2700_core.asm
mads -l:binary_t.lst -d:LDRTYPE=1 -d:TITLE=1 -o:speedy2700_core_t.bin speedy2700_core.asm
mads -l:boot.lst -d:LDRTYPE=2   -d:TITLE=0 -o:speedy2700.bot speedy2700_core.asm
mads -l:boot_t.lst -d:LDRTYPE=2   -d:TITLE=1 -o:speedy2700_t.bot speedy2700_core.asm
mads -l:wrapper.lst -d:TITLE=0 -o:speedy2700.xex wrapper.asm
mads -l:wrapper_t.lst -d:TITLE=1 -o:speedy2700_t.xex wrapper.asm
PAUSE

