@ECHO OFF
C:\utils\a8\mads\mads -d:LDRTYPE=0 -o:lbe.bot -l:lbeb.lst lbe.asm
C:\utils\a8\mads\mads -d:LDRTYPE=1 -o:lbe.xex -l:lbex.lst lbe.asm
PAUSE