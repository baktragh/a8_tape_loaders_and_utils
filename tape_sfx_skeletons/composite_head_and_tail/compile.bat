@ECHO OFF
SET MAD_A=C:\utils\a8\mads\mads
%MAD_A% -o:composite_head.xex -l:composite_head.lst composite_head.asm
%MAD_A% -o:composite_tail.xex -l:composite_tail.lst composite_tail.asm
PAUSE
