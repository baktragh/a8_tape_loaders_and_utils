@ECHO OFF
SET MAD_A=C:\utils\a8\mads\mads
%MAD_A% -o:jumbo_head.xex -l:jumbo_head.lst jumbo_head.asm
%MAD_A% -o:jumbo_tail.xex -l:jumbo_tail.lst jumbo_tail.asm
PAUSE
