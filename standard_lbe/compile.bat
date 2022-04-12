@ECHO OFF
SET MAD_A=C:\utils\a8\mads\mads
REM Boot files
%MAD_A% -d:BUILD=0 -d:LDR_CFG=0 -o:lbe_plain.bot -l:lbe_plain_b.lst lbe.asm
%MAD_A% -d:BUILD=0 -d:LDR_CFG=1 -o:lbe_pmg.bot   -l:lbe_pmg_b.lst   lbe.asm
REM Binary load files
%MAD_A% -d:BUILD=1 -d:LDR_CFG=0 -o:lbe_plain.xex -l:lbe_plain_x.lst lbe.asm
%MAD_A% -d:BUILD=1 -d:LDR_CFG=1 -o:lbe_pmg.xex   -l:lbe_pmg_x.lst   lbe.asm

PAUSE