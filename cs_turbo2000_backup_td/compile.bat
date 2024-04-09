@ECHO OFF
set PATH=%PATH%;C:\utils\a8\mads
mads -l:build\atr1.lst -o:build\atr1.bin atr_header.asm
mads -l:build\atr2.lst -o:build\atr2.bin atr_marking_pristine.asm
mads -l:build\backtd.lst -o:build\backtd.bot backtd.asm
mads -l:build\atrc.lst -o:build\backtd_0.atr atr_compose.asm
copy /b build\backtd_0.atr+atr_filler.dat backtd.atr
PAUSE
