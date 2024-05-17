@ECHO OFF
set PATH=%PATH%;C:\utils\a8\mads
mads -l:build\atr1_s.lst -o:build\atr1_s.bin resources\atr_header_s.asm
mads -l:build\atr2_s.lst -o:build\atr2_s.bin resources\atr_marking_pristine.asm
mads -l:build\backtd.lst -o:build\backtd.bot backtd.asm
mads -l:build\atrc_s.lst -o:build\backtd_0_s.atr resources\atr_compose_s.asm
copy /b build\backtd_0_s.atr+resources\atr_filler_s.dat backtd_s.atr
PAUSE
