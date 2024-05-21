@ECHO OFF
mads -l:build\atr1.lst -o:build\atr1.bin resources\atr_header.asm
mads -l:build\atr2.lst -o:build\atr2.bin resources\atr_marking_pristine.asm
mads -l:build\backtd.lst -o:build\backtd.bot backtd.asm
mads -l:build\atrc.lst -o:build\backtd_0.atr resources\atr_compose.asm
copy /b build\backtd_0.atr+resources\atr_filler.dat backup_td_utility_disk.atr
PAUSE
