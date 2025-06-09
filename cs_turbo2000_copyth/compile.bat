@ECHO OFF
set PATH=%PATH%;C:\utils\a8\mads
mads -d:FNAME_LEN=11 -l:build\body11.lst -o:build\body11.bin body.asm
mads -d:FNAME_LEN=11 -l:build\copyth11.lst -o:copyth11.xex wrapper.asm
REM mads -d:FNAME_LEN=8 -l:build\body8.lst -o:build\body8.bin body.asm
REM mads -d:FNAME_LEN=8 -l:build\copyth8.lst -o:copyth8.xex wrapper.asm
PAUSE
