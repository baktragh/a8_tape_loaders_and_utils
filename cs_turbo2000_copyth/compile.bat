@ECHO OFF
set PATH=%PATH%;C:\utils\a8\mads
mads -d:FNAME_LEN=10 -l:build\body10.lst -o:build\body10.bin body.asm
mads -d:FNAME_LEN=10 -l:build\copyth10.lst -o:copyth10.xex wrapper.asm
mads -d:FNAME_LEN=8 -l:build\body8.lst -o:build\body8.bin body.asm
mads -d:FNAME_LEN=8 -l:build\copyth8.lst -o:copyth8.xex wrapper.asm
PAUSE
