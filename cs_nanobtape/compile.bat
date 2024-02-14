@ECHO OFF
REM Raw binaries 
mads -d:UNDER_ROM=0 -o:nanobtape.bin    nanobtape.asm
mads -d:UNDER_ROM=1 -o:nanobtape_ur.bin nanobtape.asm
mads -d:UNDER_ROM=2 -o:nanobtape_u2.bin nanobtape.asm

REM Wrapped binaries
mads -d:UNDER_ROM=0 -o:nanobtape.xex    wrapper.asm
mads -d:UNDER_ROM=1 -o:nanobtape_ur.xex wrapper.asm
mads -d:UNDER_ROM=2 -o:nanobtape_u2.xex wrapper.asm

REM Delete raw binary files
REM del nanobtape.bin
REM del nanobtape_ur.bin
REM del nanobtape_u2.bin

