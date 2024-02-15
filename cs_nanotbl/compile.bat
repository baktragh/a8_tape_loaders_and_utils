@ECHO OFF
REM Raw binaries 
mads -d:UNDER_ROM=0 -o:nanotbl.bin    nanotbl.asm
mads -d:UNDER_ROM=1 -o:nanotbl_ur.bin nanotbl.asm
mads -d:UNDER_ROM=2 -o:nanotbl_u2.bin nanotbl.asm

REM Wrapped binaries
mads -d:UNDER_ROM=0 -o:nanotbl.xex    wrapper.asm
mads -d:UNDER_ROM=1 -o:nanotbl_ur.xex wrapper.asm
mads -d:UNDER_ROM=2 -o:nanotbl_u2.xex wrapper.asm

REM Delete raw binary files
del nanotbl.bin
del nanotbl_ur.bin
del nanotbl_u2.bin

