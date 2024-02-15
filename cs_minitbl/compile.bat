@ECHO OFF
REM Create raw binary
mads -o:minitbl.bin -l minitbl.asm

REM Wrapped binary
mads -o:minitbl.xex wrapper.asm

REM Delete raw binaries
REM del minitbl.bin

