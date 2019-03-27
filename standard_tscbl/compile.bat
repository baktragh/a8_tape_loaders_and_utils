@ECHO OFF
c:\appbuild\atasm107\atasm -dLDRTYPE=1 -v -otscbl.xex tscbl.asm
c:\appbuild\atasm107\atasm -dLDRTYPE=0 -v -r -otscbl.bot tscbl.asm
PAUSE