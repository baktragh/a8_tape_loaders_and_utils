@ECHO OFF
c:\appbuild\atasm107\atasm -dLDRTYPE=1 -s -v -otscbl.xex tscbl.asm 
c:\appbuild\atasm107\atasm -dLDRTYPE=0 -s -v -r -otscbl.bot tscbl.asm
PAUSE