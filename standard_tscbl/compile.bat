@ECHO OFF
C:\utils\a8\atasm\atasm -dLDRTYPE=1 -s -v -otscbl.xex tscbl.asm 
C:\utils\a8\atasm\atasm -dLDRTYPE=0 -s -v -r -otscbl.bot tscbl.asm
PAUSE