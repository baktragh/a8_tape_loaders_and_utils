@ECHO OFF
C:\utils\a8\atasm\atasm -dLDRTYPE=1 -s -v -olgv.xex lgv.asm 
C:\utils\a8\atasm\atasm -dLDRTYPE=0 -s -v -r -olgv.bot lgv.asm
PAUSE