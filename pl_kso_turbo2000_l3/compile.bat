@ECHO OFF
SET PATH=C:\utils\a8\atasm;%PATH% 
ECHO %PATH%
Rem Displays messages or turns on or off the display of commands in a batch file.
atasm -s -v -r -ol3_loader.bin l3_loader.asm
atasm -ol3_loader.xex wrapper.asm
PAUSE


