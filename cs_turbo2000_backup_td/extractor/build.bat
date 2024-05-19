@ECHO OFF
SET PATH=%PATH%;C:\utils\a8\cc65\bin

cl65 -t atari --start-addr 8192 -o btde.xex btde.c
PAUSE
