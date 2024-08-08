@ECHO OFF
SET PATH=%PATH%;C:\utils\a8\xasm
xasm  -l -o multipart_normal.bin multipart_normal.asm
xasm  -o multipart_normal.xex multipart_normal_wrapper.asm
del multipart_normal.bin
pause



