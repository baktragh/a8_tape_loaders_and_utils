@ECHO OFF
SET PATH=%PATH%;C:\utils\a8\xasm
xasm  -l -o multipart_lp.bin multipart_lp.asm
xasm  -o multipart_lp.xex multipart_lp_wrapper.asm
del multipart_lp.bin
pause



