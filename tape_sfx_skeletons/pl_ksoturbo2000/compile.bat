@ECHO OFF
SET MAD_A=C:\utils\a8\mads\mads
%MAD_A% -d:SIO=0 -o:ksoturbo2000_sfx_skeleton_js.xex -l:ksoturbo2000_sfx_skeleton_js.lst ksoturbo2000_sfx_skeleton.asm
%MAD_A% -d:SIO=1 -o:ksoturbo2000_sfx_skeleton_sio.xex -l:ksoturbo2000_sfx_skeleton_sio.lst ksoturbo2000_sfx_skeleton.asm
PAUSE
