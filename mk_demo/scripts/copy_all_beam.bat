@ECHO off

:: 设置标题
TITLE compile_all

CD ..
SET root=%cd%

SET from=%root%\deps\
SET deps=%root%\ebin_deps\

CD %from%
ECHO "开始复制 beam ..."
FOR /D %%i IN (*) DO (
	XCOPY /S/E/Y/Q "%from%%%i\ebin\*.*" "%deps%"
)
ECHO "完成复制 beam !!"
IF EXIST %root%\tmp RD /Q/S %root%\tmp

ECHO "Compile all finished !!!"
ECHO "Window close in 3 seconds"
PING 0.0.0.0  -n 3 > null