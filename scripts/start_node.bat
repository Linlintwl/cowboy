@ECHO off

:: ��������
SET SERVER_ID=%1
SET NODE_ID=%2
SET IP=192.168.1.20
SET NODE_BASE=cowboy
SET COOKIE=cowboy
SET CONFIG=sys
SET HIDDEN=-hidden

:: ��̬����
IF %SERVER_ID% EQU 0 (
    SET CONFIG=%CONFIG%_kf
) ELSE (
    SET CONFIG=%CONFIG%
)

IF %NODE_ID% EQU 10 (
    SET NODE_NAME=%NODE_BASE%_s%SERVER_ID%@%IP%
	SET NODE_TITLE=s%SERVER_ID%
	SET COOKIE=%COOKIE%_s%SERVER_ID%
    SET SMP=1
) ELSE (
	SET NODE_NAME=%NODE_BASE%_k%NODE_ID%@%IP%
	SET NODE_TITLE=k%NODE_ID%
	SET COOKIE=%COOKIE%_kf
    SET SMP=1
)

:: ���ñ���
TITLE %NODE_TITLE%

:: �����ڵ�
CD ../
ECHO �����ڵ� - %NODE_ID%
erl +P 1024000 -name %NODE_NAME% -setcookie %COOKIE% %HIDDEN% -config config\%CONFIG% -smp +S %SMP% -pa ebin -s game start -extra %NODE_ID%
PAUSE