
:: 编译并热更修改的模块
@ECHO off
TITLE hot_game

:: 设置变量
SET COOKIE=cowboy
SET NODE_PREFIX=cowboy
SET IP=192.168.1.20

CD ..
ECHO "Compiling game ... "
erl -noshell -pa ebin ebin_deps -eval "case make:files([\"src/tools/mmake.erl\"], [{out_dir, \"ebin/\"}]) of error -> halt(1); _ -> ok end" -eval "case mmake:all(4) of up_to_date -> halt(0); error -> halt(1) end."
ECHO


:: 没开跨服中心才需要执行以下rpc, 开了跨服中心不需要
ECHO "Hot update server nodes ... "
FOR %%i IN (1) DO erl -noshell -pa ebin -name vt_hot%%i@%IP% -setcookie %COOKIE%_s%%i -hidden -eval "case net_adm:ping('%NODE_PREFIX%_s%%i@%IP%') of pong -> rpc:cast('%NODE_PREFIX%_s%%i@%IP%', svr_hot_agent, hot, []); _ -> io:format(\"No nodes!!!\") end, halt(1)"

::ECHO "Hot update server nodes ... "
::FOR %%i IN (5) DO erl -noshell -pa ebin -name vt_hot%%i@%IP% -setcookie %COOKIE%_s%%i -hidden -eval "case net_adm:ping('%NODE_PREFIX%_s%%i@%IP%') of pong -> rpc:cast('%NODE_PREFIX%_s%%i@%IP%', svr_hot_agent, hot, []); _ -> io:format(\"No nodes!!!\") end, halt(1)"

:: ECHO "Hot update kf nodes ... "
:: erl -noshell -pa ebin -name vt_hot_kf@%IP% -setcookie %COOKIE%_kf -hidden -eval "case net_adm:ping('%NODE_PREFIX%_k100@%IP%') of pong -> rpc:cast('%NODE_PREFIX%_k100@%IP%', svr_hot_agent, hot, []); _ -> io:format(\"No nodes!!!\") end, halt(1)"


ECHO "Hot game finished !!!"
ECHO "Window close in 5 seconds"
PING 0.0.0.0  -n 5 > null
DEL /F null