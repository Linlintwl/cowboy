%%-------------------------------------------------------
%% @File     : common.hrl
%% @Brief    : 公共定义
%% @Author   : Hofer_lu
%% @Date     : 2017-2-4
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__COMMON_H__).
-define(__COMMON_H__, 0).

%% 数字常量
-define(INT32_MAX, 4294967295).
-define(UINT32_MAX, 8589934591).
-define(UINT64_MAX, 36893488147419103231).
-define(DIFF_SECONDS_1970_1900, 2208988800).
-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(ONE_WEEK_SECONDS, 604800).              % 一周
-define(ONE_DAY_SECONDS, 86400).                % 一天
-define(ONE_HOUR, 3600).                        % 一小时
-define(HALF_HOUR, 1800).                       % 半小时
-define(QUARTER_HOUR, 900).                     % 15分钟
-define(ONE_MINUTE, 60).                        % 一分钟
-define(HALF_MINUTE, 30).                       % 半分钟
-define(FIVE_SECONDS, 5).                       % 5秒钟

%% 三元表达式
-define(iif(A, B, C), (case A of true -> B; false -> C end)).

%% 数据类型转换
-define(l2i(List), list_to_integer(List)).
-define(i2l(Int), integer_to_list(Int)).

-define(l2f(List), list_to_float(List)).
-define(f2l(Float), float_to_list(Float)).

-define(l2a(List), list_to_atom(List)).
-define(a2l(Atom), atom_to_list(Atom)).

-define(l2b(List), list_to_binary(List)).
-define(b2l(Binary), binary_to_list(Binary)).

-define(l2bs(List), list_to_bitstring(List)).
-define(bs2l(BitString), bitstring_to_list(BitString)).

-define(l2p(List), list_to_pid(List)).
-define(p2l(Pid), pid_to_list(Pid)).

-define(l2t(List), erlang:list_to_tuple(List)).
-define(t2l(Tuple), erlang:tuple_to_list(Tuple)).

-define(T2B(Val), type:object_to_binary(Val)).


-endif.
