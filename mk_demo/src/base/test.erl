%%%-------------------------------------------------------------------
%%% @author Tom
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 七月 2019 11:48
%%%-------------------------------------------------------------------
-module(test).
-author("Tom").

%% API
-export([
    echo/1
]).


echo(Args) ->
    io:format("test echo, args=~p ~n", [Args]),
    Args.
