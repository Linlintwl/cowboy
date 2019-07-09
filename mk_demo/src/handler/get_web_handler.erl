%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(get_web_handler).

-include("common.hrl").

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	#{id := ID, type := Type} = cowboy_req:match_qs([{id, [], <<"0">>}, {type, [], undefined}], Req0),
	Req = do_test(Method, Type, Req0, {?l2i(binary_to_list(ID))}),
	{ok, Req, Opts}.

do_test(<<"GET">>, undefined, Req, _) ->
	cowboy_req:reply(400, #{}, <<"Missing type parameter.">>, Req);
do_test(<<"GET">>, <<"1">>, Req, Args) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, io_lib:format(<<"Testing type1, args=~p......">>, [Args]), Req);
do_test(<<"GET">>, UnknownType, Req, _) ->
	cowboy_req:reply(400, #{}, io_lib:format(<<"Unknown Type = ~p.">>, [type:bitstring_to_term(UnknownType)]), Req);
do_test(_, _, Req, _) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).
