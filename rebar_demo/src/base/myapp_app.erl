%%%-------------------------------------------------------------------
%% @doc myapp public API
%% @end
%%%-------------------------------------------------------------------

-module(myapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(SUP, myapp_sup).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	Routes = [
		 {'_', [
				{"/home", cgi_web_handler, []}
			   ]}
	],
    Dispatch = cowboy_router:compile(Routes),
    {ok, Port} = application:get_env(http_port),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    myapp_sup:start_link(),
    
    start_services().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_services() ->
    {ok, _} = supervisor:start_child(?SUP,
        {svr_hot_agent, {svr_hot_agent, start_link, []},
            permanent, 10000, worker, [svr_hot_agent]}).