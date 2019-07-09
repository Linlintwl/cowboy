-module(mk_demo_app).
-behaviour(application).

-include("common.hrl").

-export([start/2]).
-export([stop/1]).


start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		%% {URIHost, list({URIPath, Handler, Opts})}
		{'_', [
%%			{"/test.html", cowboy_static, {priv_file, ?APP, "test.html"}},
%%			{"/index.html", cowboy_static, {priv_file, ?APP, "index.html"}},
			
			{"/test", cgi_web_handler, []},
			{"/test_get", get_web_handler, []},
			{"/test_post", post_web_handler, []},
			{"/eventsource", eventsource_handler, []},
			{"/upload", upload_handler, []},
			
			{"/[...]", cowboy_static, {priv_dir, ?APP, "", [
				{mimetypes, cow_mimetypes, all},
				{dir_handler, directory_h}
			]}}
		]}
	]),
	{ok, Port} = application:get_env(http_port),
	{ok, _} = cowboy:start_clear(my_http_listener,
		[{port, Port}],
		#{
			env => #{dispatch => Dispatch},
%%			stream_handlers => [cowboy_compress_h, cowboy_stream_h]
			middlewares => [cowboy_router, directory_lister, cowboy_handler]
		}
	),
	io:format("~p start....~n", [?MODULE]),
	mk_demo_sup:start_link(),
	
	start_services().

stop(_State) ->
	ok.


start_services() ->
	{ok, _} = supervisor:start_child(?SUP,
		{svr_hot_agent, {svr_hot_agent, start_link, []},
			permanent, 10000, worker, [svr_hot_agent]}).