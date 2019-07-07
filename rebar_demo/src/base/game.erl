-module(game).

%% Application callbacks
-export([
    start/0,
    stop/0,
    stop_by_sync/0,
    preload_codes/0
]).

-define(APP, myapp).
-define(APPS, [kernel, stdlib, cowboy, ?APP]). %%  crypto, asn1, public_key, ssl, ranch, cowlib,

%% @doc 启动游戏
start() ->
    preload_codes(),
    start_apps(?APPS),
    ok.

start_apps([]) ->
    ok;
start_apps([App | T]) ->
    application:ensure_all_started(App),
    start_apps(T).

%% @doc 结束游戏(linux异步)
stop() ->
    spawn(fun() -> stop_apps(lists:reverse(?APPS)) end),
    stopping.

%% @doc 结束游戏(windows同步)
stop_by_sync() ->
    stop_apps(lists:reverse(?APPS)),
    stopped.

%% @doc 结束App
stop_apps(Apps) ->
    [stop_app(App) || App <- Apps].

stop_app(App) ->
    application:stop(App).

%% @doc preload
preload_codes() ->
    Paths = code:get_path(),
    RootDir = code:root_dir(),
    ScanPaths = [Path || Path <- Paths, string:prefix(filename:absname(Path), RootDir) =:= nomatch],
    [preload_dir(Path) || Path <- ScanPaths].

preload_dir(Path) ->
    FileNames = filelib:wildcard("*.beam", Path),
    [code:load_file(list_to_atom(filename:rootname(FileName))) || FileName <- FileNames].
