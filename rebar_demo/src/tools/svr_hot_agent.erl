%%-------------------------------------------------------
%% @File     : svr_hot_agent
%% @Brief    : 热更代理模块
%% @Author   : cablsbs
%% @Date     : 2018-9-21
%%-------------------------------------------------------
-module(svr_hot_agent).

-behaviour(gen_server).


-define(INFO(Format, Args), io:format("INFO:" ++ Format ++ "~n", Args)).
-define(ERROR(Format, Args), io:format("ERROR:" ++ Format ++ "~n", Args)).
-define(CRITICAL(Format, Args), io:format("CRITICAL:" ++ Format ++ "~n", Args)).

-export([
    start_link/0,
    hot/0,
    hot/1,
    hot_mods/1,
    hot_mods/2,
    sync_hot/3
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(hot_agent, {
    hot_list = []       % [{TimeStampMS, Times, Nodes, ChangeMods}]
}).


%% Apis -------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 中心节点逻辑
% 全热更
hot() ->
    FromNode = get_from_node(),
    hot(FromNode).

hot(FromNode) when is_atom(FromNode) ->
    gen_server:cast(?MODULE, {'hot_reload', FromNode}).

% 部分模块全热更
hot_mods(ChangedMods) ->
    FromNode = get_from_node(),
    hot_mods(FromNode, ChangedMods).

hot_mods(FromNode, ChangedMods) ->
    gen_server:cast(?MODULE, {'hot_reload', FromNode, ChangedMods}).


% 被动同步热更(非中心节点逻辑)
sync_hot(CenterHotAgent, TimeStampMS, ChangedMods) ->
    gen_server:cast(?MODULE, {'sync_hot', CenterHotAgent, TimeStampMS, ChangedMods}).


%% Callbacks --------------------------------------------
init([]) ->
    {ok, #hot_agent{}}.

handle_call(_Msg, _From, State) ->
    {reply, reply, State}.

% 中心节点热更
handle_cast({'hot_reload', FromNode}, State) ->
    ?INFO("Recv hot_reload request from Node: ~p, start hot ...", [FromNode]),
    ChangedMods = scan_changed(),
    do_hot(ChangedMods),
    HotRecL = do_sync_reload(FromNode, ChangedMods),
    HotListN = HotRecL ++ State#hot_agent.hot_list,
    StateN = State#hot_agent{hot_list = HotListN},
    {noreply, StateN};

% 中心节点热更指定模块列表
handle_cast({'hot_reload', FromNode, ChangedMods}, State) ->
    ?INFO("Recv hot_reload request from Node: ~p, start hot ...", [FromNode]),
    do_hot(ChangedMods),
    HotRecL = do_sync_reload(FromNode, ChangedMods),
    HotListN = HotRecL ++ State#hot_agent.hot_list,
    StateN = State#hot_agent{hot_list = HotListN},
    {noreply, StateN};

% 非中心节点同步热更
handle_cast({'sync_hot', CenterHotAgent, TimeStampMS, ChangedMods}, State) ->
    do_hot(ChangedMods),
    sync_hot_ack(CenterHotAgent, TimeStampMS),
    {noreply, State};

% 中心节点收到热更成功确认
handle_cast({'sync_hot_ack', TimeStamp, FromNode}, State) ->
    HotListN = case lists:keytake(TimeStamp, 1, State#hot_agent.hot_list) of
        {value, {TimeStamp, Times, Nodes, ChangeMods}, Rest} ->
            NodesN = lists:delete(FromNode, Nodes),
            [{TimeStamp, Times, NodesN, ChangeMods} | Rest];
        _ ->
            ?ERROR("Unexpected sync_hot_ack from Node: ~p TimeStamp: ~p", [FromNode, TimeStamp]),
            State#hot_agent.hot_list
    end,
    StateN = State#hot_agent{hot_list = HotListN},
    {noreply, StateN};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'ensure_hot_updated', TimeStampMS}, State) ->
    HotListN = case lists:keytake(TimeStampMS, 1, State#hot_agent.hot_list) of
        {value, {_, _, Nodes, _} = HotRec, Rest} when Nodes =/= [] ->
            HotRecN = re_sync_reload(HotRec),
            HotRecN ++ Rest;
        {value, HotRec, Rest} when is_tuple(HotRec) ->
            Rest;
        _ ->
            State#hot_agent.hot_list
    end,
    StateN = State#hot_agent{hot_list = HotListN},
    {noreply, StateN};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Privates ---------------------------------------------
sync_hot_ack(CenterHotAgent, TimeStampMS) ->
    gen_server:cast(CenterHotAgent, {'sync_hot_ack', TimeStampMS, node()}).

do_sync_reload(_FromNode, []) ->
    [];
do_sync_reload(FromNode, ChangedMods) ->
    case gen_need_sync_nodes(FromNode) of
        [] ->
            [];
        Nodes ->
            ?INFO("Sync hot to the following nodes: ~p", [Nodes]),
            TimeStampMS = time:unixtime_ms(),
            SyncHotArgs = [self(), TimeStampMS, ChangedMods],
            [rpc:cast(Node, ?MODULE, sync_hot, SyncHotArgs) || Node <- Nodes],
            ensure_hot_updated(TimeStampMS),
            [{TimeStampMS, 1, Nodes, ChangedMods}]
    end.

re_sync_reload({TimeStampMS, Times, Nodes, ChangedMods}) when Times < 3 ->
    SyncHotArgs = [self(), TimeStampMS, ChangedMods],
    [rpc:cast(Node, ?MODULE, sync_hot, SyncHotArgs) || Node <- Nodes],
    ensure_hot_updated(TimeStampMS),
    [{TimeStampMS, Times+1, Nodes, ChangedMods}];
re_sync_reload({TimeStampMS, _, Nodes, ChangedMods}) ->
    ?CRITICAL(
        "!!!=== Hot: ~p on Nodes: ~p Fail at Time: ~p ===!!!",
        [ChangedMods, Nodes, time:unixtime_to_iso_string(TimeStampMS div 1000)]
    ),
    [].

ensure_hot_updated(TimeStamp) ->
    erlang:send_after(10000, self(), {'ensure_hot_updated', TimeStamp}).

% 执行热更
do_hot(ChangedMods) ->
    [c:l(Mod) || Mod <- ChangedMods],
    Fmt = lists:flatten(["~p~n" || _ <- ChangedMods]),
    ?INFO("Successfully hot the following Modules:~n"++Fmt, ChangedMods).

% 扫描改变的beam
scan_changed() ->
    EbinPath = get_ebin_path(),
    AllBeam = all_beam(EbinPath),
    ScanFun = fun(Beam, AccModules) ->
        ModStr = filename:basename(Beam, ".beam"),
        Module = list_to_atom(ModStr),
        Md5 = Module:module_info(md5),
        BeamFileName = EbinPath ++ Beam,
        {ok, {_, Md5N}} = beam_lib:md5(BeamFileName),
        case Md5 =:= Md5N of
            true -> AccModules;
            false -> [Module | AccModules]
        end
    end,
    lists:foldl(ScanFun, [], AllBeam).

% @doc 根据具体环境计算ebin目录
-ifndef(app_ebin).

get_ebin_path() ->
    "ebin/".

-else.

get_ebin_path() ->
    case config:is_release() of
        false -> ?app_ebin;
        true -> "ebin/"
    end.

-endif.

% 获取指定目录下所有模块
all_beam(EbinDir) ->
    filelib:wildcard("*.beam", type:object_to_list(EbinDir)).

% 计算需要同步热更的节点列表
gen_need_sync_nodes(FromNode) ->
    AllNode = nodes(connected),
    lists:delete(FromNode, AllNode).

% 计算热更调用节点(不同步热更此节点)
get_from_node() ->
    [_, HostName] = string:tokens(atom_to_list(node()), "@"),
    list_to_atom(lists:concat(["hot_tl_kf", "@", HostName])).
