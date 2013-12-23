%%% -*- erlang -*-
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%% See the NOTICE for more information.

-module(wharf_server).
-behaviour(gen_server).

-export([attach_db/2,
         detach_db/1,
         all_dbs/0,
         open_db/1]).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("wharf.hrl").

-record(state, {db_dir,
                backoff}).

-define(SERVER, ?MODULE).
-define(DEFAULT_BACKOFF, 3600000).

attach_db(DbName, JsonConfig) ->
    gen_server:call(?MODULE, {attach_db, DbName, JsonConfig}).

detach_db(DbName) ->
    gen_server:call(?MODULE, {detach_db, DbName}).

all_dbs() ->
    gen_server:call(?MODULE, all_dbs).

open_db(DbName) ->
    gen_server:call(?MODULE, {open_db, DbName}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    %% initialse the table keeping db configurations
    ets:new(wharf_dbs, [named_table, set, protected,
                            {keypos, #sdb.name}]),
    %% initialise the state
    DbDir = application:get_env(wharf, db_dir, "databases"),
    BackOff = application:get_env(wharf, backoff, ?DEFAULT_BACKOFF),
    InitState = #state{db_dir=DbDir,
                       backoff=BackOff},

    %% attach dbs already configured
    ok = init_dbs(InitState),
    {ok, InitState}.


handle_call({attach_db, DbName, Config}, _From, State) ->
    Reply = case ets:lookup(wharf_dbs, DbName) of
        [] ->
            do_attach_db(DbName, Config, State);
        _ ->
            {error, already_exists}
    end,

    {reply, Reply, State};

handle_call({detach_db, DbName}, _From, State) ->
    Reply = case ets:lookup(wharf_dbs, DbName) of
        [] ->
            {error, not_found};
        [Db] ->
            do_detach_db(Db)
    end,
    {reply, Reply, State};


handle_call({open_db, DbName}, _From, State) ->
    Reply = case ets:lookup(wharf_dbs, DbName) of
        [] ->
            {error, not_found};
        [#sdb{db=CDB}=Db] when CDB /= nil->
            {ok, Db};
        [Db] ->
            load_db(Db)
    end,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

do_attach_db(DbName, JsonConfig, #state{db_dir=DbDir}) ->
    FName = filename:join([DbDir, binary_to_list(DbName) ++ ".json"]),
    case filelib:is_file(FName) of
        true ->
            {error, already_exists};
        false ->
            case file:write_file(FName, jsx:encode(JsonConfig)) of
                ok ->
                    init_db(DbName, FName, JsonConfig),
                    ok;
                Error ->
                    Error
            end
    end.

init_db(DbName, FName, JsonConfig) ->
    lager:info("attach ~p [~p]~n", [DbName, FName]),

    % parse the config to connect to the repote host
    ConnOptions = parse_db_config(JsonConfig),
    %% initialize the sync function, wrap it with our
    %% own code
    SyncFun = proplists:get_value(<<"sync">>, JsonConfig,
                                  ?DEFAULT_SYNC_FUN),
    WrappedSyncFun = ?SYNC_FUN_WRAP(SyncFun),
    %% get server url
    ServerUrl = proplists:get_value(<<"server_url">>,
                                    JsonConfig,
                                    ?DEFAULT_SERVER_URL),

    %% store the db
    ets:insert(wharf_dbs, #sdb{name=DbName,
                                   path=FName,
                                   server_url=ServerUrl,
                                   conn_options=ConnOptions,
                                   sync_fun=WrappedSyncFun} ).


load_db(#sdb{name=DbName, server_url=ServerUrl,
             conn_options=ConnOptions0}=Db) ->
    HPoolName = http_ref(DbName),
    PoolOptions = application:get_env(wharf, http_pool_options,
                                      [{max_connections, 15}]),
    case hackney_pool:start_pool(HPoolName, PoolOptions) of
        ok ->
            ConnOptions = [{pool, HPoolName} | ConnOptions0],

            Server = couchbeam:server_connection(ServerUrl,
                                                 ConnOptions),
            case couchbeam:db_exists(Server, DbName) of
                true ->
                    {ok, CDB} = couchbeam:open_db(Server, DbName),
                    NDb = Db#sdb{db=CDB},
                    ets:insert(wharf_dbs, NDb),
                    {ok, NDb};
                false ->
                    hackney_pool:stop_pool(HPoolName),
                    {error, remote_not_found}
            end;
        Error ->
            Error
    end.

do_detach_db(#sdb{name=DbName, path=Path, db=nil}) ->
    ets:delete(wharf_dbs, DbName),
    file:delete(Path),
    ok;
do_detach_db(#sdb{name=DbName, path=Path}) ->
    ets:delete(wharf_dbs, DbName),
    file:delete(Path),
    catch hackney_pool:stop_pool(http_ref(DbName)),
    ok.

init_dbs(#state{db_dir=DbDir}) ->
    ok = filelib:ensure_dir(filename:join(DbDir, "tmp")),
    lists:foreach(fun(FName) ->
                DbName = list_to_binary(filename:basename(FName, ".json")),
                {ok, Bin} = file:read_file(FName),
                JsonConfig = jsx:decode(Bin),
                init_db(DbName, FName, JsonConfig)
        end, filelib:wildcard(filename:join(DbDir, "*.json"))).


%% utils functions

http_ref(DbName) ->
    binary_to_atom(<<"wharf_db_", DbName/binary >>, utf8).

parse_db_config(Props) ->
    parse_db_config(Props, []).

parse_db_config([], Acc) ->
    Acc;
parse_db_config([{<<"auth">>, <<"basic">>} | Rest], Acc) ->
    UserName = proplists:get_value(<<"username">>, Rest),
    Password = proplists:get_value(<<"password">>, Rest),
    parse_db_config(Rest, [{basic_auth, UserName, Password} | Acc]);
parse_db_config([{<<"proxy">>, <<"http">>} | Rest], Acc) ->
    %% http proxy
    ProxyUrl = proplists:get_value(<<"proxy_url">>, Rest),
    parse_db_config(Rest, [{proxy, ProxyUrl} | Acc]);
parse_db_config([{<<"proxy">>, <<"socks5">>} | Rest], Acc) ->
    UserName = proplists:get_value(<<"socks5_username">>, Rest),
    Password = proplists:get_value(<<"socks5_password">>, Rest),
    parse_db_config(Rest, [{proxy, socks5},
                           {socks5_username, UserName},
                           {socks5_password, Password} | Acc]);
parse_db_config([{_,_}|Rest], Acc) ->
    parse_db_config(Rest, Acc).
