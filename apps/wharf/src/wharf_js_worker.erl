%%% -*- erlang -*-
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%% See the NOTICE for more information.

-module(wharf_js_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {ctx,
                sync_fun,
                pool}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(PoolName) ->
    HeapSize = application:get_env(wharf, js_max_vm_mem, 8),
    StackSize = application:get_env(wharf, js_thread_stack, 8),
    {ok, Ctx} = new_context(StackSize, HeapSize),
    lager:info("Spidermonkey VM (thread stack: ~pMB, max heap: ~pMB, "
               "pool: ~p) host starting (~p)", [StackSize, HeapSize,
                                                PoolName, self()]),
    {ok, #state{ctx=Ctx,
                pool=PoolName}}.

handle_call({invoke_anon_js, Source, Args}, _From, #state{ctx=Ctx}=State) ->
    Reply = invoke_anon_js(Ctx, Source, Args),
    {reply, Reply, State};
handle_call({invoke_js, JsFun}, _From, #state{ctx=Ctx}=State) ->
    Reply = invoke_js(Ctx, JsFun),
    {reply, Reply, State};
handle_call({invoke_js, JsFun, Args}, _From, #state{ctx=Ctx}=State) ->
    Reply = invoke_js(Ctx, JsFun, Args),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{ctx=Ctx, pool=Pool}) ->
    js_driver:destroy(Ctx),
    lager:info("Spidermonkey VM (pool: ~p) host stopping (~p)",
               [Pool, self()]),
    ok.


%% @private

invoke_anon_js(Ctx, JS, Args) ->
    case define_anon_js(JS, Args) of
        {ok, JSFun} ->
            invoke_js(Ctx, JSFun);
        Error ->
            Error
    end.

invoke_js(Ctx, JSFun) ->
    case js:eval(Ctx, JSFun) of
        {ok, {struct, R}} ->
            case proplists:get_value(<<"lineno">>, R) of
                undefined ->
                    {ok, to_ejson(R)};
                _ ->
                    {error, to_ejson(R)}
            end;
        R ->
            to_ejson(R)
    end.

invoke_js(Ctx, Js, Args) ->
    try
        case js:call(Ctx, Js, Args) of
            {ok, {struct, R}} ->
                case proplists:get_value(<<"lineno">>, R) of
                    undefined ->
                        {ok, to_ejson(R)};
                    _ ->
                        {error, to_ejson(R)}
                end;
            R ->
                to_ejson(R)
        end
    catch
        exit: {ucs, {bad_utf8_character_code}} ->
            lager:error("Error JSON encoding arguments: ~p", [Args]),
            {error, bad_utf8_character_code};
        exit: {json_encode, _} ->
            {error, bad_json};
        throw:invalid_utf8 ->
            {error, bad_utf8_character_code}
    end.

define_anon_js(Source, Args) ->
    try
        ArgList = build_arg_list(Args, []),
        {ok, iolist_to_binary([Source, <<"(">>, ArgList, <<");">>])}
    catch
        throw:badarg ->
            {error, bad_json}
    end.

new_context(ThreadStack, HeapSize) ->
    InitFun = fun(Ctx) -> init_context(Ctx) end,
    js_driver:new(ThreadStack, HeapSize, InitFun).

init_context(Ctx) ->
    load_underscore(Ctx),
    load_user_builtins(Ctx),
    load_sync_builtins(Ctx).

priv_dir() ->
    %% Hacky workaround to handle running from a standard app directory
    %% and .ez package
    case code:priv_dir(wharf) of
        {error, bad_name} ->
            filename:join([filename:dirname(code:which(?MODULE)), "..",
                           "priv"]);
        Dir ->
            Dir
    end.

load_user_builtins(Ctx) ->
    case application:get_env(wharf, js_source_dir, undefined) of
        undefined ->
            ok;
        Path ->
            Files = filelib:wildcard("*.js", Path),
            lists:foreach(fun(File) ->
                        {ok, Contents} = file:read_file(filename:join([Path,
                                                                       File])),
                        js:define(Ctx, Contents) end, Files)
    end.

load_sync_builtins(Ctx) ->
    {ok, Contents} = file:read_file(filename:join([priv_dir(), "sync.js"])),
    js:define(Ctx, Contents).

load_underscore(Ctx) ->
    {ok, Contents} = file:read_file(filename:join([priv_dir(),
                                                   "underscore.js"])),
    js:define(Ctx, Contents).


build_arg_list([], Accum) ->
    lists:reverse(Accum);
build_arg_list([H|[]], Accum) ->
    build_arg_list([], [jsx:encode(H)|Accum]);
build_arg_list([H|T], Accum) ->
    build_arg_list(T, [[jsx:encode(H), ","]|Accum]).


to_ejson({struct, [{_Key_, _Value} | _Rest]=Proplist}) ->
    Proplist1 = lists:foldl(fun({K, V}, Acc) ->
                Acc ++ [{K, to_ejson(V)}]
        end, [], Proplist),
    {Proplist1};
to_ejson([_Value | _Rest]=Array) ->
    [to_ejson(V) || V <- Array];
to_ejson(Value) ->
    Value.
