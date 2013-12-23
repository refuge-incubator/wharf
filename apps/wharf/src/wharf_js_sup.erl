%%% -*- erlang -*-
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%% See the NOTICE for more information.

-module(wharf_js_sup).

-behaviour(supervisor).

-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% init the pool for the sync function
    SyncPoolOptions = application:get_env(wharf, sync_pool_options,
                                          [{size, 5}, {max_overflow, 25}]),
    SyncPool = js_spec(wharf_js_pool, SyncPoolOptions),
    {ok, {{one_for_one, 10, 10}, [SyncPool]}}.


%% internal
js_spec(PoolName, PoolOptions) ->
    PoolArgs = [{name, {local, PoolName}},
                {worker_module, wharf_js_worker}] ++ PoolOptions,
    poolboy:child_spec(PoolName, PoolArgs, [PoolName]).
