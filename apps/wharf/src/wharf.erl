-module(wharf).

-export([start/0]).
-export([stop/0]).

%% @doc Start the application and all its dependencies.
-spec start() -> ok.
start() ->
    reltool_util:application_start(wharf).

%% @doc Stop the application and all its dependencies.
-spec stop() -> ok.
stop() ->
    reltool_util:application_stop(wharf).
