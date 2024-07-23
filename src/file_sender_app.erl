%%%-------------------------------------------------------------------
%% @doc file_sender public API
%% @end
%%%-------------------------------------------------------------------

-module(file_sender_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    file_sender_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
