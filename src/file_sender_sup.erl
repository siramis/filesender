%%%-------------------------------------------------------------------
%% @doc file_sender top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(file_sender_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags = #{strategy => one_for_all,
    intensity => 0,
    period => 1},
  FilerArgs = [{local, filer}, filer, [], []],
  ExchangerArgs = [{local, exchanger}, exchanger, [], []],
  ChildSpecs = [
    #{id => filer, start => {gen_server, start_link, FilerArgs}},
    #{id => exchanger, start => {gen_server, start_link, ExchangerArgs}}
  ],
  {ok, {SupFlags, ChildSpecs}}.
