%%%-------------------------------------------------------------------
%%% @doc Exchange worker
%%%-------------------------------------------------------------------
-module(exchanger).

-behaviour(gen_server).

-export([start_link/0, send_file/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% API
%%% ==================================================================

send_file(File) ->
  io:format("Sending file: ~p~n", [File]),
  gen_server:cast(?MODULE, {send_file, File}),
  ok.


%%%===================================================================
%%% GenServer
%%% ==================================================================

init([]) ->
  {ok, #state{}}.

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

handle_cast({send_file, File}, State = #state{}) ->
  SomeHostInNet = "localhost",
  {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5678, [binary, {packet, 0}]),
  {ok, Bin} = file:read_file(File),
  case gen_tcp:send(Sock, Bin) of
    ok ->
      ok = gen_tcp:close(Sock);
    {error, Reason} ->
      io:format("Send error: ~p~n", [Reason])
  end,
  {noreply, State}.

handle_info(Info, State = #state{}) ->
  io:format("Get info: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.
